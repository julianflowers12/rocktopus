# app.R — Octopus Electricity + Gas — GMT (manual tariffs)
# Adds client-side "Remember API key on this browser" using localStorage

library(shiny)
library(dplyr)
library(tibble)
library(lubridate)
library(httr)
library(jsonlite)
library(plotly)
library(DT)

# --------- helpers ----------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
row_to_list <- function(df, i = 1L) {
    as.list(lapply(df[i, , drop = FALSE], function(col) if (is.list(col)) col[[1]] else col[[1]]))
}
first_of <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) return(row_to_list(x, 1L))
    if (is.list(x)) return(x[[1]])
    x[1]
}

# Time-of-day filter in GMT (decimal hours, handles overnight)
filter_by_tod_gmt <- function(df, start_hour, end_hour, time_col = "interval_start_gmt") {
    if (!nrow(df)) return(df)
    ts <- df[[time_col]]
    pt <- as.POSIXlt(ts, tz = "GMT")
    hour_dec <- pt$hour + pt$min / 60
    h0 <- as.numeric(start_hour); h1 <- as.numeric(end_hour)
    keep <- if (h0 == 0 && h1 == 24) rep(TRUE, length(hour_dec))
    else if (h0 < h1) (hour_dec >= h0 & hour_dec < h1)
    else (hour_dec >= h0 | hour_dec < h1)  # overnight wrap
    df[keep, , drop = FALSE]
}

# --------- HTTP ---------
octo_fetch_json <- function(url, api_key) {
    auth <- authenticate(user = api_key, password = "")
    resp <- tryCatch(RETRY("GET", url, auth, user_agent("octo-gmt/3.1"), quiet = TRUE, times = 3),
                     error = function(e) NULL)
    if (is.null(resp)) return(list(ok=FALSE, url=url, body="<network error>", json=NULL, err="Network error"))
    if (http_error(resp)) {
        txt <- tryCatch(content(resp, as="text", encoding="UTF-8"), error=function(e) "")
        return(list(ok=FALSE, url=url, body=txt, json=NULL, err=sprintf("HTTP %s", status_code(resp))))
    }
    txt <- tryCatch(content(resp, as="text", encoding="UTF-8"), error=function(e) "")
    if (!is.character(txt) || identical(txt, "")) return(list(ok=FALSE, url=url, body="<empty body>", json=NULL, err="Empty body"))
    js <- tryCatch(fromJSON(txt, simplifyVector=TRUE), error=function(e) NULL)
    if (is.null(js)) return(list(ok=FALSE, url=url, body=substr(txt,1,500), json=NULL, err="Invalid JSON"))
    list(ok=TRUE, url=url, json=js, err=NULL)
}

# --------- Account + meter points ---------
get_meter_details <- function(api_key, acct) {
    res <- octo_fetch_json(paste0("https://api.octopus.energy/v1/accounts/", acct, "/"), api_key)
    if (!res$ok) { empty <- tibble(); attr(empty,"error") <- res$err; return(empty) }
    res$json
}

# Electricity MPANs (2 rows: import/export)
get_electric_mpan <- function(meter_details, property_index) {
    if (is.data.frame(meter_details) && ncol(meter_details)==0)
        return(tibble(property_index=property_index, flow=c("import","export"), mpan=NA, serial=NA))
    props <- meter_details$properties
    prop_i <- if (is.data.frame(props)) row_to_list(props, property_index) else props[[property_index]]
    if (is.null(prop_i)) return(tibble(property_index=property_index, flow=c("import","export"), mpan=NA, serial=NA))
    emp_any <- prop_i$electricity_meter_points
    if (is.null(emp_any)) return(tibble(property_index=property_index, flow=c("import","export"), mpan=NA, serial=NA))

    emps <- if (is.data.frame(emp_any)) lapply(seq_len(nrow(emp_any)), function(i) row_to_list(emp_any,i)) else emp_any
    cand <- lapply(emps, function(e){
        mpan <- e$mpan %||% e$mpan_core %||% NA
        m1 <- first_of(e$meters)
        serial <- m1$serial_number %||% m1$serial %||% NA
        is_exp <- isTRUE(e$is_export) || isTRUE(m1$is_export)
        list(mpan=as.character(mpan), serial=as.character(serial),
             is_export=is_exp, mpan_export_field=e$mpan_export %||% NA)
    })
    cdf <- dplyr::bind_rows(cand)

    serial_any <- cdf$serial[which(!is.na(cdf$serial)&nzchar(cdf$serial))][1] %||% NA
    explicit_export <- cdf$mpan_export_field[which(!is.na(cdf$mpan_export_field)&nzchar(cdf$mpan_export_field))][1] %||% NA

    mpan_import <- NA; mpan_export <- NA
    if (!is.na(explicit_export)) {
        mpan_export <- explicit_export
        mpan_import <- (cdf$mpan[which(cdf$mpan!=mpan_export)][1]) %||% NA
    } else if (any(cdf$is_export, na.rm=TRUE)) {
        mpan_export <- cdf$mpan[which(cdf$is_export)][1]
        mpan_import <- cdf$mpan[which(!cdf$is_export)][1]
    } else {
        uniq <- unique(cdf$mpan[!is.na(cdf$mpan)&nzchar(cdf$mpan)])
        if (length(uniq)>=2) { mpan_import<-uniq[1]; mpan_export<-uniq[2] }
        else if (length(uniq)==1) { mpan_import<-uniq[1]; mpan_export<-NA }
    }
    tibble(property_index=property_index, flow=c("import","export"),
           mpan=c(mpan_import,mpan_export), serial=c(serial_any,serial_any))
}

# Gas MPRN
get_gas_mprn <- function(meter_details, property_index) {
    if (is.data.frame(meter_details) && ncol(meter_details)==0)
        return(tibble(property_index=property_index, mprn=NA, serial=NA))
    props <- meter_details$properties
    prop_i <- if (is.data.frame(props)) row_to_list(props, property_index) else props[[property_index]]
    if (is.null(prop_i)) return(tibble(property_index=property_index, mprn=NA, serial=NA))
    gmp_any <- prop_i$gas_meter_points
    if (is.null(gmp_any)) return(tibble(property_index=property_index, mprn=NA, serial=NA))
    gmps <- if (is.data.frame(gmp_any)) lapply(seq_len(nrow(gmp_any)), function(i) row_to_list(gmp_any, i)) else gmp_any
    g1 <- first_of(gmps)
    if (is.null(g1)) return(tibble(property_index=property_index, mprn=NA, serial=NA))
    mprn <- g1$mprn %||% g1$mprn_core %||% NA
    meters <- g1$meters; m1 <- first_of(meters)
    serial <- m1$serial_number %||% m1$serial %||% NA
    tibble(property_index = property_index, mprn = as.character(mprn %||% NA), serial = as.character(serial %||% NA))
}

# --------- Consumption fetchers (timestamps parsed in UTC → shown as GMT) ---------
octo_get_elec <- function(flow = c("import","export"), api_key, acct,
                          property_index = 1L, period_from = NULL, period_to = NULL,
                          page_size = 25000) {
    flow <- match.arg(flow)
    md <- get_meter_details(api_key, acct)
    if (is.data.frame(md) && ncol(md) == 0) { out <- tibble(); attr(out, "error") <- attr(md, "error"); return(out) }
    kv  <- get_electric_mpan(md, property_index)
    row <- kv[kv$flow == flow, , drop = FALSE]
    mpan <- row$mpan[1];  serial <- row$serial[1]
    if (is.na(mpan) || is.na(serial) || !nzchar(mpan) || !nzchar(serial)) { out <- tibble(); attr(out,"error") <- sprintf("Missing MPAN/serial for %s", flow); return(out) }

    base <- "https://api.octopus.energy/v1"
    url  <- sprintf("%s/electricity-meter-points/%s/meters/%s/consumption/", base, mpan, serial)

    as_iso <- function(x) if (is.null(x)) NULL else if (inherits(x, "POSIXt")) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else as.character(x)
    q <- list(period_from = as_iso(period_from), period_to = as_iso(period_to), page_size = page_size, order_by = "period")
    q <- q[!vapply(q, is.null, logical(1))]
    next_url <- httr::modify_url(url, query = q)

    results <- list()
    repeat {
        res <- octo_fetch_json(next_url, api_key)
        if (!res$ok) { out <- tibble(); attr(out,"error") <- sprintf("%s\nURL: %s\nBody: %s", res$err, res$url, res$body %||% ""); return(out) }
        dat <- res$json
        if (!is.null(dat$results) && length(dat$results)) results[[length(results)+1L]] <- tibble::as_tibble(dat$results)
        if (is.null(dat$`next`) || is.na(dat$`next`) || dat$`next` == "") break
        next_url <- dat$`next`
    }
    if (!length(results)) return(tibble())
    raw <- dplyr::bind_rows(results)
    start_utc <- suppressWarnings(ymd_hms(raw$interval_start, tz = "UTC"))
    end_utc   <- suppressWarnings(ymd_hms(raw$interval_end,   tz = "UTC"))
    if (all(is.na(start_utc))) { out <- tibble(); attr(out,"error") <- "Failed to parse electricity timestamps."; return(out) }

    raw %>%
        dplyr::mutate(
            commodity          = "electricity",
            flow               = flow,
            mpan               = mpan,
            serial             = serial,
            interval_start_gmt = with_tz(start_utc, "GMT"),
            interval_end_gmt   = with_tz(end_utc,   "GMT"),
            kwh                = .data[["consumption"]]
        ) %>%
        dplyr::select(commodity, flow, mpan, serial, interval_start_gmt, interval_end_gmt, kwh, dplyr::everything())
}

octo_get_gas <- function(api_key, acct, property_index = 1L,
                         period_from = NULL, period_to = NULL, page_size = 25000) {
    md <- get_meter_details(api_key, acct)
    if (is.data.frame(md) && ncol(md) == 0) { out <- tibble(); attr(out, "error") <- attr(md, "error"); return(out) }
    gk <- get_gas_mprn(md, property_index)
    mprn <- gk$mprn[1];  serial <- gk$serial[1]
    if (is.na(mprn) || is.na(serial) || !nzchar(mprn) || !nzchar(serial)) { out <- tibble(); attr(out,"error") <- "Missing MPRN/serial"; return(out) }

    base <- "https://api.octopus.energy/v1"
    url  <- sprintf("%s/gas-meter-points/%s/meters/%s/consumption/", base, mprn, serial)

    as_iso <- function(x) if (is.null(x)) NULL else if (inherits(x, "POSIXt")) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else as.character(x)
    q <- list(period_from = as_iso(period_from), period_to = as_iso(period_to), page_size = page_size, order_by = "period")
    q <- q[!vapply(q, is.null, logical(1))]
    next_url <- httr::modify_url(url, query = q)

    results <- list()
    repeat {
        res <- octo_fetch_json(next_url, api_key)
        if (!res$ok) { out <- tibble(); attr(out,"error") <- sprintf("%s\nURL: %s\nBody: %s", res$err, res$url, res$body %||% ""); return(out) }
        dat <- res$json
        if (!is.null(dat$results) && length(dat$results)) results[[length(results)+1L]] <- tibble::as_tibble(dat$results)
        if (is.null(dat$`next`) || is.na(dat$`next`) || dat$`next` == "") break
        next_url <- dat$`next`
    }
    if (!length(results)) return(tibble())
    raw <- dplyr::bind_rows(results)
    start_utc <- suppressWarnings(ymd_hms(raw$interval_start, tz = "UTC"))
    end_utc   <- suppressWarnings(ymd_hms(raw$interval_end,   tz = "UTC"))
    if (all(is.na(start_utc))) { out <- tibble(); attr(out,"error") <- "Failed to parse gas timestamps."; return(out) }

    m3 <- as.numeric(raw[["consumption"]])
    raw %>%
        dplyr::mutate(
            commodity          = "gas",
            flow               = "import",
            mprn               = mprn,
            serial             = serial,
            interval_start_gmt = with_tz(start_utc, "GMT"),
            interval_end_gmt   = with_tz(end_utc,   "GMT"),
            m3                 = m3
        ) %>%
        dplyr::select(commodity, flow, mprn, serial, interval_start_gmt, interval_end_gmt, m3, dplyr::everything())
}

# Gas m3 -> kWh
gas_m3_to_kwh <- function(m3, vol_correction = 1.02264, cv_mj_per_m3 = 39.2) {
    as.numeric(m3) * vol_correction * cv_mj_per_m3 * (1/3.6)
}

# --------- UI ---------
ui <- fluidPage(
    titlePanel("Octopus Electricity + Gas — GMT (manual tariffs)"),
    # JS: load from localStorage on connect; handler to store/clear
    tags$script(HTML("
    Shiny.addCustomMessageHandler('storeKey', function(value){
      try { localStorage.setItem('octo_api_key', value || ''); } catch(e){}
    });
    Shiny.addCustomMessageHandler('clearKey', function(){
      try { localStorage.removeItem('octo_api_key'); } catch(e){}
    });
    document.addEventListener('shiny:connected', function(){
      try {
        var k = localStorage.getItem('octo_api_key');
        if (k && k.length) Shiny.setInputValue('api_key', k, {priority:'event'});
      } catch(e){}
    });
  ")),
    sidebarLayout(
        sidebarPanel(
            radioButtons("commodity","Commodity", choices = c("Electricity"="elec","Gas"="gas"), inline = TRUE),

            passwordInput("api_key","API key", value=""),
            fluidRow(
                column(7, checkboxInput("remember_key", "Remember on this browser", value = TRUE)),
                column(5, actionButton("forget_key", "Forget key", class = "btn-outline-danger btn-sm", width = "100%"))
            ),
            helpText("If you tick 'Remember', your API key is stored unencrypted in this browser's localStorage."),

            textInput("acct","Account ID", placeholder="e.g. A-5866AE68"),
            numericInput("property_index","Property index", value=1, min=1, step=1),

            conditionalPanel("input.commodity == 'elec'",
                             radioButtons("flow","Electricity flow", choices=c("Import"="import","Export"="export"), inline=TRUE),
                             # Manual tariff inputs for ELECTRICITY (import/export)
                             conditionalPanel("input.flow == 'import'",
                                              selectInput("elec_structure","Tariff structure",
                                                          choices = c("Single-rate"="single",
                                                                      "Economy 7 (Day/Night)"="e7",
                                                                      "Peak/Off-peak (Go/IOG)"="peak_off"),
                                                          selected = "single"),
                                              numericInput("elec_sc_pday", "Standing charge (p/day)", value = NA, min = 0, step = 0.001),
                                              conditionalPanel("input.elec_structure == 'single'",
                                                               numericInput("elec_unit_p", "Unit rate (p/kWh)", value = NA, min = 0, step = 0.001)
                                              ),
                                              conditionalPanel("input.elec_structure == 'e7'",
                                                               numericInput("elec_day_p",   "Day unit rate (p/kWh)",   value = NA, min = 0, step = 0.001),
                                                               numericInput("elec_night_p", "Night unit rate (p/kWh)", value = NA, min = 0, step = 0.001)
                                              ),
                                              conditionalPanel("input.elec_structure == 'peak_off'",
                                                               numericInput("elec_offpeak_p", "Off-peak unit rate (p/kWh)", value = NA, min = 0, step = 0.001),
                                                               numericInput("elec_peak_p",    "Peak unit rate (p/kWh)",     value = NA, min = 0, step = 0.001)
                                              )
                             ),
                             conditionalPanel("input.flow == 'export'",
                                              numericInput("export_unit_p", "Export unit rate (p/kWh)", value = NA, min = 0, step = 0.001)
                             )
            ),

            # Manual tariff inputs for GAS
            conditionalPanel("input.commodity == 'gas'",
                             radioButtons("gas_unit","Gas units for plot/table", choices = c("kWh","m³"), inline = TRUE, selected = "kWh"),
                             numericInput("gas_cv", "Calorific value (MJ/m³)", value = 39.2, min = 35, max = 42, step = 0.1),
                             numericInput("gas_corr", "Volume correction", value = 1.02264, min = 1.0, max = 1.1, step = 0.0001),
                             tags$hr(),
                             numericInput("gas_sc_pday", "Gas standing charge (p/day)", value = NA, min = 0, step = 0.001),
                             numericInput("gas_unit_p",  "Gas unit rate (p/kWh)",       value = NA, min = 0, step = 0.001)
            ),

            dateRangeInput("dates","Date range (GMT)", start=Sys.Date()-30, end=Sys.Date()),
            sliderInput("hours","Time-of-day filter (GMT)", min=0, max=24, value=c(0,24), step=0.5, post=" h"),

            checkboxInput("smooth", "Smooth with LOESS", value = FALSE),
            sliderInput("smooth_span", "LOESS span", min = 0.1, max = 1.0, value = 0.3, step = 0.05),

            selectInput("agg","Aggregate by", choices=c("Half-hourly"="hh","Day"="day","Week"="week","Month"="month","Year"="year"),
                        selected="day"),
            actionButton("fetch","Fetch data", class="btn-primary")
        ),
        mainPanel(
            # Info box (manual tariff summary)
            tags$div(
                style="margin-bottom:12px;padding:12px;border:1px solid #ddd;border-radius:8px;background:#fafafa;",
                htmlOutput("tariff_box")
            ),
            tabsetPanel(
                tabPanel("Plot", plotlyOutput("plot", height=520)),
                tabPanel("Table", DTOutput("table"))
            )
        )
    )
)

# --------- SERVER ---------
server <- function(input, output, session) {

    # Save/clear API key in browser localStorage
    observeEvent(input$api_key, {
        if (isTRUE(input$remember_key) && nzchar(input$api_key %||% "")) {
            session$sendCustomMessage("storeKey", input$api_key)
        }
    })
    observeEvent(input$remember_key, {
        if (isTRUE(input$remember_key) && nzchar(input$api_key %||% "")) {
            session$sendCustomMessage("storeKey", input$api_key)
        }
    })
    observeEvent(input$forget_key, {
        session$sendCustomMessage("clearKey", NULL)
        updateTextInput(session, "api_key", value = "")
        updateCheckboxInput(session, "remember_key", value = FALSE)
        showNotification("API key removed from this browser.", type = "message", duration = 4)
    })

    # Render manual tariff info box
    output$tariff_box <- renderUI({
        ptxt <- function(x, suffix="") if (is.na(x)) "—" else paste0(format(round(x, 3), nsmall=3), suffix)

        if (input$commodity == "elec") {
            if (input$flow == "export") {
                return(tags$div(
                    tags$b("Electricity (Export) — manual"),
                    tags$br(),
                    tags$span(sprintf("Export unit rate: %s p/kWh", ptxt(input$export_unit_p)))
                ))
            } else {
                if (input$elec_structure == "single") {
                    rows <- list(
                        tags$b("Electricity (Import, single-rate) — manual"), tags$br(),
                        tags$span(sprintf("Standing charge: %s p/day", ptxt(input$elec_sc_pday))), tags$br(),
                        tags$span(sprintf("Unit rate: %s p/kWh", ptxt(input$elec_unit_p)))
                    )
                } else if (input$elec_structure == "e7") {
                    rows <- list(
                        tags$b("Electricity (Import, Economy 7) — manual"), tags$br(),
                        tags$span(sprintf("Standing charge: %s p/day", ptxt(input$elec_sc_pday))), tags$br(),
                        tags$span(sprintf("Day unit rate: %s p/kWh",   ptxt(input$elec_day_p))), tags$br(),
                        tags$span(sprintf("Night unit rate: %s p/kWh", ptxt(input$elec_night_p)))
                    )
                } else {
                    rows <- list(
                        tags$b("Electricity (Import, Peak/Off-peak) — manual"), tags$br(),
                        tags$span(sprintf("Standing charge: %s p/day", ptxt(input$elec_sc_pday))), tags$br(),
                        tags$span(sprintf("Off-peak unit rate: %s p/kWh", ptxt(input$elec_offpeak_p))), tags$br(),
                        tags$span(sprintf("Peak unit rate: %s p/kWh",     ptxt(input$elec_peak_p)))
                    )
                }
                return(do.call(tags$div, rows))
            }
        } else {
            return(tags$div(
                tags$b("Gas — manual"),
                tags$br(),
                tags$span(sprintf("Standing charge: %s p/day", ptxt(input$gas_sc_pday))), tags$br(),
                tags$span(sprintf("Unit rate: %s p/kWh", ptxt(input$gas_unit_p)))
            ))
        }
    })

    # Fetch raw data (no price lookups)
    raw_dat <- eventReactive(input$fetch, {
        req(nzchar(input$api_key %||% ""), nzchar(input$acct %||% ""), input$dates)
        from_utc <- as.POSIXct(paste0(input$dates[1]," 00:00:00"), tz="UTC")
        to_utc   <- as.POSIXct(paste0(input$dates[2]," 23:59:59"), tz="UTC")

        if (input$commodity == "elec") {
            df <- octo_get_elec(input$flow, input$api_key, input$acct, input$property_index,
                                period_from=from_utc, period_to=to_utc)
        } else {
            df <- octo_get_gas(input$api_key, input$acct, input$property_index,
                               period_from=from_utc, period_to=to_utc)
            if (input$gas_unit == "kWh") {
                df <- df %>% mutate(kwh = gas_m3_to_kwh(m3, vol_correction = input$gas_corr, cv_mj_per_m3 = input$gas_cv))
            } else {
                df <- df %>% rename(kwh = m3)
            }
        }

        err <- attr(df,"error",exact=TRUE)
        if (!is.null(err)) { showNotification(err, type="error", duration=10); return(tibble()) }
        if (!nrow(df)) { showNotification("No data for selected date range.", type="message", duration=6); return(tibble()) }
        df
    }, ignoreInit=TRUE)

    # Time filter → Aggregate
    filtered_dat <- reactive({
        df <- raw_dat()
        if (!nrow(df)) return(df)
        filter_by_tod_gmt(df, input$hours[1], input$hours[2], time_col = "interval_start_gmt")
    })

    agg_dat <- reactive({
        df <- filtered_dat()
        if (!nrow(df)) return(tibble(interval=as.POSIXct(character()), kwh=numeric()))
        switch(input$agg,
               "hh"   = df %>% transmute(interval = interval_start_gmt, kwh),
               "day"  = df %>% mutate(day = as.Date(interval_start_gmt)) %>% group_by(day)  %>% summarise(kwh = sum(kwh, na.rm = TRUE), .groups="drop") %>% rename(interval = day),
               "week" = df %>% mutate(week = floor_date(interval_start_gmt, "week", week_start = 1)) %>% group_by(week) %>% summarise(kwh = sum(kwh, na.rm = TRUE), .groups="drop") %>% rename(interval = week),
               "month"= df %>% mutate(month= floor_date(interval_start_gmt, "month")) %>% group_by(month)%>% summarise(kwh = sum(kwh, na.rm = TRUE), .groups="drop") %>% rename(interval = month),
               "year" = df %>% mutate(year = floor_date(interval_start_gmt, "year")) %>% group_by(year) %>% summarise(kwh = sum(kwh, na.rm = TRUE), .groups="drop") %>% rename(interval = year)
        )
    })

    # Plot (+ optional LOESS)
    output$plot <- renderPlotly({
        df <- agg_dat()
        if (!nrow(df)) return(plotly::plot_ly(type="scatter", mode="markers") %>% layout(title="No data to plot — adjust filters."))

        ylab_unit <- if (input$commodity == "gas" && input$gas_unit == "m³") "m³" else "kWh"

        plt <- plot_ly(df, x=~interval, y=~kwh, type="scatter", mode="lines+markers", name=paste("Raw", input$agg))

        if (isTRUE(input$smooth) && nrow(df) >= 10) {
            xnum <- as.numeric(df$interval)
            span <- max(0.1, min(1.0, as.numeric(input$smooth_span %||% 0.3)))
            lo <- try(stats::loess(kwh ~ xnum, data = df, span = span, degree = 2,
                                   family = "gaussian", surface = "direct",
                                   control = loess.control(surface = "direct")), silent = TRUE)
            if (!inherits(lo, "try-error")) {
                pred <- try(predict(lo, newdata = data.frame(xnum = xnum)), silent = TRUE)
                if (!inherits(pred, "try-error")) {
                    sm_df <- df; sm_df$kwh_s <- as.numeric(pred)
                    plt <- plt %>% add_trace(x = sm_df$interval, y = sm_df$kwh_s,
                                             type = "scatter", mode = "lines",
                                             name = paste0("LOESS (span=", format(round(span,2), nsmall = 2), ")"))
                }
            }
        }

        plt %>% layout(xaxis=list(title="Interval (GMT)"),
                       yaxis=list(title=paste0(ylab_unit," (",input$agg,")")))
    })

    # Table
    output$table <- renderDT({
        df <- agg_dat()
        if (!nrow(df)) return(DT::datatable(tibble()))
        unit_lab <- if (input$commodity == "gas" && input$gas_unit == "m³") "m³" else "kWh"
        df_out <- df %>% rename(!!unit_lab := kwh)
        DT::datatable(df_out, options=list(pageLength=25), rownames=FALSE)
    })
}

shinyApp(ui, server)
