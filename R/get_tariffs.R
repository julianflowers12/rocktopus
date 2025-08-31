#' Get Tariff Information
#' This function retrieves tariff information associated with an Octopus Energy account.
#' @param acct A string representing the account number.
#' @param api_key A string representing the API key for authentication.
#' @param property_index An integer representing the index of the property to retrieve tariff information for (default is 1).
#' @return A list containing standing charges and unit rates for the current property consumption tariff.
#' @examples
#' get_tariff_info("your_account_number", "your_api_key", property_index = 1)
#' @export
#' @import httr2 dplyr lubridate stringr purrr tibble tidyr



get_tariff_info <- function(api_key, acct, property_index){


    require(httr2)
    require(dplyr)
    require(tibble)
    require(tidyr)
    require(purrr)
    require(lubridate)
    require(stringr)

    #acct <- "A-5866AE68"
    urlx = paste0("https://api.octopus.energy/v1/accounts/", acct, "/")
    property_index <- as.integer(property_index)


    response <- request(urlx) |>
        req_auth_basic(username = api_key, password = "") |> req_perform()

    out <- response |> resp_body_json()

## import tariff

    import_tariff <-
        out$properties[[property_index]]$electricity_meter_points[1] |>
        map(c("agreements")) |>
        enframe() |>
        unnest(value) |>
        unnest_auto(value) |>
        mutate(valid = ymd(str_sub(valid_from, 1, 10)),
               valid_to = ymd(str_sub(valid_to, 1, 10)),
               end = as.Date(ifelse(is.na(valid_to), Sys.Date(), valid_to)),
               duration = end - valid
        ) |>
        arrange(desc(valid))

# charges
    current_code <- elec_tariff[is.na(elec_tariff$valid_to), "tariff_code"]
    trunc_code <- current_code |>
        str_remove("E-1R-") |>
        str_remove("-[AZ]$")
    urlsc <- paste0("https://api.octopus.energy/v1/products/", trunc_code, "/electricity-tariffs/", current_code, "/standing-charges/")
    urlsur <- paste0("https://api.octopus.energy/v1/products/", trunc_code, "/electricity-tariffs/", current_code, "/standard-unit-rates/")

    sc <- request(urlsc) |> req_perform() |>
        resp_body_json() |>
        enframe() |>
        filter(name == "results") |>
        unnest(value) |>
        unnest_auto(value)

    ur <- request(urlsur) |> req_perform() |>
        resp_body_json() |>
        enframe() |>
        filter(name == "results") |>
        unnest(value) |>
        unnest_auto(value)

## export tariff
##

export_tariff <- out$properties[[property_index]]$electricity_meter_points[2] |>
    map(c("agreements")) |>
    enframe() |>
    unnest(value) |>
    unnest_auto(value) |>
    mutate(valid = ymd(str_sub(valid_from, 1, 10)),
           valid_to = ymd(str_sub(valid_to, 1, 10)),
           end = as.Date(ifelse(is.na(valid_to), Sys.Date(), valid_to)),
           duration = end - valid
    ) |>
    arrange(desc(valid))

    export_code <- export_tariff[export_tariff$valid_to > lubridate::today(), "tariff_code"]
    trunc_exp <- export_code |>
        str_remove("E-1R-") |>
        str_remove("-[AZ]$")

    urlexp <- paste0("https://api.octopus.energy/v1/products/", trunc_exp, "/electricity-tariffs/", export_code, "/standard-unit-rates/")
    exp_charge <- request(urlexp) |> req_perform() |>
        resp_body_json() |>
        enframe() |>
        filter(name == "results") |>
        unnest(value) |>
        unnest_auto(value)
    #     arrange(tariff_code, valid_from)

## gas charges
##
##

 get_gas_tariff <- function(property_index){

      res <- out$properties[[property_index]]$gas_meter_points
      if (length(res) == 0L) {
          return(NULL) } else {

      df <- bind_rows(res, .id = "source")

    gas_tariff <- df |>
      unnest_auto("agreements") |>
      mutate(valid = ymd(str_sub(valid_from, 1, 10)),
             valid_to = ifelse(!is.na(valid_to), ymd(str_sub(valid_to, 1, 10)), NA),
             end = as.Date(ifelse(is.na(valid_to), Sys.Date(), valid_to)),
             duration = end - valid
      ) |>
      arrange(desc(valid))


    gas_code <- gas_tariff[is.na(gas_tariff$valid_to), "tariff_code"]
    trunc_gas <- gas_code |>
        str_remove("G-1R-") |>
        str_remove("-[AZ]$")
    urlsgas <- paste0("https://api.octopus.energy/v1/products/", trunc_gas, "/gas-tariffs/", gas_code, "/standing-charges/")
    urlugasur <- paste0("https://api.octopus.energy/v1/products/", trunc_gas, "/gas-tariffs/", gas_code, "/standard-unit-rates/")
    scg <- request(urlsgas) |> req_perform() |>
        resp_body_json() |>
        enframe() |>
        filter(name == "results") |>
        unnest(value) |>
        unnest_auto(value)

    urg <- request(urlugasur) |> req_perform() |>
        resp_body_json() |>
        enframe() |>
        filter(name == "results") |>
        unnest(value) |>
        unnest_auto(value)

    return(list(gas_standing_charges = scg, gas_unit_rates = urg)
    )
      }
    }

    gas_info <- get_gas_tariff(property_index)
    if (is.null(gas_info)) {
        scg <- tibble()
        urg <- tibble()
    } else {
        scg <- gas_info$gas_standing_charges
        urg <- gas_info$gas_unit_rates
    }

result <- list(standing_charges = sc, unit_rates = ur, export_rates = exp_charge, gas_standing_charges = scg, gas_unit_rates = urg)

    return(result)

}

