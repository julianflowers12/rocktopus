#' Process EV data
#' @param df Data frame containing EV data
#' @return A data frame with processed EV data
#' @export
#' @examples
#' ev_data <- read.csv("ev_data.csv")
#' evd_process(ev_data)




evd_process <- function(df){


    home <- df |>
        dplyr::filter(Location == "home") |>
        select(4:7)

    home_peak <- home |>
        mutate(start = ymd(str_sub(`Session Start Time`, 1, 10)),
               end = ymd(str_sub(`Session Finish Time`, 1, 10)),
               start_h = hm(str_sub(`Session Start Time`, 12, 16)),
               peak = ifelse(end > start, "off_peak", "peak")
        ) |>
        select(end, `Estimated Energy Delivered (kWh)`, peak)
}
