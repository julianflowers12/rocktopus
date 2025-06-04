#' Get Tariff Details
#' This function retrieves the tariff history for a specific property from the output of the get_property_details function.
#' @param out The output from the get_property_details function.
#' @param property_index The index of the property in the output list (default is 1).
#' @return A data frame containing the tariff history, including valid dates and duration.
#' @examples
#' out <- get_meter_details("your_account_number", "your_api_key")
#' get_tariff_history(out, property_index = 1)
#' @export




get_tariff_history <- function(out, property_index = 1){

    elec_tariff <- out$properties[[property_index]]$electricity_meter_points |>
        map(c("agreements")) |>
        enframe() |>
        unnest(value) |>
        unnest_auto(value) |>
        mutate(valid = ymd(str_sub(valid_from, 1, 10)),
               valid_to = ymd(str_sub(valid_to, 1, 10)),
               end = as.Date(ifelse(is.na(valid_to), Sys.Date(), valid_to)),
               duration = end - valid
        )

    return(elec_tariff)
}
