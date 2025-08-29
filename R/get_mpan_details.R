#' Get mpan details
#' Get MPAN details from the output of the get_property_details function
#' @param out The output from the get_property_details function.
#' @param property_index The index of the property in the output list (default is 1).
#' @return A list containing the MPAN, export MPAN, meter serial number, and export serial number.
#' @export
#' @examples
#' out <- get_property_details("your_account_number", "your_api_key")


get_electric_mpan <- function(out, property_index = 1){

    require(tibble)
    require(dplyr)
    require(tidyr)

    mpan_import <- out$properties[[property_index]]$electricity_meter_points[[1]]$mpan
    mpan_export <- out$properties[[property_index]]$electricity_meter_points[[2]]$mpan
    serial_import <- out$properties[[property_index]]$electricity_meter_points[[1]]$meters[[1]]$serial_number
    serial_export <- out$properties[[property_index]]$electricity_meter_points[[2]]$meters[[1]]$serial_number
    #gas_mprn <- out$properties[[property_index]]$gas_meter_points[[1]]$mprn
    #gas_serial <- out$properties[[property_index]]$gas_meter_points[[1]]$meters[[1]]$serial_number

    mpan_table <- (tibble(property_index = property_index,
        mpan_import = mpan_import,
                mpan_export = mpan_export,
                serial_import = serial_import,
                serial_export = serial_export)) |>
        pivot_longer(cols = -property_index,
                     names_to = c(".value", "flow"),
                     names_sep = "_")

    return(mpan_table)




}
