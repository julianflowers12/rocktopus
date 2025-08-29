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

    mpan <- out$properties[[property_index]]$electricity_meter_points[[1]]$mpan
    mpan_export <- out$properties[[property_index]]$electricity_meter_points[[2]]$mpan
    meter_serial <- out$properties[[property_index]]$electricity_meter_points[[1]]$meters[[1]]$serial_number
    export_serial <- out$properties[[property_index]]$electricity_meter_points[[2]]$meters[[1]]$serial_number
    #gas_mprn <- out$properties[[property_index]]$gas_meter_points[[1]]$mprn
    #gas_serial <- out$properties[[property_index]]$gas_meter_points[[1]]$meters[[1]]$serial_number

    return(list(mpan = mpan,
                mpan_export = mpan_export,
                meter_serial = meter_serial,
                export_serial = export_serial)
    ) |>
        enframe() |>
        unnest()



}
