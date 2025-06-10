#' Create an endpoint for fetching aggregated consumption data
#' @param mpan A string representing the MPAN (Meter Point Administration Number).
#' @param serial A string representing the meter serial number.
#' @param period_from A string representing the start date for the consumption data (default is "2024-01-01T00:00:00").
#' @param group_by A string representing the aggregation period (default is "day").
#'  @return A string representing the complete URL endpoint for fetching aggregated consumption data.
#'
#'
#' @details
#' The function constructs a URL that can be used to fetch aggregated consumption data for a specific electricity meter point.
#' The URL includes parameters for pagination, a specific time period, and the aggregation period.
#' @examples
#'create_endpoint_agg("1234567890123", "ABC123456789", "2024-01-01T00:00:00", "day")
#' @export
#'
#'


create_endpoint_agg <- function(mpan, serial, period_from = "2024-01-01T00:00:00", group_by = "day"){
    base_url <- "https://api.octopus.energy/v1/"

    url1 <- paste0(base_url, "electricity-meter-points/", mpan, "/meters/", serial, "/consumption/?page_size=25000&", "period_from" = period_from, "&group_by" = group_by)

    return(url1)
}


