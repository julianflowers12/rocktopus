#' Create an endpoint for fetching consumption data from Octopus Energy API
#' @param mpan A string representing the MPAN (Meter Point Administration Number).
#' @param serial A string representing the meter serial number.
#' @return A string representing the complete URL endpoint for fetching consumption data.
#' @examples
#' # create_endpoint("1234567890123", "ABC123456789")
#' @export




create_endpoint <- function(mpan, serial){
    base_url <- "https://api.octopus.energy/v1/"

    url1 <- paste0(base_url, "electricity-meter-points/", mpan, "/meters/", serial, "/consumption/?page_size=25000&period_from=2024-01-01T00:00Z")

    return(url1)
}
