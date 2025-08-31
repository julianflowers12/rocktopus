#' Process Consumption Data
#' This function processes consumption data from the Octopus Energy API.
#'  @param consumption_data A data frame containing consumption data with columns:
#' - interval_start: Start time of the consumption interval
#' - interval_end: End time of the consumption interval
#' - other relevant columns
#' @return A data frame with processed consumption data, including:
#' - interval_start: Start time of the consumption interval
#' - interval_end: End time of the consumption interval
#' - interval: Duration of the consumption interval
#' - date: Date of the consumption interval
#' - time: Time of the consumption interval
#' - start: Start time of the consumption interval in POSIXct format
#' - hour_st: Hour of the start time
#' - hour_end: Hour of the end time
#' - wk: Week number of the year
#' - month: Month of the year (abbreviated)
#' - year: Year of the consumption interval
#' - peak: Peak or off-peak status based on the start hour
#' @examples
#' consumption_data <- read.csv("consumption_data.csv")
#' processed_data <- process_consumption_data(consumption_data)
#' @export
#' @import dplyr lubridate stringr
#' @importFrom lubridate ymd_hms ymd hm hour week month year
#' @importFrom stringr str_sub
#' @description
#' This function processes consumption data by converting date and time formats,
#' calculating the duration of each interval, extracting date and time components,
#' and categorizing the intervals into peak and off-peak periods based on the start hour.
#' The function returns a data frame with the processed data, including additional columns
#' for peak status and time components.
#'  @details
#'  The function assumes that the input data frame contains columns named `interval_start`
#'  and `interval_end` in a format compatible with `ymd_hms`. It also assumes that the
#'  input data frame has other relevant columns that will be retained in the output.
#'  The peak status is determined based on the start hour of the interval, where hours
#'  between 6 and 22 are considered peak hours, and hours between 23 and 5 are considered
#'  off-peak hours. If the start hour is not available, it is categorized as "unknown".
#'  @note
#'  This function requires the `dplyr`, `lubridate`, and `stringr` packages for data manipulation
#'  and date-time handling. Ensure these packages are installed and loaded before using the function.
#'  @seealso
#'  - `dplyr` for data manipulation functions
#'  - `lubridate` for date-time handling functions
#'  - `stringr` for string manipulation functions
#'  @examples
#'  Example usage
#'  Load the consumption data
#'  consumption_data <- read.csv("consumption_data.csv")
#'  Process the data
#'  processed_data <- process_consumption_data(consumption_data)
#'  View the processed data
#'  print(processed_data)
#'  @importFrom dplyr mutate select case_when
#'  @importFrom lubridate ymd_hms ymd hm hour
#'  @importFrom stringr str_sub
#'
#'
#'
#'


process_consumption_data <- function(consumption_data){

    consumption_data <- consumption_data |>
        mutate(interval_start = ymd_hms(interval_start),
               interval_end = ymd_hms(interval_end),
               interval = interval_end - interval_start,
               date = ymd(str_sub(interval_start, 1, 10)),
               time = hm(str_sub(interval_start, 12, 16)),
               start = ymd_hms(interval_start),
               hour_st = hour(start),
               hour_end = hour(interval_end),
               wk = week(date),
               month = lubridate::month(date, label = TRUE, abbr = TRUE),
               year = year(date)) |>
        select(wk, everything()) |>
        mutate(peak = case_when(hour_st %in% c(23, 0:5) | is.na(hour_st)  ~ "off_peak",
                                hour_st == 23 & hour_end == 0 ~ "off_peak",
                                hour_st == 5 & hour_end == 5 ~ "off_peak",
                                hour_st %in% c(6:22) ~ "peak",
                                TRUE ~ "unknown"))

    return(consumption_data)
}
