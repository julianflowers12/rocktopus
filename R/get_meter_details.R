#' Get Meter Details
#' This function retrieves details about a specific meter associated with an Octopus Energy account.
#' @param acct_no A string representing the account number.
#' @param api_key A string representing the API key for authentication.
#' @return A list containing the meter details.
#' @examples
#' # get_meter_details("your_account_number", "your_api_key")
#' @export
#' @import httr2 dplyr

get_meter_info <- function(acct_no, api_key){

    require(httr2)
    require(dplyr)

    urlx = paste0("https://api.octopus.energy/v1/accounts/", acct_no, "/")

    resp <- octo_request(urlx, api_key) |>
        httr2::req_perform()

    out <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    return(out)
}





