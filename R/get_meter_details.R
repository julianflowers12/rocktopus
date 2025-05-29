#' Get Meter Details
#' This function retrieves details about a specific meter associated with an Octopus Energy account.
#' @param acct_no A string representing the account number.
#' @param api_key A string representing the API key for authentication.
#' @return A list containing the meter details.
#' @examples
#' # get_meter_details("your_account_number", "your_api_key")
#' @export
#' @import httr2 dplyr

get_meter_details <- function(acct_no, apikey){

    require(httr2)
    require(dplyr)

    urlx = paste0("https://api.octopus.energy/v1/accounts/", acct_no, "/")

    response <- request(urlx) |>
        req_auth_basic(username = api_key, password = "") |> req_perform()

    out <- response |> resp_body_json()

    return(out)
}
