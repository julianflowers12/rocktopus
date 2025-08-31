#' Construct a base Octopus API request
#'
#' Adds authentication and a custom User-Agent header.
#'
#' @param url Full endpoint URL
#' @param api_key Octopus API key
#'
#' @return An httr2 request object
#' @keywords internal


octo_request <- function(url, api_key) {
    httr2::request(url) |>
        httr2::req_auth_basic(username = api_key, password = "") |>
        httr2::user_agent(
            sprintf(
                "rocktopus/%s (R httr2; https://github.com/julianflowers12/rocktopus)",
                utils::packageVersion("rocktopus")
            )
        )
}
