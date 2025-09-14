#' Get Consumption Details
#' This function retrieves consumption details from the Octopus Energy API.
#' @param url The URL to the Octopus Energy API endpoint for consumption details.
#' @return A tibble containing the consumption details.
#' @examples
#' url <- "https://api.octopus.energy/v1/products/your_product_id/consumption/"
#' get_cons(url)
#' @export


get_cons <- function(url, api_key){


    octo_request <- function(url, api_key) {
        httr2::request(url) |>
            httr2::req_auth_basic(username = api_key, password = "") |>
            httr2::req_headers(
                "User-Agent" = sprintf(
                    "rocktopus/%s (R httr2; https://github.com/julianflowers12/rocktopus)",
                    utils::packageVersion("rocktopus")
                )
            )
    }

    resp <- octo_request(url, api_key = api_key) |>
        httr2::req_perform() |>
        httr2::resp_body_json()


    response <- resp |>
        tibble::enframe() |>
        dplyr::filter(name == "results") |>
        tidyr::unnest(value) |>
        tidyr::unnest_auto(value)

}
