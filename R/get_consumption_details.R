#' Get Consumption Details
#' This function retrieves consumption details from the Octopus Energy API.
#' @param url The URL to the Octopus Energy API endpoint for consumption details.
#' @return A tibble containing the consumption details.
#' @examples
#' url <- "https://api.octopus.energy/v1/products/your_product_id/consumption/"
#' get_cons(url)
#' @export





get_cons <- function(url){

    response <- request(urly) |>
        req_auth_basic(username = api_key, password = "") |> req_perform() |>
        resp_body_json()

    response <- response |>
        tibble::enframe() |>
        dplyr::filter(name == "results") |>
        tidyr::unnest(value) |>
        tidyr::unnest_auto(value)
}
