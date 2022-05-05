
#' Get and optionally open all URLs used in Dev Guide
#'
#' This function can take a long time to run!
#'
#' @return Information on all URLs which are not okay.
#' @export
devguide_urls <- function () {

    Rmds <- fs::dir_ls(devguide_path (),    # nolint
                       regexp = " *.Rmd")

    find_urls <- function(filepath) {
      readLines(filepath) %>%
        glue::glue_collapse(sep = "\n") %>%
        commonmark::markdown_html(normalize = TRUE,
                                  extensions = TRUE) %>%
        xml2::read_html() %>%
        xml2::xml_find_all("//a") %>%
        xml2::xml_attr("href") -> urls

      urls <- urls[!grepl("^\\#", urls)]

      tibble::tibble(filepath = filepath,
                     url = urls)
    }

    .ok <- function(url) {
      message(url)
      crul::ok(
        url,
        verb = "get",
        useragent = "Maëlle Salmon"
        )
    }

    ok <- memoise::memoise(
      ratelimitr::limit_rate(
        .ok,
        ratelimitr::rate(1, 1)
      )
    )

    all_urls <- purrr::map_df(Rmds, find_urls)
    all_urls <- all_urls[!grepl("<issue_id>", all_urls$url), ]
    all_urls <- all_urls [which (!duplicated (all_urls)), ]
    all_urls$ok <- purrr::map_lgl(
      all_urls$url,
      ok
      )

    all_urls[!all_urls$ok, ]
}
