#' Table of external links used in the dev guide
#'
#'
#' @return a tibble
#' @export
#'
devguide_links <- function() {
  Rmds <- fs::dir_ls(devguide_path (),    # nolint
    regexp = " *.Rmd")
  Rmds <- Rmds[!grepl("\\.es\\.Rmd", Rmds)]

  find_urls <- function(filepath) {
    readLines(filepath) %>%
      glue::glue_collapse(sep = "\n") %>%
      commonmark::markdown_html(normalize = TRUE,
        extensions = TRUE) %>%
      xml2::read_html() %>%
      xml2::xml_find_all("//a") -> urls

    urls <- urls[!grepl("^\\#", urls)]

    tibble::tibble(
      filepath = fs::path_file(filepath),
      title = xml2::xml_text(urls),
      url = xml2::xml_attr(urls, "href")
    )
  }

  urls <- purrr::map_df(Rmds, find_urls)
  urls <- urls[grepl("^http", urls$url),]
}
