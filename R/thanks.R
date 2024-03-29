#' Extract list of people to thank for Dev Guide contributions
#'
#' @export
devguide_thanks <- function () {

    # suppress no visible binding notes:
    . <- domain <- path <- NULL

    thanks <- withr::with_dir (devguide_path (), {

        index_thanks <- readLines("index.Rmd") %>%
          get_rmarkdown_body() %>%
          commonmark::markdown_xml(extensions = TRUE) %>%
          xml2::read_xml() %>%
          xml2::xml_find_all(xpath = './/d1:link', xml2::xml_ns(.)) %>%
          xml2::xml_attr("destination") %>%
          urltools::url_parse() %>%
          dplyr::filter(
            domain == "github.com"
            ) %>%
          dplyr::mutate(
            path = gsub("\\/$", "", path)
          ) %>%
          dplyr::filter(
            !grepl("\\/", path)
          ) %>%
          dplyr::pull(path)

        changelog_thanks <- readLines("appendix.Rmd") %>%
          get_rmarkdown_body() %>%
          commonmark::markdown_xml(extensions = TRUE) %>%
          xml2::read_xml() %>%
          xml2::xml_find_all(xpath = './/d1:code', xml2::xml_ns(.)) %>%
          xml2::xml_text() %>%
          purrr::keep(function(x) grepl("^@", x)) %>%
          stringr::str_remove_all("^@") %>%
          unique()

        thanks2020 <- readLines("appendix.Rmd")[1:83] %>%
          get_rmarkdown_body() %>%
          commonmark::markdown_xml(extensions = TRUE) %>%
          xml2::read_xml() %>%
          xml2::xml_find_all(xpath = './/d1:code', xml2::xml_ns(.)) %>%
          xml2::xml_text() %>%
          purrr::keep(function(x) grepl("^@", x)) %>%
          stringr::str_remove_all("^@") %>%
          tolower() %>%
          unique()

        list (
            changelog = changelog_thanks[!changelog_thanks %in% index_thanks],
            thanks2020 = thanks2020[!thanks2020 %in%
                                    c("includermd", "example", "sckott", "karthik", "annakrystalli",
                                      "noamross", "stefaniebutland", "jeroen", "mpadge")]
            )
    })

    return (thanks)
}
