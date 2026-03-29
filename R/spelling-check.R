
#' Check spelling
#'
#' @export
devguide_spelling <- function () {

    path <- devguide_path ()

    # English ----

    f <- list.files (devguide_path (),
                     pattern = "\\.Rmd",
                     recursive = TRUE,
                     full.names = TRUE)
    f <- f[!endsWith(f, ".es.Rmd")]
    f <- f[!endsWith(f, ".pt.Rmd")]
    ig <- scan (file.path (devguide_path (), "inst", "WORDLIST"),
                what = "",
                quiet = TRUE)

    spelling::spell_check_files (f, ignore = ig)

    es_f <- list.files (devguide_path (),
                     pattern = "\\.es\\.Rmd",
                     recursive = TRUE,
                     full.names = TRUE)
    ig <- scan (file.path (devguide_path (), "inst", "WORDLIST"),
                what = "",
                quiet = TRUE)

    spelling::spell_check_files (es_f, lang = "es_AR", ignore = ig)
}
