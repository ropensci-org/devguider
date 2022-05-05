
#' Check spelling
#'
#' @export
devguide_spelling <- function () {

    path <- devguide_path ()

    f <- list.files (devguide_path (),
                     pattern = "\\.Rmd",
                     recursive = TRUE,
                     full.names = TRUE)
    ig <- scan (file.path (devguide_path (), "inst", "WORDLIST"),
                what = "",
                quiet = TRUE)

    spelling::spell_check_files (f, ignore = ig)
}
