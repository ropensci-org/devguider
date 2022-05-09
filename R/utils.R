
#' Get local path to `dev_guide` repo
#'
#' @param path Path to start searching for `dev_guide` repo.
#' @return Local path to `dev_guide` repo.
#' @export
devguide_path <- function (path = ".") {

    m_devguide_path_internal (path)
}

devguide_path_internal <- function (path) {

    max_levels <- 5L # maximum directory levels to search upwards
    repo_name <- "dev_guide"

    path <- normalizePath (path)

    has_devguide <- FALSE
    num_levels <- 0L
    while (!has_devguide & num_levels < max_levels) {

        path <- normalizePath (file.path (path, ".."))
        d <- list.dirs (path,
                        recursive = TRUE,
                        full.names = TRUE)
        dspl <- fs::path_split (d)
        devguide <- vapply (dspl, function (i)
                            any (grepl (repo_name, i)),
                            logical (1L))
        has_devguide <- any (devguide)

        num_levels <- num_levels + 1L
    }

    d <- d [which (devguide)]
    lens <- vapply (fs::path_split (d), length, integer (1L))

    return (d [which.min (lens)])
}

m_devguide_path_internal <- memoise::memoise (devguide_path_internal)

devguide_repo <- function () {

    path <- devguide_path ()
    desc <- data.frame (read.dcf (file.path (path, "DESCRIPTION")))

    return (desc$URL)
}

# Adapted from blogdown
get_rmarkdown_body <- function (x)
{
  i = grep("^---\\s*$", x)
  n = length(x)
  x[(i[2] + 1):n]
}
