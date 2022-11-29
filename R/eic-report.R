
get_gh_token <- function (token = "") {

    tryCatch (
        gitcreds::gitcreds_get ()$password,
        error = function (e) ""
    )
}

get_issues_qry <- function (org = "ropensci",
                            repo = "software-review",
                            end_cursor = NULL) {

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
                   issues (first: 100", after_txt, ", states: [OPEN]) {
                       pageInfo {
                           hasNextPage
                           endCursor
                       }
                       edges {
                           node {
                               ... on Issue {
                                   number
                                   createdAt
                                   lastEditedAt
                                   updatedAt
                                   assignees (first: 100) {
                                       nodes {
                                           name
                                       }
                                   }
                                   title
                                   labels (first: 100) {
                                       edges {
                                           node {
                                               name
                                           }
                                       }
                                   }
                                   url
                                   comments (last: 100) {
                                       nodes {
                                           createdAt,
                                           author {
                                               login
                                           },
                                           body
                                       }
                                   }
                               }
                           }
                       }
                   }
                }
        }")

    return (q)
}

#' Apply the query above to extract data from GitHub GraphQL API, and
#' post-process into a `data.frame`.
#'
#' @return (Invisibly) A `data.frame` with one row per issue and some key
#' statistics.
#' @noRd
devguide_eic_gh_data <- function () {

    has_next_page <- TRUE
    end_cursor <- NULL

    number <- assignees <- createdAt <- lastEditedAt <-
        updatedAt <- titles <- NULL
    labels <- comments <- list ()

    while (has_next_page) {

        q <- get_issues_qry (
            org = "ropensci",
            repo = "software-review",
            end_cursor = end_cursor
        )
        dat <- gh::gh_gql (query = q)

        has_next_page <- dat$data$repository$issues$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$issues$pageInfo$endCursor

        edges <- dat$data$repository$issues$edges

        number <- c (number, vapply (edges, function (i) i$node$number, integer (1L)))
        assignees <- c (assignees, lapply (edges, function (i) unlist (i$node$assignees$nodes)))
        createdAt <- c (createdAt, vapply (edges, function (i) i$node$createdAt, character (1L)))
        lastEditedAt <- c (lastEditedAt, vapply (edges, function (i) {
            res <- i$node$lastEditedAt
            if (is.null (res)) res <- ""
            return (res)
        }, character (1L)))
        updatedAt <- c (updatedAt, vapply (edges, function (i) i$node$updatedAt, character (1L)))
        titles <- c (titles, vapply (edges, function (i) i$node$title, character (1L)))
        labels <- c (labels, lapply (edges, function (i) unname (unlist (i$node$labels$edges))))
        comments <- c (comments, lapply (edges, function (i)
            unname (unlist (i$node$comments$nodes))))
    }

    # Sort labels so "official" 'N/' ones come first, which happens to be done
    # perfectly by standard sort:
    labels <- lapply (labels, function (i) sort (i))
    # Then separate out that first as "stage"
    stages <- vapply (labels, function (i) i [1], character (1L))
    labels <- lapply (labels, function (i) i [-1])

    # And remove `pkgcheck` results:
    comments <- lapply (comments, function (i) {
        i [grep ("^## Checks for", i)] <- "## pkgcheck check results"
        return (i)  })

    res <- data.frame (
        number = number,
        stage = stages,
        assignees = I (assignees),
        createdAt = lubridate::date (lubridate::ymd_hms (createdAt)),
        lastEditedAt = lubridate::date (lubridate::ymd_hms (lastEditedAt)),
        updatedAt = lubridate::date (lubridate::ymd_hms (updatedAt)),
        titles = titles,
        labels = I (labels),
        comments = I (comments)
    )
    res [order (res$stage), ]
}

#' Generate a summary report for incoming Editor-in-Charge of current state of
#' all open software-review issues.
#'
#' @return (Invisibly) A `data.frame` with one row per issue and some key
#' statistics.
#' @export

devguide_eic_report <- function () {

    dat <- devguide_eic_gh_data ()

    return (dat)
}
