
get_gh_token <- function (token = "") {

    tryCatch (
        gitcreds::gitcreds_get ()$password,
        error = function (e) ""
    )
}

get_issues_qry <- function (gh_cli,
                            org = "ropensci",
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
#' @noRd
devguide_eic_gh_data <- function () {

    token <- get_gh_token ()
    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )

    has_next_page <- TRUE
    end_cursor <- NULL

    number <- assignees <- createdAt <- lastEditedAt <-
        updatedAt <- titles <- NULL
    labels <- comments <- list ()

    while (has_next_page) {

        qry <- ghql::Query$new ()
        q <- get_issues_qry (
            gh_cli,
            org = "ropensci",
            repo = "software-review",
            end_cursor = end_cursor
        )
        qry$query ("issues", q)

        dat <- gh_cli$exec (qry$queries$issues) %>%
            jsonlite::fromJSON ()

        has_next_page <- dat$data$repository$issues$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$issues$pageInfo$endCursor

        dat <- dat$data$repository$issues$edges$node

        number <- c (number, dat$number)
        assignees <- c (assignees, dat$assignees$nodes)
        createdAt <- c (createdAt, dat$createdAt)
        lastEditedAt <- c (lastEditedAt, dat$lastEditedAt)
        updatedAt <- c (updatedAt, dat$updatedAt)
        titles <- c (titles, dat$title)
        labels <- c (labels, dat$labels$edges)
        comments <- c (comments, dat$comments$nodes)
    }

    labels <- lapply (labels, function (i) i$node$name)

    data.frame (
        number = number,
        assignees = I (assignees),
        createdAt = lubridate::ymd_hms (createdAt),
        lastEditedAt = lubridate::ymd_hms (lastEditedAt),
        updatedAt = lubridate::ymd_hms (updatedAt),
        titles = titles,
        labels = I (labels),
        comments = I (comments)
    )
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
