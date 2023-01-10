
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
    # Replace NULL with empty label
    labels [which (vapply (labels, is.null, logical (1L)))] <- ""
    # Then extract latest stage:
    stages <- vapply (labels, function (i) {
        ret <- NA_character_
        st_i <- grep ("^[0-9]\\/", i, value = TRUE)
        if (length (st_i) > 0) {
            ret <- st_i [length (st_i)]
        }
        return (ret)
    }, character (1L))
    # Also identify any issues with multiple stages:
    multiple_stages <- vapply (labels, function (i)
        length (grep ("^[0-9]\\/", i)) > 1L,
        logical (1L))

    # Finally, reduce labels to the non-stage values:
    labels <- lapply (labels, function (i) {
        st_i <- grep ("^[0-9]\\/", i)
        if (length (st_i) > 0) {
            i <- i [-st_i]
            if (length (i) == 0) {
                i <- ""
            }
        }
        return (i)
    })

    # And remove `pkgcheck` results:
    comments <- lapply (comments, function (i) {
        i [grep ("^## Checks for", i)] <- "## pkgcheck check results"
        return (i)  })

    res <- data.frame (
        number = number,
        titles = titles,
        stage = stages,
        has_multiple_stages = multiple_stages,
        labels = I (labels),
        assignees = I (assignees),
        createdAt = lubridate::date (lubridate::ymd_hms (createdAt)),
        lastEditedAt = lubridate::date (lubridate::ymd_hms (lastEditedAt)),
        updatedAt = lubridate::date (lubridate::ymd_hms (updatedAt)),
        comments = I (comments)
    )
    res <- res [order (res$stage), ]

    # That puts "0/editorial-team-pre" before "0/presubmission" - reverse these:
    index <- grep ("^0\\/", res$stage)
    if (length (index) > 0) {
        res0 <- res [index, ]
        res <- res [-index, ]
        index_pre <- grep ("presubmission", res0$stage)
        index_edi <- grep ("editorial", res0$stage)
        res <- rbind (
            res0 [index_pre, ],
            res0 [index_edi, ],
            res
        )
    }

    return (res)
}

#' Generate a summary report for incoming Editor-in-Charge of current state of
#' all open software-review issues.
#'
#' @param open If `TRUE` (default), open the results as a \pkg{DT} `datatable`
#' HTML page in default browser.
#' @return A `data.frame` with one row per issue and some key statistics.
#' @export

devguide_eic_report <- function (open = TRUE) {

    dat <- devguide_eic_gh_data ()

    cmt_data <- extract_comment_info (dat)
    dat$comments <- NULL

    dat <- dplyr::bind_cols (dat, cmt_data) %>%
        dplyr::relocate (editor, .after = labels) %>%
        dplyr::relocate (editor_date, .after = editor)

    if (any (dat$has_multiple_stages)) {
        numbers <- dat$number [which (dat$has_multiple_stages)]
        txt <- ifelse (
            length (numbers) == 1,
            "issue currently has",
            "issues currently have"
        )
        warning ("The following ", txt, " multiple 'stage' labels:\n   ",
                 paste0 (numbers, collapse = ", "))
    }

    # Collapse list columns:
    listcols <- c ("assignees", "labels")
    for (lc in listcols) {
        dat [[lc]] <- vapply (dat [[lc]], function (i) {
            paste0 (i, collapse = ", ")
        }, character (1L))
    }

    if (open) {
        open_gt_table (dat)
    }

    return (dat)
}

extract_comment_info <- function (dat) {

    extract_one <- function (x) {
        index <- 3 * seq_len (length (x) / 3) - 2

        dates <- x [index]
        actors <- x [index + 1]
        comments <- x [index + 2]

        index <- which (actors == "ropensci-review-bot" & grepl ("assigned|added", comments, ignore.case = TRUE))
        dates <- dates [index]
        comments <- comments [index]

        editor <- editor_date <-
            rev1 <- rev1_assigned <- rev1_due <- 
            rev2 <- rev2_assigned <- rev2_due <- ""

        index <- grep ("editor$", comments)
        if (length (index) > 0) {
            index <- max (index) # in case multiple editors re-assigned
            editor <- gsub ("^Assigned\\!\\s", "", comments [index])
            editor <- gsub ("\\s.*$", "", editor)
            editor_date <- lubridate::date (lubridate::ymd_hms (dates [index]))
        }

        extract_rev <- function (comments, dates, index) {
            rev1 <- gsub ("\\s.*$", "", comments [index [1]])
            rev1_assigned <- lubridate::date (lubridate::ymd_hms (dates [index [1]]))
            g <- regexpr ("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}", comments [index [1]])
            rev1_due <- regmatches (comments [index [1]], g)
            c (rev1, paste0 (rev1_assigned), rev1_due)
        }
        index <- grep ("reviewers list", comments)
        if (length (index) > 0) {
            rd1 <- extract_rev (comments, dates, index)
            rev1 <- rd1 [1]
            rev1_assigned <- rd1 [2]
            rev1_due <- rd1 [3]
            index <- index [-1]
            if (length (index) > 0) {
                rd2 <- extract_rev (comments, dates, index)
                rev2 <- rd2 [1]
                rev2_assigned <- rd2 [2]
                rev2_due <- rd2 [3]
            }
        }

        c (
            editor = editor,
            editor_date = paste0 (editor_date),
            rev1 = rev1,
            rev1_assigned = rev1_assigned,
            rev1_due = rev1_due,
            rev2 = rev2,
            rev2_assigned = rev2_assigned,
            rev2_due = rev2_due
        )
    }

    res <- lapply (dat$comments, extract_one)

    editor <- vapply (res, function (i) i [["editor"]], character (1L))
    editor_date <- vapply (res, function (i) i [["editor_date"]], character (1L))
    rev1 <- vapply (res, function (i) i [["rev1"]], character (1L))
    rev1_assigned <- vapply (res, function (i) i [["rev1_assigned"]], character (1L))
    rev1_due <- vapply (res, function (i) i [["rev1_due"]], character (1L))
    rev2 <- vapply (res, function (i) i [["rev2"]], character (1L))
    rev2_assigned <- vapply (res, function (i) i [["rev2_assigned"]], character (1L))
    rev2_due <- vapply (res, function (i) i [["rev2_due"]], character (1L))

    return (data.frame (
        editor = editor,
        editor_date = editor_date,
        rev1 = rev1,
        rev1_assigned = rev1_assigned,
        rev1_due = rev1_due,
        rev2 = rev2,
        rev2_assigned = rev2_assigned,
        rev2_due = rev2_due
    ))
}

open_gt_table <- function (dat) {

    requireNamespace ("gt")
    u <- "https://github.com/ropensci/software-review/issues/"
    dat_url <- dat
    add_html <- function (dat_url, what = "number") {
        dat_url [[what]] <- paste0 (
            "<p><a href=",
            u,
            dat_url$number,
            ">",
            dat_url [[what]],
            "</a>"
        )
        dat_url [[what]] <- lapply (dat_url [[what]], gt::html)
        return (dat_url)
    }
    # Number has to come last here!!
    dat_url <- add_html (dat_url, "titles")
    dat_url <- add_html (dat_url, "number")

    gt::gt (
        dat_url,
        #rowname_col = "number",
        groupname_col = "stage"
    ) %>%
        gt::tab_header ("rOpenSci submission overview") %>%
        gt::cols_hide (has_multiple_stages) %>%
        gt::tab_style (
            style = list ( gt::cell_fill (color = "#FFFF99")),
            locations = gt::cells_body (
                columns = c (`number`, `titles`, `labels`)
            )
        ) %>%
        gt::tab_spanner (
            label = "Editor",
            id = "ed_span",
            columns = c (`editor`, `editor_date`, `assignees`)
        ) %>%
        gt::tab_spanner (
            label = "Dates",
            columns = c (`createdAt`, `lastEditedAt`, `updatedAt`)
        ) %>%
        gt::tab_style (
            style = list ( gt::cell_fill (color = "#EEEEEE")),
            locations = gt::cells_body (
                columns = c (`createdAt`, `lastEditedAt`, `updatedAt`)
            )
        ) %>%
        gt::tab_spanner (
            label = "Reviewer #1",
            id = "rev1span",
            columns = c (`rev1`, `rev1_assigned`, `rev1_due`)
        ) %>%
        gt::tab_spanner (
            label = "Reviewer #2",
            columns = c (`rev2`, `rev2_assigned`, `rev2_due`)
        ) %>%
        gt::tab_style (
            style = list (gt::cell_fill (color = "#EEEEEE")),
            locations = gt::cells_body (
                columns = c (`rev2`, `rev2_assigned`, `rev2_due`)
            )
        ) %>%
        gt::tab_style (
            # Alternative dark separators to distinguish group titles
            style = list (
                gt::cell_borders (side = "bottom", color = "#666666", weight = gt::px (3))
            ),
            locations = gt::cells_column_spanners (spanners = c ("ed_span", "rev1span"))
        ) %>%
        gt::tab_options (
            heading.background.color = "#ACEACE",
            row_group.background.color = "#FFAA77",
            column_labels.background.color = "#9BD9BD",
            heading.title.font.size = "200%"
        )
}
