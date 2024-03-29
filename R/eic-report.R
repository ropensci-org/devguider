
get_gh_token <- function (token = "") {

    tryCatch (
        gitcreds::gitcreds_get ()$password,
        error = function (e) ""
    )
}

get_issues_qry <- function (org = "ropensci",
                            repo = "software-review",
                            open_only = TRUE,
                            end_cursor = NULL) {

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

    open_txt <- ifelse (open_only, ", states: [OPEN]", "")

    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
                   issues (first: 100", open_txt, after_txt, ") {
                       pageInfo {
                           hasNextPage
                           endCursor
                       }
                       edges {
                           node {
                               ... on Issue {
                                   number
                                   createdAt
                                   state
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
                                               name,
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
                                   timelineItems (itemTypes: LABELED_EVENT, first: 100) {
                                       nodes {
                                           ... on LabeledEvent {
                                               actor {
                                                   login
                                               },
                                               createdAt,
                                               label {
                                                   name
                                               }
                                           }
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
devguide_eic_gh_data <- function (open_only = TRUE) {

    has_next_page <- TRUE
    end_cursor <- NULL

    number <- assignees <- created_at <- last_edited_at <-
        updated_at <- state <- titles <- NULL
    # The "event_" field come from the timeline data, and include data on all
    # events, both addition and removal of labels. "labels" holds the current
    # labels only.
    labels <- event_labels <- event_dates <- event_actors <- comments <- list ()

    page_count <- 0L

    while (has_next_page) {

        q <- get_issues_qry (
            org = "ropensci",
            repo = "software-review",
            open_only = open_only,
            end_cursor = end_cursor
        )
        dat <- gh::gh_gql (query = q)

        has_next_page <- dat$data$repository$issues$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$issues$pageInfo$endCursor

        edges <- dat$data$repository$issues$edges

        number <- c (
            number,
            vapply (edges, function (i) i$node$number, integer (1L))
        )
        state <- c (
            state,
            vapply (edges, function (i) i$node$state, character (1L))
        )
        assignees <- c (
            assignees,
            lapply (edges, function (i) unlist (i$node$assignees$nodes))
        )
        created_at <- c (
            created_at,
            vapply (edges, function (i) i$node$createdAt, character (1L))
        )
        last_edited_at <- c (
            last_edited_at,
            vapply (edges, function (i) {
                res <- i$node$lastEditedAt
                if (is.null (res)) res <- ""
                return (res)
            }, character (1L))
        )
        updated_at <- c (
            updated_at,
            vapply (edges, function (i) i$node$updatedAt, character (1L))
        )
        titles <- c (
            titles,
            vapply (edges, function (i) i$node$title, character (1L))
        )
        labels <- c (
            labels,
            lapply (edges, function (i) unname (unlist (i$node$labels)))
        )
        # Dates for labels are in separate "timeline" data:
        event_labels <- c (
            event_labels,
            lapply (edges, function (i) {
                unlist (lapply (i$node$timeline$nodes,
                    function (j) j$label$name))
            })
        )
        event_dates <- c (
            event_dates,
            lapply (edges, function (i) {
                unlist (lapply (i$node$timeline$nodes,
                    function (j) j$createdAt))
            })
        )
        event_actors <- c (
            event_actors,
            lapply (edges, function (i) {
                unlist (lapply (i$node$timeline$nodes,
                    function (j) j$actor$login))
            })
        )

        comments <- c (
            comments,
            lapply (edges, function (i) unname (unlist (i$node$comments$nodes)))
        )

        page_count <- page_count + 1L
        message (
            "Retrieved page [", page_count, "] to issue number [",
            max (number), "]"
        )
    }

    # Reduce "event" data down to current labels only, and sort by labels so
    # that "official" 'N/' ones come first, which happens to be done perfectly
    # by standard sort:
    for (i in seq_along (labels)) {
        index <- which (event_labels [[i]] %in% labels [[i]])
        if (length (index) > 0L) {
            index <- index [order (event_labels [[i]] [index])]
            event_labels [[i]] <- event_labels [[i]] [index]
            event_dates [[i]] <- event_dates [[i]] [index]
            event_actors [[i]] <- event_actors [[i]] [index]
        }
    }
    # neither "labels" nor "event_actors" are used from that point on.

    # Replace NULL with empty label
    event_labels [which (vapply (event_labels, is.null, logical (1L)))] <- ""
    event_dates [which (vapply (event_dates, is.null, logical (1L)))] <- ""

    # Then extract latest stage and associated dates
    stages <- vapply (seq_along (labels), function (i) {
        ret <- rep (NA_character_, 2L)
        st_i <- grep ("^[0-9]\\/", event_labels [[i]])
        if (length (st_i) > 0) {
            st_i <- max (st_i) # in case multiple stage labels
            ret <- c (
                event_labels [[i]] [st_i],
                as.character (event_dates [[i]] [st_i])
            )
        }
        return (ret)
    }, character (2L))
    stages <- data.frame (t (stages))
    names (stages) <- c ("label", "date")
    stages$date <- as.Date (stages$date)

    # Also identify any issues with multiple stages:
    multiple_stages <- vapply (event_labels, function (i) {
        n_total <- length (grep ("^[0-9]\\/", i))
        n_approved <- length (grep ("^6\\/", i))
        n_total > 1L && n_total != n_approved
    }, logical (1L))

    # Finally, reduce labels to the non-stage values, ignoring
    # `labels_created_at` and `labels_updated_at` from here.
    event_labels <- lapply (event_labels, function (i) {
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
        title = titles,
        state = state,
        stage = stages$label,
        stage_date = stages$date,
        has_multiple_stages = multiple_stages,
        labels = I (event_labels),
        assignees = I (assignees),
        created_at = lubridate::date (lubridate::ymd_hms (created_at)),
        last_edited_at = lubridate::date (lubridate::ymd_hms (last_edited_at)),
        updated_at = lubridate::date (lubridate::ymd_hms (updated_at)),
        comments = I (comments)
    ) %>% dplyr::arrange (stage, stage_date)

    # That puts "0/editorial-team-prep" before "0/presubmission" - reverse these:
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
#' @param open_only If `TRUE` (default), only extract information for currently
#' open issues.
#' @param browse If `TRUE` (default), open the results as a \pkg{DT} `datatable`
#' HTML page in default browser.
#' @return A `data.frame` with one row per issue and some key statistics.
#' @export

devguide_eic_report <- function (open_only = TRUE, browse = TRUE) {

    dat <- devguide_eic_gh_data (open_only)

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

    if (browse) {
        print (open_gt_table (dat))
    }

    return (dat)
}

extract_comment_info <- function (dat) {

    extract_one <- function (x) {
        index <- 3 * seq_len (length (x) / 3) - 2

        dates <- x [index]
        actors <- x [index + 1]
        comments <- x [index + 2]

        index <- which (
            actors == "ropensci-review-bot" &
            grepl ("assigned|added", comments, ignore.case = TRUE)
        )
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
            rev1_assigned <-
                lubridate::date (lubridate::ymd_hms (dates [index [1]]))
            g <- regexpr (
                "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",
                comments [index [1]]
            )
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
    editor_date <- vapply (res, function (i) {
        i [["editor_date"]]
    }, character (1L))
    rev1 <- vapply (res, function (i) i [["rev1"]], character (1L))
    rev1_assigned <- vapply (res, function (i) {
        i [["rev1_assigned"]]
    }, character (1L))
    rev1_due <- vapply (res, function (i) i [["rev1_due"]], character (1L))
    rev2 <- vapply (res, function (i) i [["rev2"]], character (1L))
    rev2_assigned <- vapply (res, function (i) {
        i [["rev2_assigned"]]
    }, character (1L))
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
    dat_url$stage_elapsed <- Sys.Date () - dat_url$stage_date

    # Then create an "urgency" column used to highlight rows needing urgent
    # attention. Note that colours have to be individually hand-coded in the
    # `gt` code below, so use of any value other than `ncols = 5` requires the
    # subsequent colors to be re-coded.
    ncols <- 5L
    dat_url$urgency <- 0L

    # time scales for each stage, in days
    time_scales <- list (
        c (0, 7),   # initial editorial handling
        c (1, 7),   # editor checks
        c (2, 14),  # seeking reviewers
        c (3, 21),  # reviews
        c (4, 21),  # author responses
        c (5, 21)   # reviewer responses
    )
    for (i in time_scales) {
        index <- grep (paste0 ("^", i [1]), dat_url$stage)
        dat_url$urgency [index] <-
            floor (as.numeric (dat_url$stage_elapsed [index]) / i [2])
    }

    dat_url$urgency [dat_url$urgency > ncols] <- ncols
    dat_url$urgency [grepl ("holding", dat_url$labels)] <- 0

    gt::gt (
        dat_url,
        groupname_col = "stage"
    ) %>%
        gt::tab_header ("rOpenSci submission overview") %>%
        gt::cols_hide (c (stage_elapsed, urgency, has_multiple_stages)) %>%
        gt::tab_spanner (
            label = "Editor",
            id = "ed_span",
            columns = c (`editor`, `editor_date`, `assignees`)
        ) %>%
        gt::tab_spanner (
            label = "Dates",
            columns = c (`created_at`, `last_edited_at`, `updated_at`)
        ) %>%
        gt::tab_style (
            style = list (gt::cell_fill (color = "#EEEEEE")),
            locations = gt::cells_body (
                columns = c (`created_at`, `last_edited_at`, `updated_at`)
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
                gt::cell_borders (
                    side = "bottom",
                    color = "#666666",
                    weight = gt::px (3)
                )),
            locations = gt::cells_column_spanners (
                spanners = c ("ed_span", "rev1span")
            )
        ) %>%
        gt::tab_style (
            # Finally styles for "stage_date" columns to highlight need for
            # action. Each grade of "urgency" has to be individually specified.
            style = list (gt::cell_fill (color = "#FFFF8088")),
            locations = gt::cells_body (
                columns = c (`number`, `title`, `stage_date`, `labels`),
                rows = urgency == 1
            )
        ) %>%
        gt::tab_style (
            style = list (gt::cell_fill (color = "#FFFF0088")),
            locations = gt::cells_body (
                columns = c (`number`, `title`, `stage_date`, `labels`),
                rows = urgency == 2
            )
        ) %>%
        gt::tab_style (
            style = list (gt::cell_fill (color = "#FFAA0088")),
            locations = gt::cells_body (
                columns = c (`number`, `title`, `stage_date`, `labels`),
                rows = urgency == 3
            )
        ) %>%
        gt::tab_style (
            style = list (gt::cell_fill (color = "#FF550088")),
            locations = gt::cells_body (
                columns = c (`number`, `title`, `stage_date`, `labels`),
                rows = urgency == 4
            )
        ) %>%
        gt::tab_style (
            style = list (gt::cell_fill (color = "#FF000088")),
            locations = gt::cells_body (
                columns = c (`number`, `title`, `stage_date`, `labels`),
                rows = urgency == 5
            )
        ) %>%
        gt::tab_options (
            heading.background.color = "#ACEACE",
            row_group.background.color = "#ACEACE",
            column_labels.background.color = "#9BD9BD",
            heading.title.font.size = "200%"
        )
}
