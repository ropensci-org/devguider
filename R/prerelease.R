#' Open a pre-release issue on the dev_guide repo
#'
#' Uses the gh cli (https://cli.github.com), which must be installed and
#' authorised with 'gh auth'. The pre-release issue will include all open issues
#' assigned to the local git user.
#'
#' @param post If `FALSE` return markdown-formatted text of comment that would
#' be used to open a new issue, otherwise open issue with that text.
#' @return Text of issue comment (invisibly).
#' @export
devguide_prerelease <- function (post = TRUE) {

    repo <- devguide_repo ()

    checklist <- post_issue (repo, post = post)

    invisible (checklist)
}

has_gh_cli <- function () {

    ghs_path <- dirname (Sys.which ("gh"))
    nzchar (ghs_path)
}

get_version <- function () {

    u <- "https://api.github.com/repos/ropensci/dev_guide/releases"
    x <- jsonlite::fromJSON (u)
    version_current <- x$name [1]
    version_number <- gsub ("^v0\\.|\\.[0-9]+$", "", version_current)
    version_next <- paste0 ("0.", as.integer (version_number) + 1, ".0")
    return (version_next)
}

get_git_user <- function () {

    gsub ("\\s<.*$", "", gert::git_signature_default ())
}

# Currently open issues assigned either to the authorised user
# or to the current milestone
get_issues <- function (repo, version) {

    args <- list (
        "issue",
        "list",
        "--repo", repo,
        "--assignee", "@me",
        "--state", "open"
    )

    issues_me <- system2 ("gh", args = args, stdout = TRUE)

    args <- list (
        "issue",
        "list",
        "--repo", repo,
        "--milestone", version,
        "--state", "open"
    )

    issues_milestone <- system2 ("gh", args = args, stdout = TRUE)

    issues <- unique (c (issues_me, issues_milestone))

    issue_nums <- regmatches (issues, regexpr ("^[0-9]+", issues))
    issues <- gsub ("^[0-9]+\\tOPEN\\t", "", issues)
    issues <- gsub ("\\t.*$", "", issues)
    issue_titles <- gsub ("\\t.*$", "", issues)

    urls <- paste0 (repo, "/issues/", issue_nums)

    paste0 ("- [ ] ", issue_nums, ": [", issue_titles, "](", urls, ")")
}

# Takes checklist from Dev Guide, and inserts checklist of current issues at the
# top.
make_checklist <- function (repo, version) {

    u <- "https://raw.githubusercontent.com/ropensci/dev_guide/HEAD/templates/book-release.md"
    f <- file.path (tempdir (), basename (u))
    utils::download.file (u, f, quiet = TRUE)
    md <- readLines (f)
    md [1] <- gsub ("<insert version>", version, md [1])

    md <- c (md [1:2],
             paste0 ("### Current issues assigned to @",
                     get_git_user (),
                     " or v",
                     version,
                     " milestone"),
             "",
             get_issues (repo, version),
             "",
             md [seq (3, length (md))])

    return (md)
}

post_issue <- function (repo, post = TRUE) {

    version <- get_version ()
    checklist <- make_checklist (repo, version)
    f <- tempfile (fileext = ".md")
    writeLines (checklist, f)

    issue_title <- paste0 ("'Version ", version, " pre-release checklist'")

    args <- list (
        "issue",
        "create",
        "--repo", repo,
        "--body-file", f,
        "--assignee", "@me",
        "--milestone", version,
        "--title", issue_title
    )

    if (post) {
        system2 ("gh", args = args, stdout = TRUE)
    }

    return (checklist)
}
