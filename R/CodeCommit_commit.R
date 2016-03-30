#' @rdname commit
#' @title CodeCommit Commits
#' @description Get CodeCommit repo commit
#' @template commit 
#' @template repo
#' @template dots
#' @return A list.
#' @export
get_commit <- function(commit, repo, ...) {
    # GetCommit
    if (missing(repo)) {
        repo <- get_reponame(commit)
    } else {
        repo <- get_reponame(repo)
    }
    b <- list(repositoryName = repo, commitId = get_commitid(commit))
    awscodeHTTP(action = "GetCommit", body = b, service = "CodeCommit", ...)
}
