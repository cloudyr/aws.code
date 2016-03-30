#' @rdname triggers
#' @title CodeCommit Repo Triggers
#' @description Get, set, and test CodeCommit repo triggers
#' @template repo
#' @param triggers A list of objects of class \dQuote{aws_repo_trigger}, as created by \code{make_trigger}.
#' @template branch
#' @param name Optionally, a character string specifying the name for the trigger.
#' @param arn A character string specifying an Amazon Resource Name (ARN), such as an SNS topic.
#' @param events Optionally, a character vector specifying one or more events of: \dQuote{all} (the default) or any of \dQuote{updateReference} (commits are pushed), \dQuote{createReference} (new branch or tag is created), or \dQuote{deleteReference} (branch or tag is deleted).
#' @param custom_data Optionally, a character string specifying additional information to be passed with the trigger notice.
#' @template dots
#' @return A list.
#' @export
get_repo_triggers <- function(repo, ...) {
    # GetRepositoryTriggers
    repo <- get_reponame(repo)
    awscodeHTTP(action = "GetRepositoryTriggers", body = list(repositoryName = repo), service = "CodeCommit", ...)
}

#' @rdname triggers
#' @export
set_repo_triggers <- function(repo, triggers = list(), ...) {
    # PutRepositoryTriggers
    repo <- get_reponame(repo)
    if (!inherits(triggers, "aws_repo_trigger")) {
        if (!any(sapply(triggers, class) %in% "aws_repo_trigger")) {
            stop("'triggers' must be a list of objects of class 'aws_repo_trigger'. See ? make_trigger().")
        } 
    } else {
        triggers <- list(triggers = triggers)
    }
    b <- list(repositoryName = repo, triggers = triggers)
    awscodeHTTP(action = "PutRepositoryTriggers", body = b, service = "CodeCommit", ...)
}

#' @rdname triggers
#' @export
make_trigger <- function(branch, name, arn, events = "all", custom_data) {
    out <- list()
    if (!missing(branch)) {
        if (inherits(branch, "aws_repo_branch")) {
            out$branch <- get_branchname(branch)
        } else if (is.character(branch)) {
            out$branch <- branch
        } else {
            branch <- unlist(lapply(branch, get_branchname))
        }
    }
    if (!missing(custom_data)) {
        out$customData <- custom_data
    }
    if (!missing(arn)) {
        if (inherits(arn, "aws_sns_topic")) {
            arn <- arn[["TopicArn"]]
        }
        out$destinationArn <- arn
    }
    if (!missing(events)) {
        out$events <- events
    }
    if (!missing(name)) {
        out$name <- name
    }
    structure(out, class = "aws_repo_trigger")
}
