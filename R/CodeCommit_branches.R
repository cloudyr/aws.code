#' @rdname branch
#' @title CodeCommit Branches
#' @description Get, create, delete CodeCommit repo branches
#' @template branch
#' @template repo
#' @template commit
#' @param token A pagination token.
#' @template dots
#' @return A list.
#' @export
get_branch <- function(branch, repo, token, ...) {

    if (missing(branch)) {
        # ListBranches
        if (missing(repo)) {
            stop("'repo' is needed to list branches")
        }
        b <- list(repositoryName = get_reponame(repo))
        if (!missing(token)) {
            b$nextToken <- token
        }
        awscodeHTTP(action = "ListBranches", body = b, service = "CodeCommit", ...)
    } else {
        if (inherits(branch, "aws_repo_branch") || length(branch) == 1) {
            # GetBranch
            branch <- get_branchname(branch)
            if (missing(repo)) {
                repo <- get_reponame(branch)
            } else {
                repo <- get_reponame(repo)
            }
            awscodeHTTP(action = "GetBranch", body = list(repositoryName = repo, branchName = branch), service = "CodeCommit", ...)
        } else {
            stop("Can only specify one 'branch' at a time")
        }
    }
}

#' @rdname branch
#' @export
create_branch <- function(branch, commit, repo, ...) {
    # CreateBranch
    branch <- get_branchname(branch)
    if (missing(repo)) {
        repo <- get_reponame(branch)
    } else {
        repo <- get_reponame(repo)
    }
    if (missing(commit)) {
        commit <- get_commitid(branch)
    } else {
        commit <- get_commitid(commit)
    }
    b <- list(repositoryName = repo, branchName = branch, commitId = commit)
    awscodeHTTP(action = "GetBranch", body = b, service = "CodeCommit", ...)
}

#' @rdname branch
#' @export
get_default_branch <- function(repo, branch, ...) {
    out <- get_repo(repo, ...)
    get_branchname(repo)
}

#' @rdname branch
#' @export
set_default_branch <- function(repo, branch, ...) {
    b <- list(repositoryName = get_reponame(repo), 
              defaultBranchName = get_branchname(branch)) 
    awscodeHTTP(action = "UpdateDefaultBranch", body = b, service = "CodeCommit", ...)
}

