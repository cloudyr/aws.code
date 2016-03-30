get_branchname <- function(x) {
    if (inherits(x, "aws_repo")) {
        x[["defaultBranch"]]
    } else if (inherits(x, "aws_repo_branch")) {
        x[["branchName"]]
    } else {
        x
    }
}

get_reponame <- function(x) {
    if (inherits(x, "aws_repo_commit")) {
        x[["repositoryName"]]
    } else if (inherits(x, "aws_repo_branch")) {
        x[["repositoryName"]]
    } else if (inherits(x, "aws_repo_commit")) {
        x[["repositoryName"]]
    } else {
        x
    }
}

get_commitid <- function(x) {
    if (inherits(x, "aws_repo_commit")) {
        x[["commitId"]]
    } else if (inherits(x, "aws_repo_branch")) {
        x[["commitId"]]
    } else {
        x
    }
}
