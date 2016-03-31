#' @rdname repo
#' @title CodeCommit Repositories
#' @description Get, create, update, delete CodeCommit repositories
#' @template repo
#' @param name A character string specifying a repository name.
#' @param description A character string specifying a repository description.
#' @template repo 
#' @param sort One of \dQuote{repositoryName} or \dQuote{lastModifiedDate}.
#' @param order One of \dQuote{ascending} or \dQuote{descending}.
#' @param token A pagination token.
#' @template dots
#' @return A list.
#' @export
get_repo <- function(repo, sort, order, token, ...) {

    if (missing(repo)) {
        # ListRepositories
        awscodeHTTP(action = "ListRepositories", body = list(repositoryName = repo), service = "CodeCommit", ...)
    } else {
        if (inherits(repo, "aws_repo") || length(repo) == 1) {
            # GetRepository
            repo <- get_reponame(repo)
            awscodeHTTP(action = "GetRepository", body = list(repositoryName = repo), service = "CodeCommit", ...)
        } else {
            # BatchGetRepositories
            repo <- lapply(repo, get_reponame)
            awscodeHTTP(action = "BatchGetRepositories", body = list(repositoryName = repo), service = "CodeCommit", ...)
        }
    }
}


#' @rdname repo
#' @export
create_repo <- function(name, description, ...) {

    # CreateRepository
    
    if (nchar(name) < 1 || nchar(name) > 100) {
        stop("'name' mut be between 1 and 100 charaters")
    }
    if (nchar(description) > 1000) {
        stop("'description' mut be between 0 and 1000 charaters")
    }
    
    b <- list(repositoryName = name,
              repositoryDescription = description)
    awscodeHTTP(action = "CreateRepository", body = b, service = "CodeCommit", ...)
}

#' @rdname repo
#' @export
update_repo <- function(repo, name, description, ...) {
    oldname <- get_reponame(repo)
    if (!missing(name)) {
        # UpdateRepositoryName
        if (nchar(name) < 1 || nchar(name) > 100) {
            stop("'name' mut be between 1 and 100 charaters")
        }
        awscodeHTTP(action = "UpdateRepositoryName", body = list(oldName = oldname, newName = name), service = "CodeCommit", ...)
    } else if (!missing(description)) {
        # UpdateRepositoryDescription
        if (nchar(description) > 1000) {
            stop("'description' mut be between 0 and 1000 charaters")
        }
        awscodeHTTP(action = "UpdateRepositoryDescription", body = list(oldName = oldname, newName = name), service = "CodeCommit", ...)
    } else {
        stop("Must specify 'name' xor 'description'")
    }
}

#' @rdname repo
#' @export
delete_repo <- function(repo, ...) {
    # DeleteRepository
    repo <- get_reponame(repo)
    b <- list(repositoryName = repo)
    awscodeHTTP(action = "DeleteRepository", body = b, service = "CodeCommit", ...)
}
