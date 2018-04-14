#' @title API Requests
#' @description CodeCommit/CodeDeploy/CodePipeline HTTP Requests
#' @param action A character string specifying an API action.
#' @param query A named list of query string parameters.
#' @param headers A list of headers to pass to the HTTP request.
#' @param body A list of body (JSON) arguments.
#' @param service A character string specifying one of \dQuote{CodeCommit}, \dQuote{CodeDeploy}, or \dQuote{CodePipeline}.
#' @param verbose A logical indicating whether to be verbose. Default is given by \code{options("verbose")}.
#' @param region A character string containing the AWS region. If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. See \code{\link[aws.signature]{locate_credentials}}.
#' @param secret A character string containing an AWS Secret Access Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param session_token A character string containing an AWS Session Token. See \code{\link[aws.signature]{locate_credentials}}.
#' @param version A character string specifying an API version. Default is \dQuote{2015-04-13}.
#' @param \dots Additional arguments passed to \code{\link[httr]{POST}}.
#' @return A list
#' @importFrom aws.signature signature_v4_auth
#' @importFrom httr add_headers headers content warn_for_status http_status http_error POST
#' @importFrom xml2 read_xml as_list
#' @export
awscodeHTTP <- 
function(
  action,
  query = list(), 
  headers = list(),
  body = list(),
  service = c("CodeCommit", "CodeDeploy", "CodePipeline"), 
  verbose = getOption("verbose", FALSE),
  region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
  key = NULL,
  secret = NULL,
  session_token = NULL,
  version = "2015-04-13",
  ...
) {
    # locate and validate credentials
    credentials <- locate_credentials(key = key, secret = secret, session_token = session_token, region = region, verbose = verbose)
    key <- credentials[["key"]]
    secret <- credentials[["secret"]]
    session_token <- credentials[["session_token"]]
    region <- credentials[["region"]]
    
    # generate request signature
    service2 <- tolower(match.arg(service))
    query$Version <- version
    if (region == "us-east-1") {
        url <- paste0("https://",service2,".amazonaws.com")
    } else {
        url <- paste0("https://",service2,".",region,".amazonaws.com")
    }
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    Sig <- signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = service,
           verb = "POST",
           action = "/",
           query_args = query,
           canonical_headers = list(host = if (region == "us-east-1") {
                                             paste0(service2,".amazonaws.com") 
                                           } else {
                                             paste0(service2,".",region,".amazonaws.com")
                                           },
                                    `X-Amz-Date` = d_timestamp,
                                    `X-Amz-Target` = paste0(service, "_", gsub("-", "", version), action)),
           request_body = "",
           key = key,
           secret = secret,
           session_token = session_token,
           verbose = verbose)
    # setup request headers
    headers[["X-Amz-Date"]] <- d_timestamp
    headers[["X-Amz-Target"]] <- paste0(service, "_", gsub("-", "", version), action)
    headers[["Authorization"]] <- Sig[["SignatureHeader"]]
    if (!is.null(session_token) && session_token != "") {
        headers[["x-amz-security-token"]] <- session_token
    }
    H <- do.call(add_headers, headers)
    
    # execute request
    if (length(query)) {
        r <- POST(url, H, body = body, encode = "json", query = query, ...)
    } else {
        r <- POST(url, H, body = body, encode = "json", ...)
    }
    if (http_error(r)) {
        tmp <- gsub("\n\\s*", "", content(r, "text"))
        x <- try(as_list(read_xml(tmp)), silent = TRUE)
        stop(paste0(parse_errors(x), collapse = "\n"))
    } else {
        tmp <- gsub("\n\\s*", "", content(r, "text"))
        out <- try(as_list(read_xml(tmp)), silent = TRUE)
        if (inherits(out, "try-error")) {
            out <- structure(content(r, "text"))
        }
    }
    return(out)
}

parse_errors <- function(error) {
    unname(unlist(lapply(error$Errors, function(z) {
        paste0(z$Code[[1]], ": ", z$Message[[1]], "\n")
    })))
}
