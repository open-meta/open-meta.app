# email.aws.R
# code from aws.ses package, modified by TomW Nov 2018

# embedded in email-core for now; don't load this file...

send_email <- function(message,
                  html,
                  subject,
                  from,
                  to = NULL,
                  cc = NULL,
                  bcc = NULL,
                  replyto = NULL,
                  charset.subject = "UTF-8",
                  charset.message = "UTF-8",
                  charset.html = "UTF-8",
                  key = SESkey,
                  secret = SESsecret,
                  region = SESregion,
                  ...) {

    query <- list(Source = from)

    # configure message body and subject
    if (!is.null(raw)) {
        query[["Action"]] <- "SendRawEmail"
        query[["RawMessage"]] <- list(Data = raw)
    } else {
        query[["Action"]] <- "SendEmail"
        if (missing(message) & missing(html)) {
            stop("Must specify 'message', 'html', or both of them.")
        }
        if (!missing(message)) {
            query[["Message.Body.Text.Data"]] <- message
            if (!is.null(charset.message)) {
                query[["Message.Message.Charset"]] <- charset.message
            }
        }
        if (!missing(html)) {
            query[["Message.Body.Html.Data"]] <- html
            if (!is.null(charset.html)) {
                query[["Message.Body.Html.Charset"]] <- charset.html
            }
        }
        query[["Message.Subject.Data"]] <- subject
        if (!is.null(charset.subject)) {
            query[["Message.Subject.Charset"]] <- charset.subject
        }
    }

    # configure recipients
    if (length(c(to,cc,bcc)) > 50L) {
        stop("The total number of recipients cannot exceed 50.")
    }
    if (!is.null(to)) {
        names(to) <- paste0("Destination.ToAddresses.member.", seq_along(to))
        query <- c(query, to)
    }
    if (!is.null(cc)) {
        names(cc) <- paste0("Destination.CcAddresses.member.", seq_along(cc))
        query <- c(query, cc)
    }
    if (!is.null(bcc)) {
        names(bcc) <- paste0("Destination.BccAddresses.member.", seq_along(bcc))
        query <- c(query, bcc)
    }
    if (!is.null(replyto)) {
        names(replyto) <- paste0("ReplyToAddresses.member.", seq_along(replyto))
        query <- c(query, replyto)
    }
    # if (!is.null(returnpath)) {
    #   query[["ReturnPath"]] <- returnpath
    # }
    r <- sesPOST(body = query,
                 key = key,
                 secret = secret,
                 region = region,
                 ...)
    return(r)
}

sesPOST <- function(
              query = list(),
              headers = list(),
              body = NULL,
              verbose = getOption("verbose", FALSE),
              region = NULL,
              key = NULL,
              secret = NULL,
              ...
            ) {

    # generate request signature
    uri <- paste0("https://email.",region,".amazonaws.com")
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    body_to_sign <- if (is.null(body)) {
        ""
    } else {
        paste0(names(body), "=", sapply(unname(body), utils::URLencode, reserved = TRUE), collapse = "&")
    }
    Sig <- aws.signature::signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = "email",
           verb = "POST",
           action = "/",
           query_args = query,
           canonical_headers = list(host = paste0("email.",region,".amazonaws.com"),
                                    `x-amz-date` = d_timestamp),
           request_body = body_to_sign,
           key = key,
           secret = secret,
 #          session_token = session_token,
           verbose = verbose)
    # setup request headers
    headers[["x-amz-date"]] <- d_timestamp
    headers[["x-amz-content-sha256"]] <- Sig$BodyHash
    headers[["Authorization"]] <- Sig[["SignatureHeader"]]
    # if (!is.null(session_token) && session_token != "") {
    #     headers[["x-amz-security-token"]] <- session_token
    # }
    H <- do.call(httr::add_headers, headers)

    # execute request
    if (length(query)) {
        if (!is.null(body)) {
            r <- httr::POST(uri, H, query = query, body = body, encode = "form", ...)
        } else {
            r <- httr::POST(uri, H, query = query, ...)
        }
    } else {
        if (!is.null(body)) {
            r <- httr::POST(uri, H, body = body, encode = "form", ...)
        } else {
            r <- httr::POST(uri, H, ...)
        }
    }

    return(httr::http_status(r$status_code)$message)
}



