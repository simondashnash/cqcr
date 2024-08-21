## Query construction function to reduce code-reuse

cqc_query_construction <- function(query) {
  subscription_key <- getOption("cqc.subscription.key")

  if (is.null(subscription_key)) {
    stop("CQC Subscription Key not set. The API will reject your request")
  } else if (!grepl("&", query)) {
    print("we were trying to put a spare ? here")
    #query <- paste0(query, "?")
  }

  query2 <- paste0(
    baseurl, query
  )


  x <- httr::GET(query2, add_headers(`Ocp-Apim-Subscription-Key` = subscription_key))

  if (httr::status_code(x) != "200") {
    stop(paste(
      "Request returned error code:",
      httr::status_code(x)
    ), call. = FALSE)
  }

  suppressMessages(cont <- jsonlite::fromJSON(httr::content(x, "text")))

  cont
}

# cqc_query_construction <- function(query) {
#   partner_code <- getOption("cqc.partner.code")

#   if (is.null(partner_code)) {
#     message("CQC Partner Code not set.")
#   } else if (!grepl("&", query)) {
#     query <- paste0(query, "?")
#   }

#   query2 <- paste0(
#     baseurl, query, partner_code
#   )

#   query2 <- gsub("?&partnerCode=", "?partnerCode=", query2, fixed = TRUE)

#   x <- httr::GET(query2)

#   if (httr::status_code(x) != "200") {
#     stop(paste(
#       "Request returned error code:",
#       httr::status_code(x)
#     ), call. = FALSE)
#   }

#   suppressMessages(cont <- jsonlite::fromJSON(httr::content(x, "text")))

#   cont
# }
