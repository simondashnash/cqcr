## Query construction function to reduce code-reuse

cqc_query_construction <- function(query) {
  subscription_key <- getOption("cqc.subscription.key")

  if (is.null(subscription_key)) {
    stop("CQC Subscription Key not set. The API will reject your request")
  } else if (!grepl("&", query)) {
    #do nothing
  }

  query2 <- paste0(
    baseurl, query
  )



  # Make the GET request
  x <- httr::GET(query2, add_headers(`Ocp-Apim-Subscription-Key` = subscription_key))


  if (httr::status_code(x) != "200") {
      # Log the error message and URL or any other useful information
      error_message <- paste("Request returned error code:", httr::status_code(x))
      request_url <- query2  # or use x$url if available

      # If the cqcr_errors data frame does not exist, create it
      if (!exists("cqcr_errors")) {
        cqcr_errors <- data.frame(
          error_message = character(),
          url = character(),
          stringsAsFactors = FALSE
        )
      }

      # Append the error message and URL to the cqcr_errors data frame
      cqcr_errors <- rbind(cqcr_errors, data.frame(
        error_message = error_message,
        url = request_url,
        stringsAsFactors = FALSE
      ))

    }
    else {
      # Proceed with fromJSON only if the request was successful
      suppressMessages(cont <- jsonlite::fromJSON(httr::content(x, "text")))

      # Return the parsed content
      cont
    }
}

