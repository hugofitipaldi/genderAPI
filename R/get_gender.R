#' Function to query Gender API
#'
#' This function uses Gender API to predict gender based on first name and country if desired
#'
#' @param name desired given name as a string
#' @param api_key your pai key as a string
#' @param country_code optional country code (e.g. "US")
#' @param last_name optional persons last name
#' @return a data table with prediction of gender
#' @export

get_gender <- function(name, api_key, country_code = NA, last_name = NA){

  # Construct the query
  path <- paste0("https://gender-api.com/get?key=", api_key)

  if (is.na(last_name)) {
    if (is.na(country_code)) {
      request <- httr::GET(url = path,
                           query = list(
                             name = name))
    } else {
      request <- httr::GET(url = path,
                           query = list(
                             name = name,
                             country = country_code))
    }
  } else {
    if (is.na(country_code)){
      request <- httr::GET(url = path,
                           query = list(
                             split = paste0(name, " ", last_name)))
    } else {
      request <- httr::GET(url = path,
                           query = list(
                             split = paste0(name, " ", last_name),
                             country = country_code))
    }
  }

  response <- httr::content(request, as = "text", encoding = "UTF-8")
  response_df <- data.frame(jsonlite::fromJSON(response, flatten = TRUE))

  if (is.na(last_name)) {
    print(dplyr::select(response_df,name, country, gender, accuracy))
  } else {
    print(dplyr::select(response_df,first_name, last_name, country, gender, accuracy) )
  }
}

# Enhanced Gender API R Package Functions

#' Function to query Gender API v2 by first name
#'
#' This function uses Gender API v2 to predict gender based on first name
#'
#' @param first_name desired given name as a string
#' @param api_token your API token as a string (Bearer token)
#' @param country optional ISO 3166 ALPHA-2 country code (e.g. "US")
#' @param locale optional browser locale (e.g. "en_US")
#' @param ip optional IPv4 or IPv6 address for localization
#' @param id optional alphanumeric ID (max 50 chars) for tracking requests
#' @param return_raw logical, if TRUE returns raw JSON response, if FALSE returns data frame
#' @return a data frame with gender prediction results or raw JSON if return_raw = TRUE
#' @export

get_gender_v2 <- function(first_name, api_token, country = NULL, locale = NULL, ip = NULL, id = NULL, return_raw = FALSE) {

  # Construct the request body
  body_list <- list(first_name = first_name)

  # Add optional parameters if provided
  if (!is.null(country)) body_list$country <- country
  if (!is.null(locale)) body_list$locale <- locale
  if (!is.null(ip)) body_list$ip <- ip
  if (!is.null(id)) body_list$id <- id

  # Make POST request
  response <- httr::POST(
    url = "https://gender-api.com/v2/gender",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_token)
    ),
    body = jsonlite::toJSON(body_list, auto_unbox = TRUE),
    encode = "raw"
  )

  # Check for HTTP errors
  if (httr::status_code(response) != 200) {
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop("API request failed. Status: ", httr::status_code(response), ". Response: ", content)
  }

  # Parse response
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content, flatten = TRUE)

  if (return_raw) {
    return(result)
  }

  # Create a clean data frame with the most important columns
  df_result <- data.frame(
    first_name = result$first_name,
    gender = result$gender,
    probability = result$probability,
    result_found = result$result_found,
    country = ifelse(is.null(result$details$country), NA, result$details$country),
    samples = result$details$samples,
    credits_used = result$details$credits_used,
    duration = result$details$duration,
    stringsAsFactors = FALSE
  )

  return(df_result)
}

#' Function to query Gender API v2 by full name
#'
#' This function uses Gender API v2 to predict gender and split full names
#'
#' @param full_name full name as a string (first and last name)
#' @param api_token your API token as a string (Bearer token)
#' @param country optional ISO 3166 ALPHA-2 country code (e.g. "US")
#' @param locale optional browser locale (e.g. "en_US")
#' @param ip optional IPv4 or IPv6 address for localization
#' @param id optional alphanumeric ID (max 50 chars) for tracking requests
#' @return a data frame with gender prediction and name splitting results
#' @export

get_gender_fullname <- function(full_name, api_token, country = NULL, locale = NULL, ip = NULL, id = NULL) {

  # Construct the request body
  body_list <- list(full_name = full_name)

  # Add optional parameters if provided
  if (!is.null(country)) body_list$country <- country
  if (!is.null(locale)) body_list$locale <- locale
  if (!is.null(ip)) body_list$ip <- ip
  if (!is.null(id)) body_list$id <- id

  # Make POST request
  response <- httr::POST(
    url = "https://gender-api.com/v2/gender",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_token)
    ),
    body = jsonlite::toJSON(body_list, auto_unbox = TRUE),
    encode = "raw"
  )

  # Check for HTTP errors
  if (httr::status_code(response) != 200) {
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop("API request failed. Status: ", httr::status_code(response), ". Response: ", content)
  }

  # Parse response
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content, flatten = TRUE)

  # Create a clean data frame
  df_result <- data.frame(
    first_name = result$first_name,
    last_name = ifelse(is.null(result$last_name), NA, result$last_name),
    gender = result$gender,
    probability = result$probability,
    result_found = result$result_found,
    country = ifelse(is.null(result$details$country), NA, result$details$country),
    samples = result$details$samples,
    credits_used = result$details$credits_used,
    duration = result$details$duration,
    stringsAsFactors = FALSE
  )

  return(df_result)
}

#' Function to query Gender API v2 by email address
#'
#' This function uses Gender API v2 to predict gender based on email address
#'
#' @param email email address as a string
#' @param api_token your API token as a string (Bearer token)
#' @param country optional ISO 3166 ALPHA-2 country code (e.g. "US")
#' @param locale optional browser locale (e.g. "en_US")
#' @param ip optional IPv4 or IPv6 address for localization
#' @param id optional alphanumeric ID (max 50 chars) for tracking requests
#' @return a data frame with gender prediction results
#' @export

get_gender_email <- function(email, api_token, country = NULL, locale = NULL, ip = NULL, id = NULL) {

  # Construct the request body
  body_list <- list(email = email)

  # Add optional parameters if provided
  if (!is.null(country)) body_list$country <- country
  if (!is.null(locale)) body_list$locale <- locale
  if (!is.null(ip)) body_list$ip <- ip
  if (!is.null(id)) body_list$id <- id

  # Make POST request
  response <- httr::POST(
    url = "https://gender-api.com/v2/gender",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_token)
    ),
    body = jsonlite::toJSON(body_list, auto_unbox = TRUE),
    encode = "raw"
  )

  # Check for HTTP errors
  if (httr::status_code(response) != 200) {
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop("API request failed. Status: ", httr::status_code(response), ". Response: ", content)
  }

  # Parse response
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content, flatten = TRUE)

  # Create a clean data frame
  df_result <- data.frame(
    email = result$email,
    first_name = result$first_name,
    last_name = ifelse(is.null(result$last_name), NA, result$last_name),
    gender = result$gender,
    probability = result$probability,
    result_found = result$result_found,
    country = ifelse(is.null(result$details$country), NA, result$details$country),
    samples = result$details$samples,
    credits_used = result$details$credits_used,
    duration = result$details$duration,
    stringsAsFactors = FALSE
  )

  return(df_result)
}

#' Function to get country of origin from Gender API v2
#'
#' This function uses Gender API v2 to determine country of origin for names
#'
#' @param first_name first name as a string
#' @param full_name optional full name (alternative to first_name)
#' @param email optional email address (alternative to first_name)
#' @param api_token your API token as a string (Bearer token)
#' @param id optional alphanumeric ID (max 50 chars) for tracking requests
#' @return a data frame with country of origin, ethnicity, and meaning information
#' @export

get_country_origin <- function(first_name = NULL, full_name = NULL, email = NULL, api_token, id = NULL) {

  # Validate that at least one name parameter is provided
  if (is.null(first_name) && is.null(full_name) && is.null(email)) {
    stop("At least one of first_name, full_name, or email must be provided")
  }

  # Construct the request body
  body_list <- list()
  if (!is.null(first_name)) body_list$first_name <- first_name
  if (!is.null(full_name)) body_list$full_name <- full_name
  if (!is.null(email)) body_list$email <- email
  if (!is.null(id)) body_list$id <- id

  # Make POST request
  response <- httr::POST(
    url = "https://gender-api.com/v2/country-of-origin",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_token)
    ),
    body = jsonlite::toJSON(body_list, auto_unbox = TRUE),
    encode = "raw"
  )

  # Parse response
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content, flatten = TRUE)

  # Return as data frame
  return(as.data.frame(result))
}

#' Function to get account statistics from Gender API v2
#'
#' This function retrieves account usage statistics
#'
#' @param api_token your API token as a string (Bearer token)
#' @return a data frame with account statistics
#' @export

get_api_stats <- function(api_token) {

  # Make GET request
  response <- httr::GET(
    url = "https://gender-api.com/v2/statistic",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_token)
    )
  )

  # Parse response
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content, flatten = TRUE)

  # Return as data frame
  return(as.data.frame(result))
}

#' Function to process multiple names in batch
#'
#' This function allows batch processing of multiple names
#'
#' @param names_list a list of lists, each containing name information (first_name, country, etc.)
#' @param api_token your API token as a string (Bearer token)
#' @param request_type type of request: "first_name", "full_name", or "email"
#' @return a data frame with results for all names
#' @export

get_gender_batch <- function(names_list, api_token, request_type = "first_name") {

  # Validate request type
  valid_types <- c("first_name", "full_name", "email")
  if (!request_type %in% valid_types) {
    stop("request_type must be one of: ", paste(valid_types, collapse = ", "))
  }

  # Make POST request
  response <- httr::POST(
    url = "https://gender-api.com/v2/gender",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_token)
    ),
    body = jsonlite::toJSON(names_list, auto_unbox = TRUE),
    encode = "raw"
  )

  # Parse response
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content, flatten = TRUE)

  # Return as data frame
  return(as.data.frame(result))
}

#' Helper function to create country code reference
#'
#' This function returns a data frame with all supported country codes
#'
#' @return a data frame with country codes, names, and regions
#' @export

get_country_codes <- function() {

  # This would ideally be loaded from the API or stored as package data
  # For now, returning a sample - you could expand this with the full list from the docs
  countries <- data.frame(
    country_code = c("US", "GB", "DE", "FR", "CA", "AU", "JP", "CN", "IN", "BR"),
    country_name = c("United States", "United Kingdom", "Germany", "France",
                     "Canada", "Australia", "Japan", "China", "India", "Brazil"),
    continental_region = c("Americas", "Europe", "Europe", "Europe",
                           "Americas", "Oceania", "Asia", "Asia", "Asia", "Americas"),
    statistical_region = c("Northern America", "Northern Europe", "Western Europe", "Western Europe",
                           "Northern America", "Australia and New Zealand", "Eastern Asia",
                           "Eastern Asia", "Southern Asia", "South America")
  )

  return(countries)
}

#' Helper function for error handling
#'
#' This function processes API error responses
#'
#' @param response httr response object
#' @return processed error message or NULL if no error
#' @export
handle_api_errors <- function(response) {

  if (httr::status_code(response) != 200) {
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    error_info <- jsonlite::fromJSON(content, flatten = TRUE)

    error_message <- paste("API Error:", httr::status_code(response))
    if ("status" %in% names(error_info)) {
      error_message <- paste(error_message, "-", error_info$status)
    }

    warning(error_message)
    return(error_info)
  }

  return(NULL)
}

#' Test function to check API connectivity and response structure
#'
#' This function helps debug API issues and confirms successful connection
#'
#' @param first_name name to test with
#' @param api_token your API token
#' @param verbose logical, if TRUE shows detailed debug info, if FALSE shows only success/failure
#' @return prints connection status and returns raw response if verbose = TRUE
#' @export

test_api_connection <- function(first_name, api_token, verbose = FALSE) {

  if (verbose) {
    cat("Testing API connection...\n")
    cat("First name:", first_name, "\n")
    cat("API token (first 10 chars):", substr(api_token, 1, 10), "...\n\n")
  }

  # Construct the request body
  body_list <- list(first_name = first_name)
  body_json <- jsonlite::toJSON(body_list, auto_unbox = TRUE)

  if (verbose) {
    cat("Request body:", body_json, "\n\n")
  }

  # Make POST request
  response <- httr::POST(
    url = "https://gender-api.com/v2/gender",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_token)
    ),
    body = body_json,
    encode = "raw"
  )

  status_code <- httr::status_code(response)

  if (status_code == 200) {
    cat("✓ API connection successful! Gender API is working properly.\n")

    if (verbose) {
      cat("HTTP Status Code:", status_code, "\n")
      cat("Response headers:\n")
      print(httr::headers(response))

      # Get response content
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      cat("\nRaw response content:\n")
      cat(content, "\n\n")

      # Try to parse JSON
      tryCatch({
        parsed <- jsonlite::fromJSON(content, flatten = TRUE)
        cat("Parsed JSON structure:\n")
        str(parsed)
        return(parsed)
      }, error = function(e) {
        cat("JSON parsing error:", e$message, "\n")
        return(content)
      })
    } else {
      return(invisible(TRUE))
    }
  } else {
    cat("✗ API connection failed!\n")
    cat("HTTP Status Code:", status_code, "\n")

    content <- httr::content(response, as = "text", encoding = "UTF-8")
    cat("Error response:", content, "\n")

    return(invisible(FALSE))
  }
}

