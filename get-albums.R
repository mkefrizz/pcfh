# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)

# Discogs API credentials and settings
discogs_api_token <- "YOUR_DISCOGS_API_TOKEN" # Replace with your actual Discogs API token
base_url <- "https://api.discogs.com/database/search"

# Function to handle rate limits
handle_rate_limit <- function(rate_limit_remaining, reset_time) {
  if (rate_limit_remaining <= 0) {
    wait_time <- as.numeric(reset_time) - as.numeric(Sys.time())
    if (wait_time > 0) {
      message("Rate limit reached, waiting for ", round(wait_time, 2), " seconds.")
      Sys.sleep(wait_time)
    }
  }
}

# Function to get albums from Discogs by year
get_albums_by_year <- function(year, page = 1) {
  # Build the query parameters
  query_params <- list(
    year = year,
    format = "album",
    type = "release",
    token = discogs_api_token,
    per_page = 100,
    page = page
  )
  
  # Make the API request
  response <- GET(url = base_url, query = query_params)
  
  # Parse the response headers for rate limits
  rate_limit_remaining <- as.numeric(headers(response)["x-discogs-ratelimit-remaining"])
  reset_time <- as.POSIXct(headers(response)["x-discogs-ratelimit-reset"], origin = "1970-01-01")
  
  # Handle rate limits
  handle_rate_limit(rate_limit_remaining, reset_time)
  
  # Parse the response body
  content <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(content, flatten = TRUE)
  
  return(data)
}

# Function to collect all albums from 1994 to 2023
get_all_albums <- function(start_year = 1994, end_year = 2023) {
  all_albums <- data.frame()
  
  for (year in start_year:end_year) {
    page <- 1
    repeat {
      message("Fetching albums for year: ", year, " (Page: ", page, ")")
      albums_data <- get_albums_by_year(year, page)
      
      # Extract relevant information (artist, album title, year, etc.)
      if (!is.null(albums_data$results)) {
        albums <- albums_data$results %>%
          select(artist = `artist`, album_title = `title`, year = `year`)
        
        all_albums <- bind_rows(all_albums, albums)
      }
      
      # Break the loop if all pages are fetched
      if (is.null(albums_data$pagination) || page >= albums_data$pagination$pages) {
        break
      } else {
        page <- page + 1
      }
      
      # Delay to respect rate limits (60 requests per minute for authenticated)
      Sys.sleep(1)
    }
  }
  
  return(all_albums)
}

# Get all albums from 1994 to 2023
all_albums <- get_all_albums(1994, 2023)

# Save the dataset as a CSV file
write.csv(all_albums, "discogs_albums_1994_2023.csv", row.names = FALSE)

# Print the number of albums retrieved
message("Total number of albums retrieved: ", nrow(all_albums))
