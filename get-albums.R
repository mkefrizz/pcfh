# Load required libraries
library(httr)
library(jsonlite)

# Your Discogs API personal token (replace with your actual token)
discogs_token <- "YOUR_DISCOGS_TOKEN"

# Function to get albums for a specific year
get_albums_by_year <- function(year, page = 1, per_page = 100) {
  # Build the Discogs API URL
  url <- paste0("https://api.discogs.com/database/search?type=release&year=", year, 
                "&format=album&page=", page, "&per_page=", per_page)
  
  # Make the GET request with the Discogs API token
  response <- GET(url, add_headers(Authorization = paste("Discogs token", discogs_token)))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the response as JSON
    content <- content(response, as = "text")
    data <- fromJSON(content)
    
    # Extract artist and album information
    if (!is.null(data$results)) {
      albums <- data$results[, c("title", "year", "artist", "id")]
      return(albums)
    }
  } else {
    warning(paste("Failed to retrieve data for year", year))
    return(NULL)
  }
}

# Function to get albums for all years from 1994 to 2023
get_albums_for_all_years <- function(start_year = 1994, end_year = 2023) {
  all_albums <- data.frame()
  
  # Loop over the years
  for (year in start_year:end_year) {
    message(paste("Fetching albums for year:", year))
    
    page <- 1
    repeat {
      # Get albums for the current year and page
      albums <- get_albums_by_year(year, page)
      
      # If there are no more results, break the loop
      if (is.null(albums) || nrow(albums) == 0) {
        break
      }
      
      # Append the albums to the all_albums data frame
      all_albums <- rbind(all_albums, albums)
      
      # Go to the next page
      page <- page + 1
    }
  }
  
  return(all_albums)
}

# Fetch albums from 1994 to 2023
albums_data <- get_albums_for_all_years(1994, 2023)

# Display the first few rows of the data
head(albums_data)

# Optionally, write the data to a CSV file
write.csv(albums_data, "discogs_albums_1994_2023.csv", row.names = FALSE)
