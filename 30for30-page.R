# Load required libraries
library(shiny)

# Create a sample master data frame of artists and albums
master_data <- data.frame(
  Artist = c("Radiohead", "Radiohead", "Nirvana", "Madonna", "Madonna", "The Beatles", "Pink Floyd"),
  Album = c("OK Computer", "The Bends", "Nevermind", "Ray of Light", "Like a Prayer", "Abbey Road", "The Wall"),
  Year = c(1997, 1995, 1991, 1998, 1989, 1969, 1979),
  stringsAsFactors = FALSE
)

# Define UI for the survey form
ui <- fluidPage(
  titlePanel("Music Survey (1994 - 2003)"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Fill out the form for each year (1994-2003)"),
      p("For each year, enter the Artist, Album, and Points."),
      p("Select an artist to filter albums by that artist."),
      p("You can also add new Artists or Albums to the list if needed."),
      actionButton("previous", "Previous"),
      actionButton("next", "Next"),
      actionButton("submit", "Submit Survey")
    ),
    
    mainPanel(
      uiOutput("formUI")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to hold the updated master list
  master_data_reactive <- reactiveVal(master_data)
  
  # Keep track of which page (3-year block) we're on
  page <- reactiveVal(1)
  
  # Define which years to show on each page
  year_groups <- list(
    c(1994, 1995, 1996),
    c(1997, 1998, 1999),
    c(2000, 2001, 2002, 2003)
  )
  
  # Function to update albums based on selected artist
  observe({
    current_data <- master_data_reactive()
    for (year in year_groups[[page()]]) {
      for (i in 1:3) {
        artist_input <- paste0("artist_", year, "_", i)
        album_input <- paste0("album_", year, "_", i)
        
        observeEvent(input[[artist_input]], {
          selected_artist <- input[[artist_input]]
          available_albums <- current_data$Album[current_data$Artist == selected_artist & current_data$Year == year]
          
          # If no albums are found, allow the user to add new albums
          if (length(available_albums) == 0) {
            available_albums <- c(available_albums, "Add new album")
          }
          
          updateSelectizeInput(session, album_input, choices = available_albums)
        })
      }
    }
  })
  
  # Create UI for form based on the current page (3-year block)
  output$formUI <- renderUI({
    current_data <- master_data_reactive()
    
    # Generate form for each year in the current block
    lapply(year_groups[[page()]], function(year) {
      fluidRow(
        h4(paste("Year:", year)),
        lapply(1:3, function(i) {
          fluidRow(
            column(3, selectizeInput(paste0("artist_", year, "_", i), 
                                     label = paste0("Artist ", i, " (", year, ")"),
                                     choices = c(current_data$Artist, "Add new artist"),
                                     options = list(create = TRUE))),
            column(3, selectizeInput(paste0("album_", year, "_", i), 
                                     label = paste0("Album ", i, " (", year, ")"),
                                     choices = NULL, 
                                     options = list(create = TRUE))),
            column(3, numericInput(paste0("points_", year, "_", i), 
                                   label = paste0("Points ", i, " (", year, ")"), 
                                   value = 0, min = 0, max = 10))
          )
        })
      )
    })
  })
  
  # Function to move to the next or previous page
  observeEvent(input$next, {
    if (page() < length(year_groups)) {
      page(page() + 1)
    }
  })
  
  observeEvent(input$previous, {
    if (page() > 1) {
      page(page() - 1)
    }
  })
  
  # Function to check if new artist/album was added and update master list
  update_master_list <- function(new_artist, new_album, year) {
    current_data <- master_data_reactive()
    
    # Check if the new artist or album is already in the master data
    if (!(new_artist %in% current_data$Artist & new_album %in% current_data$Album & year %in% current_data$Year)) {
      # Add new entry to the master data
      new_entry <- data.frame(Artist = new_artist, Album = new_album, Year = year, stringsAsFactors = FALSE)
      updated_data <- rbind(current_data, new_entry)
      master_data_reactive(updated_data)
    }
  }
  
  # Observe the submit button click
  observeEvent(input$submit, {
    responses <- list()
    
    # Collect responses for each year and row
    for (year_group in year_groups) {
      for (year in year_group) {
        for (i in 1:3) {
          artist <- input[[paste0("artist_", year, "_", i)]]
          album <- input[[paste0("album_", year, "_", i)]]
          points <- input[[paste0("points_", year, "_", i)]]
          
          # Update master list if new artist or album is added
          update_master_list(artist, album, year)
          
          # Append responses to list
          responses <- rbind(
            responses, 
            data.frame(Year = year, Artist = artist, Album = album, Points = points, stringsAsFactors = FALSE)
          )
        }
      }
    }
    
    # Print the collected responses in console (or handle in any other way)
    print(responses)
    
    # Optionally, write updated master data to a file or other storage
    print("Updated master list:")
    print(master_data_reactive())
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
