library(shiny)
library(RMySQL)

# Sample choices for dropdowns
choices <- c("Option 1", "Option 2", "Option 3", "Option 4", "Option 5")

# Database connection details
db_host <- "your-database-host.mysql.database.azure.com"
db_user <- "your-username@your-database-host"
db_password <- "your-password"
db_name <- "your-database-name"

# Define UI
ui <- fluidPage(
  titlePanel("Survey"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("row1_col1", "Row 1, Column 1:", choices = choices, options = list(create = TRUE)),
      selectizeInput("row1_col2", "Row 1, Column 2:", choices = choices, options = list(create = TRUE)),
      selectizeInput("row1_col3", "Row 1, Column 3:", choices = choices, options = list(create = TRUE)),
      selectizeInput("row2_col1", "Row 2, Column 1:", choices = choices, options = list(create = TRUE)),
      selectizeInput("row2_col2", "Row 2, Column 2:", choices = choices, options = list(create = TRUE)),
      selectizeInput("row2_col3", "Row 2, Column 3:", choices = choices, options = list(create = TRUE)),
      selectizeInput("row3_col1", "Row 3, Column 1:", choices = choices, options = list(create = TRUE)),
      selectizeInput("row3_col2", "Row 3, Column 2:", choices = choices, options = list(create = TRUE)),
      selectizeInput("row3_col3", "Row 3, Column 3:", choices = choices, options = list(create = TRUE)),
      actionButton("nextPage", "Next Page")
    ),
    mainPanel(
      textOutput("pageNumber")
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  page <- reactiveVal(1)

  observeEvent(input$nextPage, {
    new_data <- data.frame(
      Row1_Col1 = input$row1_col1,
      Row1_Col2 = input$row1_col2,
      Row1_Col3 = input$row1_col3,
      Row2_Col1 = input$row2_col1,
      Row2_Col2 = input$row2_col2,
      Row2_Col3 = input$row2_col3,
      Row3_Col1 = input$row3_col1,
      Row3_Col2 = input$row3_col2,
      Row3_Col3 = input$row3_col3
    )

    # Connect to the database
    conn <- dbConnect(MySQL(),
                      dbname = db_name,
                      host = db_host,
                      user = db_user,
                      password = db_password)

    # Insert data into the database
    dbWriteTable(conn, "survey_data", new_data, append = TRUE, row.names = FALSE)

    # Disconnect from the database
    dbDisconnect(conn)

    if (page() < 30) {
      page(page() + 1)
    } else {
      stopApp("Survey Complete!")
    }
  })

  output$pageNumber <- renderText({
    paste("Page", page(), "of 30")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
