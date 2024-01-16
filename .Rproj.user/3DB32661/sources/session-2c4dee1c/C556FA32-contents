# Load specific datasets from ggplot2
library(shiny)
library(ggplot2)
# Known datasets from ggplot2
ggplot2_datasets <- c("mpg", "diamonds")

ui <- fluidPage(
  titlePanel("Dynamic Dataset Viewer for ggplot2"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Dataset:", choices = ggplot2_datasets),
      selectInput("columns", "Select Columns:", choices = NULL, multiple = TRUE),
      numericInput("rows", "Number of Rows to Show:", value = 10, min = 1),
      downloadButton("downloadRCode", "Download R Code")
    ),
    mainPanel(
      h2("Data Viewer"),  # Header for Data Viewer
      tableOutput("data"),
      h2("R Code"),  # Header for R Code
      verbatimTextOutput("rcode")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    selected_dataset_name <- input$dataset
    if (selected_dataset_name != "") {
      tryCatch({
        # Directly use the dataset name as a symbol to get the dataset
        selected_dataset <- get(selected_dataset_name, envir = asNamespace('ggplot2'))
        
        if(is.data.frame(selected_dataset)) {
          updateSelectInput(session, "columns", choices = names(selected_dataset), selected = character(0))
          updateNumericInput(session, "rows", max = nrow(selected_dataset))
        } else {
          print(paste(selected_dataset_name, "is not a dataframe."))
        }
      }, error = function(e) {
        print(paste("Error loading dataset:", selected_dataset_name, "; Error:", e$message))
      })
    } else {
      updateSelectInput(session, "columns", choices = NULL, selected = character(0))
    }
  })
  
  # Subset the data based on selected columns and row count
  filtered_data <- reactive({
    req(input$dataset, input$columns)
    selected_dataset <- get(input$dataset, 'package:ggplot2')
    selected_columns <- input$columns
    dataset_subset <- selected_dataset[, selected_columns, drop = FALSE]
    head(dataset_subset, as.numeric(input$rows))
  })
  
  # Render the filtered data
  output$data <- renderTable({
    filtered_data()
  })
  
  # Create R code to reproduce the operation
  output$rcode <- renderText({
    req(input$dataset, input$columns, input$rows)
    selected_columns <- paste0("'", input$columns, "'")
    r_code <- paste(input$dataset, "_subset <-", input$dataset, "[, c(", paste(selected_columns, collapse = ", "), "), drop = FALSE]",
                    "\nhead(", input$dataset, "_subset, ", input$rows, ")")
    return(r_code)
  })
  
  # Download R code
  output$downloadRCode <- downloadHandler(
    filename = function() {
      paste(input$dataset, "_subset_code_", Sys.Date(), ".R", sep = "")
    },
    content = function(file) {
      writeLines(output$rcode(), con = file)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
