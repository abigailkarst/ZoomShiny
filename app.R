
library(shiny)
source("00-Functions.R")

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Uploading Files"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose vtt File",
                multiple = FALSE,
                accept = c("text/vtt",
                           ".vtt")),

      # Horizontal line ----
      tags$hr(),

    #Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")

     ),

    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("totaltimeplot"),
    
      # Output: Data file ----
      tableOutput("contents")
    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$totaltimeplot <- renderPlot({
    #req(input$file1)
    TotalSpokenPlot(input$file1$datapath)
  })

  output$contents <- renderTable({
    
    #req(input$file1)
    
    DF <- zoomVTTtoDF(input$file1$datapath)
    
    if(input$disp == "head") {
      return(head(DF))
    }
    else {
      return(DF)
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
