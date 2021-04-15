library(shiny)
library(tidyverse)
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
      plotOutput("exampleplot"),
    
      # Output: Data file ----
      tableOutput("contents")
    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  ## Make reactive here
  
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
  
  output$exampleplot <- renderPlot({
    
    zoomVTTtoDF(input$file1$datapath) %>%
      mutate(length_nested = list(1:max(end_seconds))) %>% 
      unnest(length_nested) %>% 
      mutate(spoken_second = length_nested > start_seconds &
               length_nested < end_seconds) %>% 
      filter(spoken_second) %>% 
      
      ggplot(aes(length_nested, speaker, fill = speaker)) +
      geom_tile() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "Seconds Speaking",
           y = "Speaker")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
