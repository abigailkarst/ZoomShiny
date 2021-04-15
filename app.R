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
  
  ## Reactive values
  re <- reactiveValues()
  re$df <- reactive({
    data_re <- zoomVTTtoDF(input$file1$datapath)
    return(data_re)
  })
  
  output$totaltimeplot <- renderPlot({
    
    #get rid of "con" error in connection warning before reactive value is made
    if (is.null(input$file1$datapath))
      return(NULL)
    
      re$df() %>%
      group_by(speaker) %>% 
      transmute(Total = sum(duration_seconds)) %>%
      distinct() %>% 
      
      ggplot(aes(x = speaker, y = Total)) + 
        ggtitle("Total Time Spoken") +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Set1") + #why is color palette not working?
        theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust = 0.5))
    
  })

  output$contents <- renderTable({
    
    if (is.null(input$file1$datapath))
      return(NULL)
    
    DF <- re$df()
    
    if(input$disp == "head") {
      return(head(DF))
    }
    else {
      return(DF)
    }
    
  })
  
  output$exampleplot <- renderPlot({
    
    if (is.null(input$file1$datapath))
      return(NULL)
    
    re$df() %>%
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
