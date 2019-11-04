library(shiny) 

ui <- pageWithSidebar(
  headerPanel('Lab 1-Part 3'),      # title of the app
  sidebarPanel(
    selectInput('xcol', 'CDC vs Twitter Map', c("all" = "all",
                                                "flu" = "flu"))
  ),     # create a sidebar which can include actionButtons, drop-downs, etc.
  mainPanel(imageOutput(outputId = "imageTwitter",width = "100%"),br(),br(),br(),br(),br(),br(),br(), imageOutput(outputId = "imageCDC"),width = "100%")       # create a main panel to represent your charts, texts etc.
)
server <- function(input, output) {
  # links all functions to ui
  output$imageCDC <- renderImage({
    filename <- normalizePath(file.path(dirname(rstudioapi::getSourceEditorContext()$path),
                                        paste(input$xcol, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$imageTwitter <- renderImage({
    filename1 <- normalizePath(file.path(dirname(rstudioapi::getSourceEditorContext()$path),
                                         paste(input$xcol, 'twitter.png', sep='')))
    # Return a list containing the filename and alt text
    list(src = filename1)
    
  }, deleteFile = FALSE)
  
  #output$imageTwitter <- img(src="myImage.png",height=500,width=500)
}


shinyApp(ui, server)