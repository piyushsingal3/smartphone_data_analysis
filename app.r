library(shiny)
library(ggplot2)
library(shinythemes)
library(magick)
shinytheme("superhero")

MainData <- read.csv("smartphone_data.csv")
MainData$release_date <- as.numeric(MainData$release_date)
MainData <- subset(MainData, !is.na(MainData$release_date))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel("Smartphone Master"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("brand",
                        "Select Brand: ",
                        choices = unique(MainData$brand),
                        selected = c("Oneplus")),
            sliderInput("year",
                        "Release Year: ",
                        min = min(MainData$release_date),
                        max = max(MainData$release_date),
                        value = c(min(MainData$release_date),max(MainData$release_date))),
            selectInput("model",
                        "Select Model: ",
                        choices = NULL)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("image")
        )
    )
)
#MainData$image_link[which(MainData$device_name == input$model)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
   observeEvent({input$brand
                 input$year},
                updateSelectInput(session, "model", choices = subset(MainData, MainData$brand %in% input$brand & MainData$release_date <= input$year[2] & MainData$release_date >= input$year[1])$device_name))
   output$image <- renderPlot(
     plot(image_read(MainData$image_link[which(MainData$device_name == input$model)]))
   )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
