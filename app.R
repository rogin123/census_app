library(shiny)
library(tidyverse)
library(sf)
library(maps)
library(rgeos)
library(sp)
library(maptools)



# Read in a prep data
source("data_prep.R")

# Helper
source("helpers.R")


# User interface ----
ui <- fluidPage(
  titlePanel("censusVis"),
  sidebarLayout(
    sidebarPanel(
      ## Quick synopsis of app's purpose
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
      ## drop down widget so user can select which race to create map for 
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent White", "Percent Black",
                              "Percent Hispanic", "Percent Asian"),
                  selected = "Percent White"),
      ## drop down widget so user can select which race to create map for 
      selectInput("state", 
                  label = "Select a state of the contiguous 48 states to display",
                  choices = c(state_name_widget,
                              "Contiguous 48 States, Counties"),
                  selected = "illinois"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(
      h1(textOutput("title"), align = "center"),
      plotOutput("map")
      )
  )
)

# Server logic ----
server <- function(input, output) {
  
  # Title for main panel 
  output$title <- renderText({
    input$state
  })
  ## Constructing and storing data into a heat map 
  output$map <- renderPlot({
    ## The plotting function percent_map()
    race_var <- switch(input$var, 
                   "Percent White" = "white",
                   "Percent Black" = "black",
                   "Percent Hispanic" = "hispanic",
                   "Percent Asian" = "asian")
    ## Switch to identify from the UI which color should be passed into 
    ## the plotting function percent_map() - the color is tied to 
    ## which race's data is selected 
    color <- switch(input$var, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")
    ## Switch to identify from the UI which legend title sshould be passed into
    ## the plotting function percent_map()
    legend <- switch(input$var, 
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    percent_map(var = race_var,
                color = color,
                legend_title = legend,
                min = as.numeric(input$range[1]),
                max = as.numeric(input$range[2]), 
                state_selection = input$state)
  })
}

# Run app ----
shinyApp(ui, server)

