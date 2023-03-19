# Data import & Calling required libraries
library(shiny)
library(tidyverse)
library(ggplot2)


ui = fluidPage(
    
    # Application title (I added for aesthetics)
    titlePanel("PSY 8960 Week 8 Shiny App"),
  
    # Sidebar with a select input for gender, error band, and inclusion of assessment
    sidebarLayout(
        sidebarPanel(
            selectInput("gender", "Select Gender:",
                        selected = "All",
                        choices = c("Male", "Female", "All")),
            selectInput("error_band", "Display error band or not?",
                      selected = "Display Error Band",
                      choices = c("Display Error Band", "Suppress Error Band")),
            selectInput("date_include", "Include or exclude assessment completed before August 1st 2017?",
                      selected = "Include",
                      choices = c("Include", "Exclude")) 
    ),
        # Display initeractive ggplot 
        mainPanel(
            plotOutput("plot")
       )
    ) 
)



server = function(input, output){
  
    # Loading in the RDS object as data once 
    week8_shiny_data = readRDS("../data/week8_shiny_data.RDS")
  
    # Rendering output ggplot 
    output$plot = renderPlot({
    
        # If-statement: filter data if exclude records before 2017-08-01
        if(input$date_include == "Exclude"){
          week8_shiny_data = week8_shiny_data %>% filter(timeEnd > "2017-08-01 00:00:00")
          }
    
        # If-statement: filter data based on gender if  gender is all 
        if(input$gender != "All"){ 
          week8_shiny_data = week8_shiny_data %>% filter(gender == input$gender)
          }
    
        # Creating logical for whether to display error band
        error_band_boo = ifelse(input$error_band == "Display Error Band", TRUE, FALSE)

        # Generating required scatterplot using filtered data  
        week8_shiny_data %>% 
          ggplot(aes(x = mean_1_to_6, y = mean_8_to_10)) +
          geom_point() + 
          geom_smooth(method = "lm", color = "purple", se = error_band_boo) + 
          labs(x = "Mean scores on Q1-Q6", y = "Mean scores on Q8-Q10")     
    })
}


shinyApp(ui = ui , server = server)
# rsconnect::deployApp('shiny_week8/')