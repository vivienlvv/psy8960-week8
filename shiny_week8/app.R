# Data import & Calling required libraries
library(shiny)
library(tidyverse)
library(ggplot2)




# Defining user interface
ui = fluidPage(
  titlePanel("PSY 8960 Week 8 Shiny App"),
  sidebarLayout(
    sidebarPanel(
      # 4. SelectInput() for Male, Female, All (default)
      selectInput("gender", "Select Gender:", selected = "All", choices = c("Male", "Female", "All")),
      # 5. SelectInput() for error band display, "Display Error Band" (default) and "Suppress Error Band", used as argument for geom_smooth()
      selectInput("error_band", "Display error band or not?", selected = "Display Error Band", choices = c("Display Error Band", "Suppress Error Band")),
      # 6. SelectInput() include (default) or exclude assessment before Aug 1 2017
      selectInput("date_include", "Include or exclude assessment completed before August 1st 2017?", selected = "Include", choices = c("Include", "Exclude")) 
    ),
    mainPanel(
      # 3. Display ggplot2 from part 2
      DT::DTOutput("plot")
    )
  ) 
)


# Defining server function
server = function(input, output){
  
  week8_shiny_data = readRDS("../data/week8_shiny_data.RDS")
  
  # output ggplot 
  output$plot = renderPlot({
    
    # If exclude is chosen, then participants who completed the assessment before AUG 1 2017 will be filtered out
    if(input$date_include == "Exclude"){
      week8_shiny_data = week8_shiny_data %>% filter(timeEnd > "2017-08-01 00:00:00")
    }
    
    # If gender chosen is Male/ Female, dataset will be further filtered
    if(input$gender != "All"){ # When gender selected in "All"
      week8_shiny_data = week8_shiny_data %>% filter(gender == input$gender)
    }
    
    # Creating argument for whether to display error band
    error_band_boo = ifelse(input$error_band == "Display Error Band", TRUE, FALSE)

    # Creating required scatterplot using filtered data  
    week8_shiny_data %>% 
      ggplot(aes(x = mean_1_to_6, y = mean_8_to_10)) + 
      geom_point() + 
      geom_smooth(method = "lm", color = "purple", se = error_band_boo) + 
      labs(x = "Mean scores on Q1-Q6",
           y = "Mean scores on Q8-Q10")     
    })
}


shinyApp(ui = ui , server = server)

# Deploying app at the end 
# rsconnect::deployApp('shiny_week8/')