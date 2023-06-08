library(shiny)
library(ggthemes)
library(readr)
library(plotly)
library(tidyverse)
course_reg_data <- read_csv("course_reg_data.csv")


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  tabsetPanel(
    tabPanel("Enrollment by Course",
             sidebarLayout(
               sidebarPanel(
                 tabsetPanel(
                   tabPanel("Yearly",
                            selectInput("courseYearly", label = "Select a Course", 
                                        choices = course_reg_data$course,
                                        selected = course_reg_data$course[1])),
                   tabPanel("By Semester",
                            selectInput("courseSemester", label = "Select a Course", 
                                        choices = course_reg_data$course, 
                                        selected = course_reg_data$course[1]))
                 )),
               mainPanel(
                 plotlyOutput(outputId = "plot")
               )
             )
    )
  ))


server <- function(input, output, session) {
  # plot enrollment by semester
  output$plot <- renderPlotly({
    if ( input$courseSemester %in% course_reg_data$course){
      course_app_semester <- course_reg_data |> filter(course %in% input$courseSemester) |> 
        arrange(reporting_year)
      plot1 <- ggplot(data = course_app_semester, aes(x = reporting_year, group = 1)) +
        geom_line(aes(y = FA, colour = "Fall")) +
        geom_point(aes(y = FA, colour = "Fall")) +
        geom_line(aes(y = SP, colour = "Spring")) +
        geom_point(aes(y = SP, colour = "Spring")) +
        labs(title = glue::glue("Enrollment for ", input$courseSemester),
             x = "Year",
             y = "Enrollment",
             colour = "Semester") + 
        theme_economist() +
        scale_color_economist() +
        theme(axis.text.x = element_text(angle = 90)) 
      ggplotly(plot1)
    }
    else if( input$courseYearly %in% course_reg_data$course){
      course_app_yearly <- course_reg_data |> filter(course %in% input$courseYearly)
      plot2 <-ggplot(data = course_app_yearly, aes(x = reporting_year, y = yearly_enrolled, group = 1)) +
        geom_line() +
        geom_point() +
        labs(title = glue::glue("Enrollment for ", input$courseYearly),
             x = "Year",
             y = "Enrollment") + 
        theme_economist() +
        scale_color_economist() +
        theme(axis.text.x = element_text(angle = 90))
      ggplotly(plot2)
    }
    
  })
  
}

shinyApp(ui, server)

