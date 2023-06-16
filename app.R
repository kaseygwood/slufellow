library(shiny)
library(ggthemes)
library(readr)
library(plotly)
library(tidyverse)
course_reg_data <- read_csv("course_reg_data.csv")
course_reg_section <- course_reg_data |> 
  pivot_longer((c(6:43)), names_to = "semester", values_to = "enrolled") |>
  filter(enrolled != 0) 
course_reg_section <- course_reg_section |> separate(col = semester, into = c("semester", "section"), sep = "_")
course_reg_section <- course_reg_section |> unite("term", c(reporting_year, semester))

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  tabsetPanel(
    tabPanel("Enrollment by Course",
             sidebarLayout(
               sidebarPanel(
                   radioButtons("time", "Graph by year or by semester?",
                                choices = c("Year", "Semester")),
                   selectInput("courses", label = "Select a Course", 
                                choices = course_reg_data$course,
                                selected = course_reg_data$course[1])),
               mainPanel(plotlyOutput(outputId = "plot"))
             ))
    )
  )


server <- function(input, output, session) {
  # plot enrollment by semester
  output$plot <- renderPlotly({
    if ( input$courses %in% course_reg_data$course){
      if (input$time == "Semester"){
        course_app_semester <- course_reg_data |> 
          rename("FA" = "fall_enrolled",
                 "SP" = "spring_enrolled")|>
          pivot_longer(cols = c(FA, SP),names_to = "semester", values_to = "semester_enrolled") |> 
          filter(course %in% input$courses) |> 
          mutate(order = case_when(
            semester == "SP" ~ reporting_year,
            semester == "FA" ~ reporting_year + 1)) |> 
          unite("term", c(reporting_year, semester)) |>
          mutate(term = fct_reorder(term, order, .desc = FALSE)) 
        course_app_semester <- course_app_semester |>
          filter(semester_enrolled != 0)
        course_app_section <- course_reg_section |> 
          filter(course %in% input$courses)|>
          filter(enrolled != 0)
        plot1 <- ggplot(data = course_app_semester, aes(x = term, y = semester_enrolled, label = semester_enrolled, group = 1)) +
          geom_line() +
          geom_point() +
          scale_x_discrete(limits = levels(course_app_semester$term)) +
          scale_y_continuous(limits = c(0, max(course_app_semester$semester_enrolled))) +
          geom_point(data = course_app_section, aes(x = term, y = enrolled, label = section, group = 1), alpha = 0.5) +
          labs(title = glue::glue("Enrollment for ", input$courses),
               x = "Year",
               y = "Enrollment",
               colour = "Semester") + 
          theme_economist() +
          scale_color_economist() +
          theme(axis.text.x = element_text(angle = 90)) 
        ggplotly(plot1, tooltip = c("label"))
      }
      else if (input$time == "Year"){
        course_app_yearly <- course_reg_data |> filter(course %in% input$courses)
        plot2 <-ggplot(data = course_app_yearly, aes(x = reporting_year, y = yearly_enrolled, group = 1)) +
          geom_line() +
          geom_point() +
          labs(title = glue::glue("Enrollment for ", input$courses),
               x = "Year",
               y = "Enrollment") + 
          theme_economist() +
          scale_color_economist() +
          theme(axis.text.x = element_text(angle = 90))
        ggplotly(plot2)
      }
    }
  })
}

shinyApp(ui, server)

