library(shiny)
library(ggthemes)
library(readr)
library(plotly)
library(tidyverse)
course_reg_data <- read_csv("course_reg_data.csv")
course_reg_section <- course_reg_data |> 
  pivot_longer((c(6:43)), names_to = "semester", values_to = "enrolled") |>
  filter(enrolled != 0) 
course_reg_section <- course_reg_section |> separate(col = semester, into = c("semester", "section"), sep = "_") |> mutate(year = reporting_year)
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
                                selected = "Applied Statistics")),
               mainPanel(plotlyOutput(outputId = "plot")
             ))
    ), # course tabPanel
    tabPanel("Enrollment by Discipline",
             sidebarLayout(
               sidebarPanel(
                 selectInput("discipline", label = "Select a Subject",
                             choices = course_reg_data$Subject)
               ),
               mainPanel(plotlyOutput(outputId = "discipline_plot"))
             )) # discipline tabPanel
  ) #tabsetPanel
  ) #end fluidPage


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
        course_app_section <- course_reg_section |> group_by(course, term) |> mutate(cumulative_enrolled = cumsum(enrolled))
        course_app_section <- course_app_section |> 
          filter(course %in% input$courses)|>
          filter(enrolled != 0)
        course_app_section3 <- course_app_section |> group_by(term) |> summarise(total_sections = n())
        course_app_semester <- left_join(course_app_semester, course_app_section3,
                                         by = c("term"))
        plot1 <- ggplot(data = course_app_semester, aes(x = term, y = semester_enrolled, label = total_sections, group = 1)) +
          geom_line() +
          geom_point() +
          geom_point(data = course_app_section, aes(x = term, y = cumulative_enrolled, group = 1), alpha = 0.25, shape = 45)+
          geom_segment(data = course_app_section, aes(x = term, xend = term, y = 0, yend = cumulative_enrolled), color = "gray", alpha = 0.25) +
          geom_text(data = course_app_section, aes(x = term, y = cumulative_enrolled - 5, label = section), angle = 90,
                    alpha = 0.5, size = 2,
                    hjust = 0, vjust = 0.5, nudge_x= -0.2) +
          scale_x_discrete(limits = levels(course_app_semester$term)) +
          scale_y_continuous(limits = c(0, max(course_app_semester$semester_enrolled))) +
          labs(title = glue::glue("Enrollment for ", input$courses),
               x = "Year",
                 y = "Enrollment",
                 colour = "Semester") + 
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                axis.text.y = element_text(hjust = 1))
        ggplotly(plot1, tooltip = c("y", "label"))
      }
      else if (input$time == "Year"){
        course_app_yearly <- course_reg_data |> filter(course %in% input$courses)
        course_app_section2 <- course_reg_section |> group_by(course, term) |> mutate(cumulative_enrolled = cumsum(enrolled))
        course_app_section2 <- course_app_section2 |> 
          filter(course %in% input$courses)|>
          filter(enrolled != 0)
        course_app_section2 <- course_app_section2 |> group_by(year) |> summarise(total_sections = n()) |> rename("reporting_year"="year")
        course_app_yearly <- left_join(course_app_yearly, course_app_section2,
                             by= c("reporting_year"))
        plot2 <-ggplot(data = course_app_yearly, aes(x = reporting_year, y = yearly_enrolled, group = 1, label = total_sections)) +
          geom_line() +
          geom_point() +
          labs(title = glue::glue("Enrollment for ", input$courses),
               x = "Year",
               y = "Enrollment") + 
          theme_minimal()+
          theme(axis.text.x = element_text(angle = 90, vjust=0.5,hjust=1))
        ggplotly(plot2, tooltip = c("label", "y"))
      } # year
    } # input courses
  }) # render plotly
  output$discipline_plot <- renderPlotly({
    if (input$discipline %in% course_reg_data$Subject){
      subject_data <- course_reg_data |> mutate(level = case_when(
        as.numeric(`Course Number`) >= 1000 ~ as.numeric(`Course Number`) %/% 1000 - 1,
        as.numeric(`Course Number`) < 1000 ~ as.numeric(`Course Number`) %/% 100)) |> na.omit(level)
      subject_data <- subject_data |> filter(Subject %in% input$discipline)
      subject_data <- subject_data |> group_by(level, reporting_year) |> summarise(enrolled = sum(yearly_enrolled))
      subject_plot<- ggplot(data = subject_data, aes(x = reporting_year, y = enrolled, color = factor(level))) +
        geom_point() +
        geom_line() +
        labs(color = "Course Level") +
        theme_minimal()
      ggplotly(subject_plot)
    }
  }) # end discipline plotly
  } # end server


shinyApp(ui, server)

