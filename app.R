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
                   radioButtons("time", "Choose your preferred time frame.",
                                choices = c("Year", "Semester")),
                   sliderInput("capacity", "Minimum Course Capacity", min = 1, max = max(course_reg_data$yearly_capacity), value= 5),
                   selectizeInput("courses", label = "Select a Course", 
                                choices = unique(course_reg_data$course),
                                selected = "Applied Statistics"),
                   checkboxGroupInput("sections", "Graph Settings:", 
                                choices = NULL,
                                selected = "basic")),
               mainPanel(plotlyOutput(outputId = "plot")
             ))
    ), # course tabPanel
    tabPanel("Enrollment by Discipline",
             sidebarLayout(
               sidebarPanel(
                 selectInput("discipline", label = "Select a Subject",
                             choices = unique(course_reg_data$Subject),
                             selected = "CS"),
                 textOutput(outputId = "data_description")
               ),
               mainPanel(plotlyOutput(outputId = "discipline_plot"))
             )), # discipline tabPanel
    tabPanel("Open Seats",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("time_openseats", "Choose your preferred time frame.",
                              choices = c("Year", "Semester")),
                 selectInput("course_openseats", label = "Select a Course",
                             choices = unique(course_reg_data$course),
                             selected = "Applied Statistics")
               ), #side panel
               mainPanel(plotlyOutput(outputId = "openseats_plot"))
             ) #side layout
             ) # open seats tabPanel
  ) #tabsetPanel
  ) #end fluidPage


server <- function(input, output, session) {
  description_text <- "*The data being used in this plot does not include SYE courses (these courses are often independent and have low enrollment), 
  therefore the level 4 courses are not pictured."
  output$data_description <- renderText({
    description_text
  })
  observeEvent(input$time, {
    if (input$time == "Semester"){
      choices <- c("show class sections", "label sections", "omit COVID-19 years")
    }
    else if (input$time == "Year"){
      choices <- c("omit COVID-19 years")
    }
    updateCheckboxGroupInput(session, "sections", choices = choices, selected = NULL)
  })
  observeEvent(input$capacity, {
    if (input$time == "Semester") {
      filter_capacity <- course_reg_data |> filter(yearly_capacity >= input$capacity)
    }
    else if (input$time == "Year"){
      filter_capacity <- course_reg_data |> filter(yearly_capacity >= input$capacity)
    }
    updateSelectizeInput(inputId = "courses", choices = unique(filter_capacity$course))
  })
  # plot enrollment by semester
  output$plot <- renderPlotly({
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
        course_app_semester <- course_app_semester |> mutate(avg_section = round(semester_enrolled / total_sections))|>
          mutate(term = fct_reorder(term, order, .desc = FALSE))
        if ("omit COVID-19 years" %in% input$sections){
          course_app_semester <- course_app_semester |> filter(term != "2020_FA") |> filter(term != "2021_SP")
          course_app_section <- course_app_section |> filter(term != "2020_FA") |> filter(term != "2021_SP")
        }
        plot1 <- ggplot(data = course_app_semester, aes(x = term, y = semester_enrolled, group = 1)) +
          geom_line() +
          geom_point(data = course_app_semester, aes(x = term, y = semester_enrolled, group = 1)) +
          geom_point(data = course_app_section, aes(x = term, y = cumulative_enrolled, group = 1), alpha = 0.25, shape = 45)+
          geom_text(data = course_app_section, aes(x = term, y = cumulative_enrolled - 5, label = section), angle = 90,
                    alpha = 0.5, size = 2,
                    hjust = 0, vjust = 0.5, nudge_x= -0.2) +
          scale_x_discrete(limits = levels(course_app_semester$term)) +
          scale_y_continuous(limits = c(0, max(course_app_semester$semester_enrolled))) +
          labs(title = glue::glue("Enrollment for ", input$courses),
               x = "Semester",
                 y = "Enrollment",
                 colour = "Semester") + 
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                axis.text.y = element_text(hjust = 1))
        plot_nosection <- ggplot(data = course_app_semester, aes(x = term, y = semester_enrolled, group = 1, label = total_sections, label2 = avg_section)) +
          geom_line() +
          geom_point() +
          scale_x_discrete(limits=levels(course_app_semester$term)) +
          scale_y_continuous(limits= c(0, max(course_app_semester$semester_enrolled))) +
          labs(title = glue::glue("Enrollment for ", input$courses),
               x = "Semester",
               y = "Enrollment",
               colour = "Semester") + 
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                axis.text.y = element_text(hjust = 1))
        plot_sectiontic <- ggplot(data = course_app_semester, aes(x = term, y = semester_enrolled, group = 1)) +
          geom_line() +
          geom_point(data = course_app_semester, aes(x = term, y = semester_enrolled, group = 1)) +
          geom_point(data = course_app_section, aes(x = term, y = cumulative_enrolled, group = 1), alpha = 0.25, shape = 45)+
          scale_x_discrete(limits = levels(course_app_semester$term)) +
          scale_y_continuous(limits = c(0, max(course_app_semester$semester_enrolled))) +
          labs(title = glue::glue("Enrollment for ", input$courses),
               x = "Semester",
               y = "Enrollment",
               colour = "Semester") + 
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                axis.text.y = element_text(hjust = 1))
        plot_labels <- ggplot(data = course_app_semester, aes(x = term, y = semester_enrolled, group = 1)) +
          geom_line() +
          geom_point(data = course_app_semester, aes(x = term, y = semester_enrolled, group = 1)) +
          geom_text(data = course_app_section, aes(x = term, y = cumulative_enrolled - 5, label = section), angle = 90,
                    alpha = 0.5, size = 2,
                    hjust = 0, vjust = 0.5, nudge_x= -0.2) +
          scale_x_discrete(limits = levels(course_app_semester$term)) +
          scale_y_continuous(limits = c(0, max(course_app_semester$semester_enrolled))) +
          labs(title = glue::glue("Enrollment for ", input$courses),
               x = "Semester",
               y = "Enrollment",
               colour = "Semester") + 
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                axis.text.y = element_text(hjust = 1))
        if ("show class sections" %in% input$sections && "label sections" %in% input$sections){
          semester_plot <- ggplotly(plot1) |> style(hoverinfo = "none", 
                                   traces = c(5))
        }
        else if ("show class sections" %in% input$sections){
          semester_plot <- ggplotly(plot_sectiontic)
        }
        else if ("label sections" %in% input$sections){
          semester_plot <- ggplotly(plot_labels)
        }
        else {
          semester_plot <- ggplotly(plot_nosection)
        }
        return(semester_plot)
        
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
        course_app_yearly <- course_app_yearly |> mutate(avg_section = round(yearly_enrolled / total_sections))
        if ("omit COVID-19 years" %in% input$sections){
          course_app_yearly <- course_app_yearly |> filter(reporting_year != 2020)
        }
        plot2 <-ggplot(data = course_app_yearly, aes(x = as.character(reporting_year), y = yearly_enrolled, group = 1, label = total_sections, label2 = avg_section)) +
          geom_line() +
          geom_point() +
          labs(title = glue::glue("Enrollment for ", input$courses),
               x = "Year",
               y = "Enrollment") + 
          scale_y_continuous(limits = c(0, max(course_app_yearly$yearly_enrolled))) +
          theme_minimal()+
          theme(axis.text.x = element_text(angle = 90, vjust=0.5,hjust=1))
        ggplotly(plot2)
      } # year
    } # input courses
  ) # render plotly
  output$discipline_plot <- renderPlotly({
    if (input$discipline %in% course_reg_data$Subject){
      subject_data <- course_reg_data |> mutate(level = case_when(
        as.numeric(`Course Number`) >= 1000 ~ as.numeric(`Course Number`) %/% 1000 - 1,
        as.numeric(`Course Number`) < 1000 ~ as.numeric(`Course Number`) %/% 100))
      subject_data <- subject_data |> filter(Subject %in% input$discipline)
      subject_data <- subject_data |> group_by(level, reporting_year) |> summarise(enrolled = sum(yearly_enrolled))
      subject_data <- subject_data |> filter(level <= 3)
      section_total <- course_reg_section |> mutate(level = case_when(
        as.numeric(`Course Number`) >= 1000 ~ as.numeric(`Course Number`) %/% 1000 - 1,
        as.numeric(`Course Number`) < 1000 ~ as.numeric(`Course Number`) %/% 100))
      section_total <- section_total |> filter(Subject %in% input$discipline)
      section_total <- section_total |> group_by(year, level) |> summarise(total_sections = n()) |> rename("reporting_year" = "year")
      section_total <- section_total |> filter(level <=3)
      subject_data <- left_join(subject_data, section_total, by = c("reporting_year", "level"))
      subject_data <- subject_data |> mutate(avg_section = round((enrolled/total_sections)))
      subject_plot<- ggplot(data = subject_data, aes(x = reporting_year, y = enrolled, color = factor(level), label = total_sections, label2 = avg_section)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0, max(subject_data$enrolled))) +
        labs(color = "Course Level",
             x = "Year",
             y = "Enrollment") +
        theme_minimal()
      ggplotly(subject_plot, tooltip = c("label", "y", "label2"))
    }
  }) # end discipline plotly
  output$openseats_plot <- renderPlotly({
    if (input$time_openseats == "Semester") {
      course_app_semester <- course_reg_data |> 
        rename("FA" = "fall_enrolled",
               "SP" = "spring_enrolled")|>
        pivot_longer(cols = c(FA, SP),names_to = "semester", values_to = "semester_enrolled") |> 
        filter(course %in% input$course_openseats) |> 
        mutate(order = case_when(
          semester == "SP" ~ reporting_year,
          semester == "FA" ~ reporting_year + 1)) |> 
        unite("term", c(reporting_year, semester)) |>
        mutate(term = fct_reorder(term, order, .desc = FALSE)) 
      course_app_semester <- course_app_semester |>
        filter(semester_enrolled != 0)
      course_app_semester <- course_app_semester |> 
        mutate(semester_capacity = if_else(str_detect(term, "FA"), fall_capacity, spring_capacity))
      course_app_section <- course_reg_section |> group_by(course, term) |> mutate(cumulative_enrolled = cumsum(enrolled))
      course_app_section <- course_app_section |> 
        filter(course %in% input$course_openseats)|>
        filter(enrolled != 0)
      course_app_section3 <- course_app_section |> group_by(term) |> summarise(total_sections = n())
      course_app_semester <- left_join(course_app_semester, course_app_section3,
                                       by = c("term"))
      course_app_semester <- course_app_semester |> mutate(avg_section = round(semester_enrolled / total_sections))
      plot1 <- ggplot(data = course_app_semester, aes(x = term)) +
        geom_col(aes(y = semester_enrolled, fill = "Enrollment")) +
        geom_col(aes(y = semester_capacity, fill = "Enrollment Capacity"), alpha = 0.5) +
        scale_fill_grey()+
        labs(
          x = "Year",
          y = "Enrollment",
          fill = "") + 
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, vjust=0.5,hjust=1))
      ggplotly(plot1)
    } # end semester
    else if (input$time_openseats == "Year") {
      course_app_yearly <- course_reg_data |> filter(course %in% input$course_openseats)
      course_reg_section <- course_reg_section  |>
        filter(course %in% input$course_openseats) |> 
        group_by(year) |> summarise(total_sections = n()) |> rename("reporting_year"="year")
      course_app_yearly <- left_join(course_app_yearly, course_reg_section,
                           by= c("reporting_year"))
      course_app_yearly <- course_app_yearly |> mutate(avg_section = round((yearly_enrolled/total_sections)))
      plot1 <- ggplot(data = course_app_yearly, aes(x = as.character(reporting_year)), label = total_sections, label2 = avg_section) +
        geom_col(aes(y = yearly_enrolled, fill = "Enrollment")) +
        geom_col(aes(y = yearly_capacity, fill = "Enrollment Capacity"), alpha = 0.5) +
        scale_fill_grey() +
        labs(
          x = "Year",
          y = "Enrollment",
          fill = "") + 
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, vjust=0.5,hjust=1))
      ggplotly(plot1, tooltip = c("label", "y", "label2"))
    }
  }) # end openseats plot

  } # end server

# add number of sections to discipline graph

shinyApp(ui, server)



