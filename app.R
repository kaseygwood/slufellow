library(shiny)
library(ggthemes)
library(readr)
library(plotly)
library(tidyverse)

course_reg_data <- read_csv("full_course_reg_data.csv")
course_reg_section <- course_reg_data |> 
  pivot_longer((c(6:43)), names_to = "semester", values_to = "enrolled") |>
  filter(enrolled != 0) |>
  mutate(semester_year = ifelse(str_detect(semester, "SP"), reporting_year+1, reporting_year))
course_reg_section <- course_reg_section |> 
  separate(col = semester, into = c("semester", "section"), sep = "_") |> 
  mutate(year = reporting_year)
course_reg_section <- course_reg_section |> 
  unite("term", c(semester_year, semester))

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  tabsetPanel(
    tabPanel("About the App",
             mainPanel(
               tags$div(
                 style = "padding: 20px;",
                 tags$h3("App Description"),
                 tags$p(
                   style = "font-size: 16px;",
                   "The data used in this app comes from St. Lawrence University's registration data with data spanning
                   from 2014 to 2022. Data is included from the years that COVID-19 impacted the University, however there are settings to omit that data.
                   Therefore, the section checkbox (in the Semester time frame of the \"Enrollment by Course\" tab) for the 2020 semesters are not reliable.
                   Cross-listed courses will show the full enrollment total for both courses together when looking 
                   at either individual course."
                 )
               )
             )),
    tabPanel("Enrollment by Course",
             sidebarLayout(
               sidebarPanel(
                   radioButtons("time", "Choose your preferred time frame.",
                                choices = c("Year", "Semester")),
                   selectizeInput("select_subject", "Select a Subject",
                               choices = unique(course_reg_data$Subject),
                               selected = "CS"),
                   sliderInput("capacity", "Minimum Course Capacity", 
                               min = 1, 
                               max = 60, 
                               value= 6),
                   sliderInput("years_offered", "Minimum Years Offered", 
                               min = 1, 
                               max = 9, 
                               value = 5),
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
                 selectizeInput("discipline", 
                                label = "Select a Subject",
                             choices = unique(course_reg_data$Subject),
                             selected = "CS"),
                 checkboxGroupInput("discipline_settings", "Graph Settings:", 
                                    choices = c("omit COVID-19 years"),
                                    selected = "basic"),
                 textOutput(outputId = "data_description")
               ),
               mainPanel(plotlyOutput(outputId = "discipline_plot"))
             )), # discipline tabPanel
    tabPanel("Open Seats",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("time_openseats", 
                              "Choose your preferred time frame.",
                              choices = c("Year", "Semester")),
                 selectizeInput("select_subject_seats", 
                                "Select a Subject",
                             choices = unique(course_reg_data$Subject),
                             selected = "CS"),
                 sliderInput("capacity_openseats", 
                             "Minimum Course Capacity", 
                             min = 1, 
                             max = 60, 
                             value= 6),
                 sliderInput("years_offered_seats", 
                             "Minimum Years Offered", 
                             min = 1, 
                             max = 9, 
                             value = 5),
                 selectizeInput("course_openseats", 
                                label = "Select a Course",
                             choices = unique(course_reg_data$course),
                             selected = "Applied Statistics"),
                 checkboxGroupInput("openseats_settings", 
                                    "Graph Settings:",
                                    choices = c("omit COVID-19 years"),
                                    selected = "basic")
               ), #side panel
               mainPanel(plotlyOutput(outputId = "openseats_plot"))
             ) #side layout
             ) # open seats tabPanel
  ) #tabsetPanel
  ) #end fluidPage


server <- function(input, output, session) {
  observeEvent(input$years_offered, {
    filter_subject <- course_reg_section |> 
      group_by(course) |> 
      mutate(total_offered = n())
    filter_subject <- course_reg_section |> 
      group_by(year, course) |> 
      summarise(total_sections = n(),
                yearly_capacity = first(yearly_capacity),
                avg_capacity = round(yearly_capacity/total_sections)) |>
      rename("reporting_year" = "year")
    filter_subject <- filter_subject |> 
      group_by(course) |> mutate(total_years_offered = n())
    filter_subject <- left_join(course_reg_data, filter_subject, by=c("course", "reporting_year"))
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      summarise(full_avg_capacity = round(sum(avg_capacity, na.rm = TRUE)/n()),
                Subject = first(Subject),
                total_years_offered =first(total_years_offered))
    filter_subject <- filter_subject |> 
      filter(full_avg_capacity >= input$capacity) |> 
      filter(Subject == input$select_subject) |> 
      filter(total_years_offered >= input$years_offered)
    updateSelectizeInput(session, "courses", choices = unique(filter_subject$course))
  })
  observeEvent(input$years_offered_seats, {
    filter_subject <- course_reg_section |> 
      group_by(course) |> 
      mutate(total_offered = n())
    filter_subject <- course_reg_section |> 
      group_by(year, course) |> 
      summarise(total_sections = n(),
                yearly_capacity = first(yearly_capacity),
                avg_capacity = round(yearly_capacity/total_sections)) |>
      rename("reporting_year" = "year")
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      mutate(total_years_offered = n())
    filter_subject <- left_join(course_reg_data, filter_subject, by=c("course", "reporting_year"))
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      summarise(full_avg_capacity = round(sum(avg_capacity, na.rm = TRUE)/n()),
                Subject = first(Subject),
                total_years_offered =first(total_years_offered))
    filter_subject <- filter_subject |> 
      filter(full_avg_capacity >= input$capacity_openseats) |> 
      filter(Subject == input$select_subject_seats) |> 
      filter(total_years_offered >= input$years_offered_seats)
    updateSelectizeInput(session, "course_openseats", choices = unique(filter_subject$course))
  })
  observeEvent(input$select_subject, {
    filter_subject <- course_reg_section |> 
      group_by(course) |> 
      mutate(total_offered = n())
    filter_subject <- course_reg_section |> 
      group_by(year, course) |> 
      summarise(total_sections = n(),
                yearly_capacity = first(yearly_capacity),
                avg_capacity = round(yearly_capacity/total_sections)) |>
      rename("reporting_year" = "year")
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      mutate(total_years_offered = n())
    filter_subject <- left_join(course_reg_data, filter_subject, by=c("course", "reporting_year"))
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      summarise(full_avg_capacity = round(sum(avg_capacity, na.rm = TRUE)/n()),
                Subject = first(Subject),
                total_years_offered =first(total_years_offered))
    filter_subject <- filter_subject |> 
      filter(full_avg_capacity >= input$capacity) |> 
      filter(Subject == input$select_subject) |> 
      filter(total_years_offered >= input$years_offered)
    updateSelectizeInput(session, "courses", choices = unique(filter_subject$course))
  }) # end subject event
  observeEvent(input$select_subject_seats, {
    filter_subject <- course_reg_section |> 
      group_by(course) |> 
      mutate(total_offered = n())
    filter_subject <- course_reg_section |> 
      group_by(year, course) |> 
      summarise(total_sections = n(),
                yearly_capacity = first(yearly_capacity),
                avg_capacity = round(yearly_capacity/total_sections)) |>
      rename("reporting_year" = "year")
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      mutate(total_years_offered = n())
    filter_subject <- left_join(course_reg_data, filter_subject, by=c("course", "reporting_year"))
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      summarise(full_avg_capacity = round(sum(avg_capacity, na.rm = TRUE)/n()),
                Subject = first(Subject),
                total_years_offered =first(total_years_offered))
    filter_subject <- filter_subject |> 
      filter(full_avg_capacity >= input$capacity_openseats) |> 
      filter(Subject == input$select_subject_seats) |> 
      filter(total_years_offered >= input$years_offered_seats)
    updateSelectizeInput(session, "course_openseats", choices = unique(filter_subject$course))
  }) # end subject event
  description_text <- "*The data being used in this plot does not include SYE courses 
  (these courses are often independent and have low enrollment), 
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
  }) # end checkboxinputupdate
  observeEvent(input$capacity, {
    filter_subject <- course_reg_section |> 
      group_by(course) |> 
      mutate(total_offered = n())
    filter_subject <- course_reg_section |> 
      group_by(year, course) |> summarise(total_sections = n(),
                                          yearly_capacity = first(yearly_capacity),
                                          avg_capacity = round(yearly_capacity/total_sections)) |>
      rename("reporting_year" = "year")
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      mutate(total_years_offered = n())
    filter_subject <- left_join(course_reg_data, filter_subject, by=c("course", "reporting_year"))
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      summarise(full_avg_capacity = round(sum(avg_capacity, na.rm = TRUE)/n()),
                Subject = first(Subject),
                total_years_offered =first(total_years_offered))
    filter_subject <- filter_subject |> 
      filter(full_avg_capacity >= input$capacity) |> 
      filter(Subject == input$select_subject) |> 
      filter(total_years_offered >= input$years_offered)
    updateSelectizeInput(session, "courses", choices = unique(filter_subject$course))
  }) # end capacity edit for enrollment graph
  observeEvent(input$capacity_openseats, {
    filter_subject <- course_reg_section |> 
      group_by(course) |> 
      mutate(total_offered = n())
    filter_subject <- course_reg_section |> 
      group_by(year, course) |> 
      summarise(total_sections = n(),
                yearly_capacity = first(yearly_capacity),
                avg_capacity = round(yearly_capacity/total_sections)) |>
      rename("reporting_year" = "year")
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      mutate(total_years_offered = n())
    filter_subject <- left_join(course_reg_data, filter_subject, by=c("course", "reporting_year"))
    filter_subject <- filter_subject |> 
      group_by(course) |> 
      summarise(full_avg_capacity = round(sum(avg_capacity, na.rm = TRUE)/n()),
                Subject = first(Subject),
                total_years_offered =first(total_years_offered))
    filter_subject <- filter_subject |> 
      filter(full_avg_capacity >= input$capacity_openseats) |> 
      filter(Subject == input$select_subject_seats) |> 
      filter(total_years_offered >= input$years_offered_seats)
    updateSelectizeInput(session, "course_openseats", choices = unique(filter_subject$course))
  }) # end capacity edit for open seats graph
  # plot enrollment by semester
  output$plot <- renderPlotly({
      if (input$time == "Semester"){
        course_app_semester <- course_reg_data |> 
          rename("FA" = "fall_enrolled",
                 "SP" = "spring_enrolled")|>
          pivot_longer(cols = c(FA, SP), 
                       names_to = "semester", 
                       values_to = "semester_enrolled") |> 
          filter(course %in% input$courses) |> 
          mutate(year = ifelse(semester == "SP", reporting_year+1, reporting_year)) |>
          unite("term", c(year, semester)) 
        course_app_semester <- course_app_semester |>
          filter(semester_enrolled != 0)
        course_app_section <- course_reg_section |> 
          group_by(course, term) |> 
          mutate(cumulative_enrolled = cumsum(enrolled))
        course_app_section <- course_app_section |> 
          filter(course %in% input$courses)|>
          filter(enrolled != 0)
        course_app_section3 <- course_app_section |> 
          group_by(term) |> 
          summarise(total_sections = n())
        course_app_semester <- left_join(course_app_semester, course_app_section3,
                                         by = c("term"))
        course_app_semester <- course_app_semester |> 
          mutate(avg_section = round(semester_enrolled / total_sections))
        course_app_semester <- course_app_semester |> 
          mutate(year = parse_number(term),
                 semester = case_when(
                   str_detect(term, "FA") ~ "FA",
                   str_detect(term, "SP") ~ "SP"))
        course_app_semester <- course_app_semester |> 
          mutate(order = case_when(
          semester == "FA" ~ year + 1,
          semester == "SP" ~ year)) |> 
          mutate(term = fct_reorder(term, order, .desc = FALSE))
        if ("omit COVID-19 years" %in% input$sections){
          course_app_semester <- course_app_semester |> 
            filter(term != "2020_FA") |> 
            filter(term != "2021_SP")
          course_app_section <- course_app_section |> 
            filter(term != "2020_FA") |> 
            filter(term != "2021_SP")
        }
        course_app_semester <- course_app_semester |> 
          mutate(term = fct_reorder(term, order, .desc = FALSE))
        plot1 <- ggplot(data = course_app_semester, aes(x = term, y = semester_enrolled, group = 1)) +
          geom_line() +
          geom_point(data = course_app_semester, aes(x = term, y = semester_enrolled, group = 1)) +
          geom_point(data = course_app_section, aes(x = term, y = cumulative_enrolled, group = 1), 
                     alpha = 0.25, shape = 45)+
          geom_text(data = course_app_section, 
                    aes(x = term, y = cumulative_enrolled - 5, label = section), 
                    angle = 90, alpha = 0.5, size = 2,
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
        plot_nosection <- ggplot(data = course_app_semester, 
                                 aes(x = term, y = semester_enrolled, group = 1, 
                                     label = total_sections, label2 = avg_section)) +
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
          geom_point(data = course_app_section, aes(x = term, y = cumulative_enrolled, group = 1), 
                     alpha = 0.25, shape = 45)+
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
          geom_text(data = course_app_section, aes(x = term, y = cumulative_enrolled - 5, label = section), 
                    angle = 90, alpha = 0.5, size = 2,
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
          semester_plot <- ggplotly(plot1) |> 
            style(hoverinfo = "none",
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
        course_app_yearly <- course_reg_data |> 
          filter(course %in% input$courses)
        course_app_section2 <- course_reg_section |> 
          group_by(course, term) |> 
          mutate(cumulative_enrolled = cumsum(enrolled))
        course_app_section2 <- course_app_section2 |> 
          filter(course %in% input$courses)|>
          filter(enrolled != 0)
        course_app_section2 <- course_app_section2 |> 
          group_by(year) |> 
          summarise(total_sections = n()) |> 
          rename("reporting_year"="year")
        course_app_yearly <- left_join(course_app_yearly, course_app_section2,
                             by= c("reporting_year"))
        course_app_yearly <- course_app_yearly |> 
          mutate(avg_section = round(yearly_enrolled / total_sections))
        if ("omit COVID-19 years" %in% input$sections){
          course_app_yearly <- course_app_yearly |> 
            filter(reporting_year != 2020)
        }
        plot2 <-ggplot(data = course_app_yearly, aes(x = as.character(reporting_year), y = yearly_enrolled, group = 1, 
                                                     label = total_sections, label2 = avg_section)) +
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
      subject_data <- course_reg_data |> 
        mutate(level = case_when(
        as.numeric(`Course Number`) >= 1000 ~ as.numeric(`Course Number`) %/% 1000 - 1,
        as.numeric(`Course Number`) < 1000 ~ as.numeric(`Course Number`) %/% 100))
      subject_data <- subject_data |> 
        filter(Subject %in% input$discipline)
      subject_data <- subject_data |> 
        group_by(level, reporting_year) |> 
        summarise(enrolled = sum(yearly_enrolled))
      subject_data <- subject_data |> 
        filter(level <= 3)
      section_total <- course_reg_section |> 
        mutate(level = case_when(
        as.numeric(`Course Number`) >= 1000 ~ as.numeric(`Course Number`) %/% 1000 - 1,
        as.numeric(`Course Number`) < 1000 ~ as.numeric(`Course Number`) %/% 100))
      section_total <- section_total |> 
        filter(Subject %in% input$discipline)
      section_total <- section_total |> 
        group_by(year, level) |> 
        summarise(total_sections = n()) |> 
        rename("reporting_year" = "year")
      section_total <- section_total |> 
        filter(level <=3)
      subject_data <- left_join(subject_data, section_total, by = c("reporting_year", "level"))
      subject_data <- subject_data |> 
        mutate(avg_section = round((enrolled/total_sections)))
      if ("omit COVID-19 years" %in% input$discipline_settings){
        subject_data <- subject_data |> 
          filter(reporting_year != 2020)
      }
      subject_plot<- ggplot(data = subject_data, aes(x = reporting_year, y = enrolled, color = factor(level), 
                                                     label = total_sections, label2 = avg_section)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0, max(subject_data$enrolled))) +
        labs(title = glue::glue("Enrollment for ", input$discipline),
              color = "Course Level",
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
        mutate(year = ifelse(semester == "SP", reporting_year+1, reporting_year)) |>
        unite("term", c(year, semester)) 
      full_terms <- tibble(term = unique(course_app_semester$term))
      course_app_semester <- course_app_semester |>
        filter(course %in% input$course_openseats) |>
        filter(semester_enrolled != 0)
      course_app_semester <- course_app_semester |> 
        mutate(semester_capacity = if_else(str_detect(term, "FA"), fall_capacity, spring_capacity))
      course_app_section <- course_reg_section |> 
        group_by(course, term) |> 
        mutate(cumulative_enrolled = cumsum(enrolled))
      course_app_section <- course_app_section |> 
        filter(course %in% input$course_openseats)|>
        filter(enrolled != 0)
      course_app_section3 <- course_app_section |> 
        group_by(term) |> 
        summarise(total_sections = n())
      course_app_semester <- left_join(course_app_semester, course_app_section3,
                                       by = c("term"))
      course_app_semester <- course_app_semester |> 
        mutate(avg_section = round(semester_enrolled / total_sections))
      if ("omit COVID-19 years" %in% input$openseats_settings){
        course_app_semester <- course_app_semester |> 
          filter(term != "2020_FA") |> 
          filter(term != "2021_SP")
      }
      course_app_semester <- course_app_semester |> 
        complete(term = full_terms$term) 
      course_app_semester <- course_app_semester |> 
        mutate(year = parse_number(term),
             semester = case_when(
               str_detect(term, "FA") ~ "FA",
               str_detect(term, "SP") ~ "SP")) |> 
        filter(!is.na(year))
      course_app_semester <- course_app_semester |> 
        mutate(order = case_when(
        semester == "FA" ~ year + 1,
        semester == "SP" ~ year))
      course_app_semester <- course_app_semester |> 
        mutate(term = fct_reorder(term, order, .desc = FALSE))
      plot1 <- ggplot(data = course_app_semester, aes(x = term, 
                                                      label = total_sections, label2 = avg_section)) +
        geom_col(aes(y = semester_enrolled, fill = case_when(semester_enrolled > semester_capacity ~ "Over Enrollment",
                                                           semester_enrolled >= semester_capacity - total_sections & semester_enrolled <= semester_capacity ~ "Enrollment (full)",
                                                           semester_enrolled < semester_capacity ~ "Enrollment"))) +
        geom_col(aes(y = semester_capacity, fill = case_when(semester_enrolled > semester_capacity ~ "Over Enrollment", 
                                                           semester_enrolled < semester_capacity - total_sections ~"Empty Seats",
                                                           semester_enrolled >= semester_capacity - total_sections & semester_enrolled <= semester_capacity ~ "Empty Seats"
        )),alpha = 0.5) +
        scale_fill_manual(values = c("Empty Seats" = "lightgrey",
                                     "Enrollment"="darkgrey",
                                     "Enrollment (full)" = "lightpink",
                                     "Over Enrollment" = "red"),
                          breaks = c("Empty Seats", "Enrollment", "Enrollment (full)", "Over Enrollment"),
                          labels = c("Empty Seats", "Enrollment", "Enrollment (full)", "Over Enrollment")) +
        labs(
          title = glue::glue("Course Capacity for ", input$course_openseats),
          x = "Year",
          y = "Enrollment",
          fill = "") + 
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, vjust=0.5,hjust=1))
      ggplotly(plot1, tooltip = c("label", "y", "label2"))
    } # end semester
    else if (input$time_openseats == "Year") {
      course_app_yearly <- course_reg_data |> 
        filter(course %in% input$course_openseats)
      course_reg_section_yearly <- course_reg_section  |>
        filter(course %in% input$course_openseats) |> 
        group_by(year) |> 
        summarise(total_sections = n()) |> 
        rename("reporting_year"="year")
      course_app_yearly <- left_join(course_app_yearly, course_reg_section_yearly,
                           by= c("reporting_year"))
      course_app_yearly <- course_app_yearly |> 
        mutate(avg_section = round((yearly_enrolled/total_sections)))
      if ("omit COVID-19 years" %in% input$openseats_settings){
        course_app_yearly <- course_app_yearly |> 
          filter(reporting_year != 2020)
      }
      course_reg_fix <- course_reg_data |> 
        filter(!is.na(reporting_year))
      full_years <- data.frame(reporting_year = min(course_reg_fix$reporting_year):max(course_reg_fix$reporting_year))
      course_app_yearly <- course_app_yearly |> 
        complete(reporting_year = full_years$reporting_year)
      plot1 <- ggplot(data = course_app_yearly, aes(x = as.character(reporting_year), 
                                                    label = total_sections, label2 = avg_section)) +
        geom_col(aes(y = yearly_enrolled, fill = case_when(yearly_enrolled > yearly_capacity ~ "Over Enrollment",
                                                           yearly_enrolled >= yearly_capacity - total_sections & yearly_enrolled <= yearly_capacity ~ "Enrollment (full)",
                                                           yearly_enrolled < yearly_capacity ~ "Enrollment"))) +
        geom_col(aes(y = yearly_capacity, fill = case_when(yearly_enrolled > yearly_capacity ~ "Enrollment (full)", 
                                                           yearly_enrolled < yearly_capacity - total_sections ~"Empty Seats",
                                                           yearly_enrolled >= yearly_capacity - total_sections & yearly_enrolled <= yearly_capacity ~ "Empty Seats"
                                                           )) ,alpha = 0.5) +
        scale_fill_manual(values = c("Enrollment (full)" = "lightpink",
                                     "Over Enrollment" = "red",
                                     "Enrollment"="darkgrey", 
                                     "Empty Seats" = "lightgrey"),
                          breaks = c("Empty Seats", "Enrollment", "Enrollment (full)", "Over Enrollment"),
                          labels = c("Empty Seats", "Enrollment", "Enrollment (full)", "Over Enrollment")) +
        labs(
          title = glue::glue("Course Capacity for ", input$course_openseats),
          x = "Year",
          y = "Enrollment",
          fill = "") + 
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, vjust=0.5,hjust=1)) +
        guides(fill = guide_legend(title = ""))
      ggplotly(plot1, tooltip = c("label", "y", "label2"))
    }
  }) # end openseats plot

  } # end server

# add number of sections to discipline graph

shinyApp(ui, server)



