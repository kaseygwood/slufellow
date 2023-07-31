library(shiny)
library(ggthemes)
library(readr)
library(plotly)
library(tidyverse)

course_reg_data <- read_csv("full_course_reg_data.csv")




course_reg_section <- course_reg_data |> 
  pivot_longer((c(6:43)), names_to = "semester", values_to = "enrolled") |>
  filter(enrolled != 0) |>
  mutate(semester_year = ifelse(str_detect(semester, "SP"),
                                reporting_year + 1, reporting_year)) |> 
  separate(col = semester, into = c("semester", "section"), sep = "_") |> 
  mutate(year = reporting_year) |> 
  arrange(reporting_year, desc(semester)) |>
  unite("term", c(semester_year, semester)) |> 
  mutate(term = factor(term)) |>
  mutate(term = fct_inorder(term)) |>
  filter(term != "2020_FA") |> 
  filter(term != "2021_SP")




#### yearly data

course_app_yearly <- course_reg_data 

course_app_section2 <- course_reg_section |> 
  group_by(course, term) |> 
  mutate(cumulative_enrolled = cumsum(enrolled)) |> 
 ## filter(course %in% input$courses)|>
  filter(enrolled != 0) |> 
  group_by(year, course) |> 
  summarise(total_sections = n()) |>
  rename("reporting_year" = "year") |>
  ungroup()

course_app_yearly <- left_join(course_app_yearly, course_app_section2,
                               by= c("reporting_year", "course")) |> 
  mutate(avg_section = round(yearly_enrolled / total_sections)) |> 
  mutate(reporting_year = factor(reporting_year)) |>
  filter(reporting_year != "2020")

course_empty_yearly <- course_app_yearly |>
  mutate(full_ind = case_when(yearly_enrolled > yearly_capacity ~ "Full",
                              yearly_enrolled >= yearly_capacity - total_sections & yearly_enrolled <= yearly_capacity ~ "Full",
                              yearly_enrolled < yearly_capacity ~ "Not Full")) |>
  mutate(overenrollments = if_else(yearly_capacity < yearly_enrolled,
                                   true = yearly_enrolled - yearly_capacity,
                                   false = 0),
         empty_seats = if_else(yearly_capacity > yearly_enrolled,
                               true = yearly_capacity - yearly_enrolled,
                               false = 0)) |>
  relocate(overenrollments, empty_seats, full_ind) |>
  rename(enrollments = yearly_enrolled) |>
  pivot_longer(c(overenrollments, empty_seats, enrollments),
               names_to = "Bar Code",
               values_to = "n_students") |>
  rename(`Colour Code` = `Bar Code`,
         `Full?` = full_ind) |>
  mutate(`Colour Code` = fct_relevel(`Colour Code`,
                                     c("empty_seats", "overenrollments",
                                       "enrollments"))) 






course_app_semester <- course_reg_data |> 
  rename("FA" = "fall_enrolled",
         "SP" = "spring_enrolled") |>
  pivot_longer(cols = c(FA, SP), 
               names_to = "semester", 
               values_to = "semester_enrolled") |> 
  mutate(year = ifelse(semester == "SP", reporting_year + 1,
                       reporting_year)) |>
  arrange(year, desc(semester)) |>
  unite("term", c(year, semester)) |> 
  mutate(term = factor(term)) |>
  mutate(term = fct_inorder(term)) |>
  filter(term != "2020_FA") |> 
  filter(term != "2021_SP") |>
  filter(semester_enrolled != 0)

course_app_section <- course_reg_section |> 
  group_by(course, term) |> 
  mutate(cumulative_enrolled = cumsum(enrolled)) |> 
  filter(enrolled != 0)

course_app_section3 <- course_app_section |> 
  group_by(term, course) |> 
  summarise(total_sections = n()) |>
  ungroup()

course_app_semester <- left_join(course_app_semester,
                                 course_app_section3,
                                 by = c("term", "course")) |> 
  mutate(avg_section = round(semester_enrolled / total_sections)) |> 
  mutate(year = parse_number(as.character(term)),
         semester = case_when(
           str_detect(term, "FA") ~ "FA",
           str_detect(term, "SP") ~ "SP")) |> 
  mutate(order = case_when(
    semester == "FA" ~ year + 1,
    semester == "SP" ~ year)) 
 
 ## mutate(term = fct_reorder(term, order, .desc = FALSE))



course_empty_df <- course_app_semester |>
  mutate(semester_capacity = if_else(str_detect(term, "FA"),
                                     fall_capacity,
                                     spring_capacity)) |>
mutate(full_ind = case_when(semester_enrolled > semester_capacity ~ "Full",
                            semester_enrolled >= semester_capacity - total_sections & semester_enrolled <= semester_capacity ~ "Full",
                            semester_enrolled < semester_capacity ~ "Not Full")) |>
  mutate(overenrollments = if_else(semester_capacity < semester_enrolled,
                                   true = semester_enrolled - semester_capacity,
                                   false = 0),
         empty_seats = if_else(semester_capacity > semester_enrolled,
                               true = semester_capacity - semester_enrolled,
                               false = 0)) |>
  relocate(overenrollments, empty_seats, full_ind) |>
  rename(enrollments = semester_enrolled) |>
  pivot_longer(c(overenrollments, empty_seats, enrollments),
               names_to = "Bar Code",
               values_to = "n_students") |>
  rename(`Colour Code` = `Bar Code`,
         `Full?` = full_ind) |>
  mutate(`Colour Code` = fct_relevel(`Colour Code`,
                                     c("empty_seats", "overenrollments",
                                       "enrollments"))) 
  


sidebar_panel_ui <- sidebarPanel(
  radioButtons("time", "Choose your preferred time frame.",
               choices = c("Year", "Semester")),
  selectizeInput("select_subject", "Select a Subject",
                 c("CS", "DATA", "MATH", "STAT"),
                 ##  choices = unique(course_reg_data$Subject),
                 selected = "STAT"),
  selectizeInput("courses", label = "Select a Course", 
                 choices = unique(course_reg_data$course),
                 selected = "Applied Statistics"),
  sliderInput("capacity", "Minimum Course Capacity", 
              min = 1, 
              max = 60, 
              value = 6),
  sliderInput("years_offered", "Minimum Years Offered", 
              min = 1, 
              max = 9, 
              value = 3),
  checkboxGroupInput("sections", "Graph Settings:", 
                     choices = NULL,
                     selected = "basic"))

sidebar_panel_ui_seats <- sidebarPanel(
  radioButtons("time_seats", "Choose your preferred time frame.",
               choices = c("Year", "Semester")),
  selectizeInput("select_subject_seats", "Select a Subject",
                 c("CS", "DATA", "MATH", "STAT"),
                 ##  choices = unique(course_reg_data$Subject),
                 selected = "STAT"),
  selectizeInput("courses_seats", label = "Select a Course", 
                 choices = unique(course_reg_data$course),
                 selected = "Applied Statistics"),
  sliderInput("capacity_seats", "Minimum Course Capacity", 
              min = 1, 
              max = 60, 
              value= 6),
  sliderInput("years_offered_seats", "Minimum Years Offered", 
              min = 1, 
              max = 9, 
              value = 3)
  )

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
    tabPanel("Open Seats",
             sidebarLayout(
               sidebar_panel_ui_seats, 
               mainPanel(plotlyOutput(outputId = "openseats_plot"))
             ) #side layout
    ),
    tabPanel("Enrollment by Course",
             sidebarLayout(
               sidebar_panel_ui,
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
                 textOutput(outputId = "data_description")
               ),
               mainPanel(plotlyOutput(outputId = "discipline_plot"))
             )) # discipline tabPanel
     # open seats tabPanel
  ) #tabsetPanel
) #end fluidPage


server <- function(input, output, session) {

  observeEvent(eventExpr = {
    c(input$select_subject, input$years_offered, input$capacity)
  }, {
    
    filter_subject <- course_reg_section |> 
      group_by(year, course) |> 
      summarise(total_sections = n(),
                yearly_capacity = first(yearly_capacity),
                avg_capacity = round(yearly_capacity / total_sections)) |>
      rename("reporting_year" = "year") |> 
      group_by(course) |> 
      mutate(total_years_offered = n())
    
    filter_subject <- left_join(course_reg_data, filter_subject,
                                by=c("course", "reporting_year")) |> 
      group_by(course) |> 
      summarise(full_avg_capacity = round(sum(avg_capacity,
                                              na.rm = TRUE) / n()),
                Subject = first(Subject),
                total_years_offered =first(total_years_offered)) |> 
      filter(full_avg_capacity >= input$capacity) |> 
      filter(Subject == input$select_subject) |> 
      filter(total_years_offered >= input$years_offered)
    
    updateSelectizeInput(session, "courses",
                         choices = unique(filter_subject$course))
  })
  
  # description_text <- "*The data being used in this plot does not include SYE courses 
  # (these courses are often independent and have low enrollment), 
  # therefore the level 4 courses are not pictured."
  # output$data_description <- renderText({
  #   description_text
  # })
  
  observeEvent(eventExpr = {
    c(input$select_subject_seats,
      input$years_offered_seats, input$capacity_seats)
  }, {
    
    filter_subject <- course_reg_section |> 
      group_by(year, course) |> 
      summarise(total_sections = n(),
                yearly_capacity = first(yearly_capacity),
                avg_capacity = round(yearly_capacity / total_sections)) |>
      rename("reporting_year" = "year") |> 
      group_by(course) |> 
      mutate(total_years_offered = n())
    
    filter_subject <- left_join(course_reg_data, filter_subject,
                                by=c("course", "reporting_year")) |> 
      group_by(course) |> 
      summarise(full_avg_capacity = round(sum(avg_capacity,
                                              na.rm = TRUE) / n()),
                Subject = first(Subject),
                total_years_offered =first(total_years_offered)) |> 
      filter(full_avg_capacity >= input$capacity_seats) |> 
      filter(Subject == input$select_subject_seats) |> 
      filter(total_years_offered >= input$years_offered_seats)
    
    updateSelectizeInput(session, "courses_seats",
                         choices = unique(filter_subject$course))
  })
  
  
  # description_text <- "*The data being used in this plot does not include SYE courses 
  # (these courses are often independent and have low enrollment), 
  # therefore the level 4 courses are not pictured."
  # output$data_description <- renderText({
  #   description_text
  # })
  
  
  observeEvent(input$time, {
    if (input$time == "Semester") {
      choices <- c("show class sections",
                   "label sections")
    }
    else if (input$time == "Year"){
      choices <- NULL
    }
    updateCheckboxGroupInput(session, "sections",
                             choices = choices, selected = NULL)
  }) # end checkboxinputupdate
  
 # end checkboxinputupdate
  
  
  
  # plot enrollment by semester
  output$plot <- renderPlotly({
    if (input$time == "Semester") {
      
      course_app_one <- course_app_semester |>
       ## filter(course == "STAT-113: Applied Statistics")
        filter(course %in% input$courses) 

      course_app_section_one <- course_app_section |>
     ##   filter(course == "STAT-113: Applied Statistics")
        filter(course %in% input$courses) 
      
      ## , label = total_sections,
     ## label2 = avg_section
      plot_nosection <- ggplot(data = course_app_one, 
                               aes(x = term, y = semester_enrolled,
                                   group = 1)) +
        geom_line() +
        geom_point() +
        scale_x_discrete(limits = levels(course_app_semester$term),
                         drop = FALSE) +
        scale_y_continuous(limits= c(0, max(course_app_one$semester_enrolled))) +
       labs(title = glue::glue("Enrollment for ", input$courses),
             x = "Semester",
             y = "Enrollment",
             colour = "Semester") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                         hjust = 1),
              axis.text.y = element_text(hjust = 1))
      

      
      
      if ("show class sections" %in% input$sections && "label sections" %in% input$sections) {
        
        static_plot <- plot_nosection +
          geom_point(data = course_app_one,
                     aes(x = term, y = semester_enrolled, group = 1)) +
          geom_point(data = course_app_section_one,
                     aes(x = term, y = cumulative_enrolled, group = 1), 
                     alpha = 0.25, shape = 45) +
          geom_text(data = course_app_section_one, 
                    aes(x = term, y = cumulative_enrolled - 5,
                        label = section), 
                    angle = 90, alpha = 0.5, size = 2,
                    hjust = 0, vjust = 0.5, nudge_x = -0.2) 
        semester_plot <- ggplotly(static_plot) |> 
          style(hoverinfo = "none")
      }
      
      else if ("show class sections" %in% input$sections){
        
        static_plot <- plot_nosection + 
          geom_point(data = course_app_one, aes(x = term, y = semester_enrolled, group = 1)) +
          geom_point(data = course_app_section_one, aes(x = term, y = cumulative_enrolled, group = 1, label = NULL, label2 = NULL), 
                     alpha = 0.25, shape = 45)
        semester_plot <- ggplotly(static_plot) |>
          style(hoverinfo = "none")
      }
      
      else if ("label sections" %in% input$sections){
        static_plot <- plot_nosection +
          geom_point(data = course_app_one,
                     aes(x = term, y = semester_enrolled, group = 1)) +
          geom_text(data = course_app_section_one, aes(x = term,
                                                       y = cumulative_enrolled - 5, label = section), 
                    angle = 90, alpha = 0.5, size = 2,
                    hjust = 0, vjust = 0.5, nudge_x = -0.2) 
        semester_plot <- ggplotly(static_plot) |>
          style(hoverinfo = "none")
      }
      
      else {
        semester_plot <- ggplot(data = course_app_one, 
                                 aes(x = term, y = semester_enrolled,
                                     group = 1,
                                     label = total_sections,
                                     label2 = avg_section)) +
          geom_line() +
          geom_point() +
          scale_x_discrete(limits = levels(course_app_semester$term),
                           drop = FALSE) +
          scale_y_continuous(limits= c(0, max(course_app_one$semester_enrolled))) +
          labs(##title = glue::glue("Enrollment for ", input$courses),
            x = "Semester",
            y = "Enrollment",
            colour = "Semester") + 
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                           hjust = 1),
                axis.text.y = element_text(hjust = 1))
        ggplotly(semester_plot)
      }
      
      return(semester_plot)
      
    }
    
    else if (input$time == "Year") {
      
      
      course_app_one_year <- course_app_yearly |> 
        filter(course %in% input$courses)
      plot2 <- ggplot(data = course_app_one_year,
                      aes(x = reporting_year,
                          y = yearly_enrolled, group = 1,
                          label = total_sections, label2 = avg_section)) +
        geom_line() +
        geom_point() +
        labs(title = glue::glue("Enrollment for ", input$courses),
             x = "Year",
             y = "Enrollment") + 
        scale_y_continuous(limits =
                             c(0, max(course_app_one_year$yearly_enrolled))) + 
      scale_x_discrete(breaks = levels(course_app_yearly$reporting_year),
                       drop = FALSE) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90,
                                         vjust = 0.5, hjust = 1))
      ggplotly(plot2)
    } # year
  } # input courses
  ) # render plotly
  
  output$discipline_plot <- renderPlotly({
    if (input$discipline %in% course_reg_data$Subject){
      subject_data <- course_reg_data |> 
        filter(Subject %in% input$discipline)
      subject_data <- subject_data |> 
        group_by(`Course Level1`, reporting_year) |> 
        summarise(enrolled = sum(yearly_enrolled))
      section_total <- course_reg_section |> 
        filter(Subject %in% input$discipline)
      section_total <- section_total |> 
        group_by(year, `Course Level1`) |> 
        summarise(total_sections = n()) |> 
        rename("reporting_year" = "year")
      subject_data <- left_join(subject_data, section_total, by = c("reporting_year", "Course Level1"))
      subject_data <- subject_data |> 
        mutate(avg_section = round((enrolled/total_sections)))
      subject_data <- subject_data |> 
        filter(reporting_year != 2020)
      subject_plot<- ggplot(data = subject_data, aes(x = reporting_year, y = enrolled, color = factor(`Course Level1`), 
                                                     label = total_sections, label2 = avg_section)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0, max(subject_data$enrolled))) +
        labs(title = glue::glue("Enrollment for ", input$discipline),
             color = "Course Level",
             x = "Year",
             y = "Enrollment") +
        theme_minimal()+
        scale_colour_viridis_d(option = "B")
      ggplotly(subject_plot, tooltip = c("label", "y", "label2"))
    }
  }) # end discipline plotly
  
  
  output$openseats_plot <- renderPlotly({
    if (input$time_seats == "Semester") {
      
      course_app_one <- course_empty_df |>
        filter(course %in% input$courses_seats) ##|> 
       ## filter(course == "STAT-113: Applied Statistics") 
      
      course_app_section_one <- course_app_section |> 
        filter(course %in% input$courses_seats)
      
      
      plot1 <- ggplot(data = course_app_one,
                      aes(x = term, label = total_sections, label2 = avg_section)) +
        geom_col(aes(y = n_students,
                     fill = `Colour Code`##,
                 ##    linetype = `Full?`
                     ),
                 colour = "grey50") +
        scale_fill_manual(values = c("lightgrey", "red", "lightpink")) + 
       ## scale_linetype_manual(values = c("solid", "blank")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        scale_x_discrete(breaks = levels(course_app_semester$term),
                         drop = FALSE) +
        labs(
          title = glue::glue("Course Capacity for ", input$courses_seats),
          x = "Semester",
          y = "Students",
          fill = "") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90,
                                         vjust = 0.5, hjust = 1))## +
   ## guides(fill = guide_legend(override.aes = list(pattern = "none"), order = 1),
         ##    linetype = guide_legend(override.aes = list(fill = "white")), order = 2)
      ggplotly(plot1, tooltip = c("label", "label2"))
    } # end semester
    
    else if (input$time_seats == "Year") {
      
      course_empty_df <- course_empty_yearly |> 
        ##filter(course == "STAT-113: Applied Statistics")
        filter(course %in% input$courses_seats) 

      plot1 <- ggplot(data = course_empty_df,
                      aes(x = reporting_year, 
                          label = total_sections, label2 = avg_section)) +
        geom_col(aes(y = n_students,
                     fill = `Colour Code`##,
                     ##    linetype = `Full?`
        ),
        colour = "grey50") +
        scale_fill_manual(values = c("lightgrey", "red", "lightpink")) +
        labs(
          title = glue::glue("Course Capacity for ", input$courses_seats),
          x = "Year",
          y = "Students",
          fill = "") + 
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90,
                                         vjust = 0.5, hjust = 1)) +
        scale_x_discrete(breaks = levels(course_empty_yearly$reporting_year),
                         drop = FALSE) 
      ggplotly(plot1, tooltip = c("label", "label2"))
    }
  }) # end openseats plot
  
} # end server

# add number of sections to discipline graph

shinyApp(ui, server)
