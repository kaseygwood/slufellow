library(shiny)
library(ggthemes)
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  tabsetPanel(
    tabPanel("Enrollment by Course",
             sidebarLayout(
               sidebarPanel(
                 tabsetPanel(
                   tabPanel("Yearly",
                            selectInput("courseYearly", label = "Select a Course", 
                                        choices = course_reg_data_yearly$`Course Title`,
                                        selected = course_reg_data_yearly$`Course Title`[1])),
                   tabPanel("By Semester",
                            selectInput("courseSemester", label = "Select a Course", 
                                        choices = course_reg_data$`Course Title`, 
                                        selected = course_reg_data$`Course Title`[1])),
                   tabPanel("Compare Semesters",
                            selectInput("course", label = "Select a Course", 
                                        choices = course_reg_data$`Course Title`, 
                                        selected = course_reg_data_fall$`Course Title`[1]))
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
    if (!is.null(input$courseSemester) && input$courseSemester %in% course_reg_data$`Course Title`){
      course_app_semester <- course_reg_data |> filter(`Course Title` %in% input$courseSemester) |> 
        arrange(`Reporting Year`)
      plot1 <- ggplot(data = course_app_semester, aes(x = `Acad Year Term`, y = Enrolled, group = 1)) +
        geom_line() +
        geom_point() +
        labs(title = glue::glue("Enrollment for ", input$courseSemester),
             x = "Semester") + 
        theme_economist() +
        scale_color_economist() +
        theme(axis.text.x = element_text(angle = 90)) 
      ggplotly(plot1)
    }
    else if(!is.null(input$courseYearly) && input$courseYearly %in% course_reg_data_yearly$`Course Title`){
        course_app_yearly <- course_reg_data_yearly |> filter(`Course Title` %in% input$courseYearly)
        plot2 <-ggplot(data = course_app_yearly, aes(x = `Reporting Year`, y = Enrolled, group = 1)) +
          geom_line() +
          geom_point() +
          labs(title = glue::glue("Enrollment for ", input$courseYearly)) + 
          theme_economist() +
          scale_color_economist() +
          theme(axis.text.x = element_text(angle = 90))
        ggplotly(plot2)
    }
    else if (!is.null(input$course) && 
             (input$course) %in% course_reg_data$`Course Title`){
      test1 <- course_reg_data_fall |> filter(`Course Title` %in% input$course)
      test2 <- course_reg_data_spring |> filter(`Course Title` %in% input$course)
      plot3 <- ggplot(data = course_reg_data, aes(x = `Reporting Year`, y = Enrolled, group = 1)) +
        geom_line(data = test1, colour = "red") +
        geom_line(data = test2) +
        geom_point(data = test1, colour = "red") +
        geom_point(data= test2) +
        labs(title = glue::glue("Enrollment for ", input$course)) +
        theme_economist() +
        scale_color_economist() +
        theme(axis.text.x = element_text(angle = 90))
      ggplotly(plot3)
    }
      
  })
  
}

shinyApp(ui, server)

