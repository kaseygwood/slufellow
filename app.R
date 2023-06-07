library(shiny)

ui <- fluidPage(
  # plot enrollment by semester
  selectInput("course", label = "Select a Course", choices = course_reg_data$`Course Title`),
  plotOutput(outputId = "plot")
)

server <- function(input, output, session) {
  # plot enrollment by semester
  output$plot <- renderPlot({
    course_app <- course_reg_data |> filter(`Course Title` %in% input$course)
    ggplot(data = course_app, aes(x = `Acad Year Term`, y = Enrolled, group = 1)) +
      geom_line() +
      geom_point() +
      labs(title = glue::glue("Enrollment for ", input$course))
  })
}

shinyApp(ui, server)
