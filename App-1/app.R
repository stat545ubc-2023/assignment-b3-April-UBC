library(shiny)
library(ggplot2)
library(dbplyr)
library(tidyverse)
library(here)

USArrests_edit <- read_csv(here('USArrests.csv'))

ui <- fluidPage(
  titlePanel("Murder Arrest Statistics in the US"),
  sliderInput('murder_slider', 'Murder arrests per 100,000 residents', min = 0, max = 20,
              value = c(0, 5)),
  plotOutput('murder_col'),
  tableOutput('murder_table')
)

server <- function(input, output) {
  observe(print(input$murder_slider))

  #murder bar graph
  output$murder_col <- renderPlot({
    USArrests_edit %>%
      filter(Murder < input$murder_slider [2],
             Murder > input$murder_slider [1]) %>%
      ggplot(aes(y = Murder,
                               x = State)) +
        geom_col() +
      scale_x_discrete(guide = guide_axis(n.dodge=3))
  })

  #murder table
  output$murder_table <- renderTable({
    USArrests_edit %>%
      filter(Murder < input$murder_slider [2],
             Murder > input$murder_slider [1])
  })
}

shinyApp(ui = ui, server = server)

