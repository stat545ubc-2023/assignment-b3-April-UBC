#assignment B3
#feature 1 - add a map of the US to the UI. This can be useful for users to visualize geographical location of the states and also make the app more visually interesting.
#feature 2 - add interactive bar graph. This can be useful to help users to visually compare murder arrest rates between the different states, and view states within a chosen arrest range.
#feature 3 - add interactive table. This can be useful to help users to quickly view the exact arrest numbers, compare the rate between the different states, and view states within a chosen arrest range.
#feature 4 - add sidebarPanel and mainPanel to reorganize the page to make it more visually aesthetic

library(shiny)
library(ggplot2)
library(dbplyr)
library(tidyverse)
library(here)

USArrests_edit <- read_csv(here('USArrests.csv'))

ui <- fluidPage(
  titlePanel("Murder Arrest Statistics in the US"),
  img(src = "map.png"),
  sidebarLayout(
    sidebarPanel(
      sliderInput('murder_slider', 'Murder arrests per 100,000 residents', min = 0, max = 20,
                  value = c(0, 5))
    ),
    mainPanel(
      plotOutput('murder_col'),
      tableOutput('murder_table')
    )
  )
)

server <- function(input, output) {
  observe(print(input$murder_slider))

  murder_filtered <- reactive({
    USArrests_edit %>%
      filter(Murder < input$murder_slider [2],
             Murder > input$murder_slider [1])
  })

  #murder bar graph
  output$murder_col <- renderPlot({
      murder_filtered() %>%
      ggplot(aes(y = Murder,
                 x = State)) +
        geom_col() +
      scale_x_discrete(guide = guide_axis(n.dodge=3))
  })

  #murder table
  output$murder_table <- renderTable({
    murder_filtered()
  })
}

shinyApp(ui = ui, server = server)

