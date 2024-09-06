library(shiny)
library(tidyverse)

# ASPEN THIS IS WHERE YOU'LL LOAD YOUR DATA. REMEMBER, IT NEEDS TO BE IN app/!!
aspens_birdybirds <- read_csv("app/IDONTKNOWSOMESHITIGUESS.csv") %>% 
  mutate() # AS NECESSARY. MAYBE cut() OR SOMETHING HOW THE FUCK WOULD I KNOW
# THIS VARIABLE, aspens_birdybirds NEEDS TO BE RENAMED OBVIOUSLY BUT IT'S A
# GLOBABL VARIABLE NOW MEANING YOU CAN REFERENCE IT IN server()

ui <- fluidPage(
  titlePanel("Pacific Seabird Species Prioritization"),
  sidebarLayout(
    sidebarPanel(
      # Sensitivity
      h2("Sensitivity"),
      numericInput("sens_low", "Low", 0.65, min = 0, max = 1, step = 0.05),
      p("High sensivity is the inverse of low sensitivity."),
      textOutput("sens_high"),
      # Threat
      h2("Threat"),
      p("NT is standardized at 1. Apply a fixed ratio to step between the other categories."),
      numericInput("threat_ratio", "Threat ratio", 1.2, min = 1, max = 10, step = 0.01),
      textOutput("threat_lc"),
      textOutput("threat_nt"),
      textOutput("threat_vu"),
      textOutput("threat_en"),
      textOutput("threat_cr"),
      width = 2
    ),
    mainPanel(
      fluidPage(
        fluidRow(
          column(6, plotOutput("priority_plot")),
          column(6, 
                 DT::DTOutput("extreme_table"),
                 DT::DTOutput("high_table"),
                 DT::DTOutput("moderate_table"),
                 DT::DTOutput("low_table"),)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Sidebar
  
  # Sensitivity
  output$sens_high <- renderText({
    sens_low <- input$sens_low
    if (sens_low == 0) sens_low <- 0.05
    sprintf("%0.2f", 1 / sens_low)
  }) #input$sens_low is sensitivity low, sensitivity high is 1/input$sens_low
  # Threat
  output$threat_lc <- renderText({
    sprintf("LC: %0.2f", input$threat_ratio^-1) #input$threat_ratio^-1 this is the thing that becomes the input in the plot for the thing that people will change
  })
  output$threat_nt <- renderText({
    sprintf("NT: %0.2f", input$threat_ratio^0)
  })
  output$threat_vu <- renderText({
    sprintf("VU: %0.2f", input$threat_ratio^1)
  })
  output$threat_en <- renderText({
    sprintf("EN: %0.2f", input$threat_ratio^2)
  })
  output$threat_cr <- renderText({
    sprintf("CR: %0.2f", input$threat_ratio^3)
  })
  
  # Main panel
  output$priority_plot <- renderPlot({
    # Code to create plot goes here ASPEN, YOUR MISSION (SHOULD YOU CHOOSE TO
    # ACCEPT IT) IS TO PORT YOUR PRIORITY PLOT CODE HERE. FIGURE OUT WHAT THE
    # INPUT DATA FOR YOUR GGPLOT IS. FIGURE OUT HOW TO PUT THOSE DATA INTO A CSV
    # FILE THAT CAN LIVE IN app/.
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point(aes(color = cyl))
    # WHATEVER YOU RENAME aspens_birdybirds TO, THAT'S WHAT YOU'LL USE HERE
  })
  
  # LATER MAX WILL SHOW YOU HOW TO PUT THE TABLES HERE, THAT SHOULD BE EASIER
}

shinyApp(ui, server)
