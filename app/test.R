
library(tidyverse)
library(shiny)
library(dplyr)

# ASPEN THIS IS WHERE YOU'LL LOAD YOUR DATA. REMEMBER, IT NEEDS TO BE IN app/!!
alldat <- read_csv("cleaned_data.csv")



#aspens_birdybirds <- read_csv("app/IDONTKNOWSOMESHITIGUESS.csv") %>% 
# mutate() # AS NECESSARY. MAYBE cut() OR SOMETHING HOW THE FUCK WOULD I KNOW
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
  
  
  # Reactive expression to create the lookup table based on user input
  lookup <- reactive({
    tibble(
      iucn_status = c("LC", "NT", "VU", "EN", "CR"),
      iucn_value = c(input$threat_ratio^-1, input$threat_ratio^0, input$threat_ratio^1, 
                     input$threat_ratio^2, input$threat_ratio^3)
    )
  })
  
  # Reactive expression for rescaling the data
  rescaled_dat <- reactive({
    alldat %>% 
      filter(!is.na(exposure_model)) %>% 
      left_join(lookup(), by = "iucn_status") %>% # Use lookup() to access the reactive expression
      mutate(
        rescaled_propALL = (propALL - min(propALL)) / (max(propALL) - min(propALL)) + 0.0001,
        rescaled_DV = (DV - min(DV)) / (max(DV) - min(DV)) + 0.0001,
        rescaled_CV = (CV - min(CV)) / (max(CV) - min(CV)) + 0.0001,
        highest_sens = pmax(rescaled_CV, rescaled_DV),
        highest_sens_source = case_when(
          rescaled_DV > rescaled_CV ~ "DV",
          rescaled_CV > rescaled_DV ~ "CV",
          TRUE ~ "tie"
        ),
        rescaled_sensitivity = input$sens_low + (highest_sens - min(highest_sens)) * 
          (1/input$sens_low - input$sens_low) / 
          (max(highest_sens) - min(highest_sens))
      )
  })
  
  prioritizationdf <- reactive({
    rescaled_dat() %>%
      mutate(
        es = rescaled_propALL * rescaled_sensitivity,
        est = rescaled_propALL * rescaled_sensitivity * iucn_value,
        bin = cut(est, 4, labels = c("Low", "Moderate", "High", "Extreme"))
      )
  })
  
  # Main panel
  output$priority_plot <- renderPlot({
    prioritizationdf() %>%
      mutate(origin = factor(rescaled_propALL)) %>%
      pivot_longer(c(rescaled_propALL, es, est), names_to = "x", values_to = "y") %>%
      mutate(x = factor(x, levels = c("rescaled_propALL", "es", "est"))) %>%
      ggplot(aes(x, y, group = alpha_code, color = origin)) +
      geom_line(aes(color = origin), show.legend = FALSE) +
      geom_point(aes(fill = bin), shape = 21, color = "white", size = 6) +
      geom_text(data = . %>%
                  group_by(alpha_code) %>%
                  filter(row_number() == n()),  # Label the last point
                aes(label = alpha_code), vjust = -0.5, hjust = -1, size = 1.5) +
      theme_classic() +
      guides(color = "none") +
      theme(
        legend.key.size = unit(0.1, 'cm'),
        legend.text = element_text(size = 6)
      ) +
      scale_x_discrete(labels = c(
        "rescaled_propALL" = "Exposure",
        "es" = "Exposure * Sensitivity",
        "est" = "Exposure * Sensitivity * Threat"
      ))
  })
}

shinyApp(ui, server)