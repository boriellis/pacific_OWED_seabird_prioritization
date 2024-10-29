
library(tidyverse)
library(shiny)
library(dplyr)

# Load the data (from the app folder, make sure it's the right csv!)
alldat <- read_csv("cleaned_data.csv")


ui <- fluidPage(
  titlePanel("Pacific Seabird Species Prioritization"),
  sidebarLayout(
    sidebarPanel(
      #select input values value
      h2("Select Inputs"),
      h3("Exposure"),
      p("Select which region you're interested in. The default is set to consider overlap with all wind energy development areas in the Pacific Outer Continental Shelf Region, but you can also select by state or individual lease areas."),
      selectInput("exposure_column", "Select Exposure Source:", 
                  choices = c(
                    "CA Humboldt - OCS-P 0561" = "prop0561", 
                    "CA Humboldt - OCS-P 0562" = "prop0562", 
                    "CA Morro Bay - OCS-P 0563" = "prop0563", 
                    "CA Morro Bay - OCS-P 0564" = "prop0564", 
                    "CA Morro Bay - OCS-P 0565" = "prop0565", 
                    "OR Coos Bay - OCS-P 0566" = "prop0566", 
                    "OR Brookings - OCS-P 0567" = "prop0567", 
                    "All California Sites" = "propCA", 
                    "All Oregon Sites" = "propOR", 
                    "All Pacific Outer Continental Shelf Sites" = "propALL" 
                  ),  # specify the options
                  selected = "propALL"),  # set default value
      h3("Sensitivity"),
      p("Select which value you're interested in using for sensitivity. The default is the combination of collision and displacement sensitivity for each species (each rescaled to 0.5 and summed). You can also choose to work with whichever sensitivity value is higher for a species (when both are rescaled to 1), or select to use either collision or displacement if you're only interested in one metric."),
      selectInput("sens_column", "Select Sensitivity Source:", 
                  choices = c(
                    "Rescaled Displacement Sensitivity" = "rescaled_DV", 
                    "Rescaled Collision Sensitivity" = "rescaled_CV", 
                    "Summed Sensitivity" = "summed_sens", 
                    "Highest Sensitivity" = "highest_sens"
                  ),  # specify the options
                  selected = "summed_sens"),  # set default value
      #pick weights
      h2("Adjust Weights"),
      #Sensitivity
      h3("Sensitivity"),
      numericInput("sens_low", "Low", 0.62, min = 0, max = 1, step = 0.05),
      p("High sensivity is the inverse of low sensitivity:"),
      textOutput("sens_high"),
      # Threat
      h3("Threat"),
      p("NT is standardized at 1. Apply a fixed ratio to step between the other categories."),
      numericInput("threat_ratio", "Threat ratio", 1.22, min = 1, max = 10, step = 0.01),
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
          column(6, plotOutput("priority_plot", height = "2700px")),
          column(6, 
                 # Add headers above each table
                 h3("Extreme Priority"),
                 DT::DTOutput("extreme_table"),
                 h3("High Priority"),
                 DT::DTOutput("high_table"),
                 h3("Moderate Priority"),
                 DT::DTOutput("moderate_table"),
                 h3("Low Priority"),
                 DT::DTOutput("low_table"))
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
        #rescaled_propALL = (propALL - min(propALL)) / (max(propALL) - min(propALL)) + 0.0001, This was to rescale the expsure but I maybe am just giving up on that
        rescaled_DV = (DV - min(DV)) / (max(DV) - min(DV)) + 0.0001,
        rescaled_CV = (CV - min(CV)) / (max(CV) - min(CV)) + 0.0001,
        summed_sens = (rescaled_DV/2 + rescaled_CV/2), 
        highest_sens = pmax(rescaled_CV, rescaled_DV),
        highest_sens_source = case_when(
          rescaled_DV > rescaled_CV ~ "DV",
          rescaled_CV > rescaled_DV ~ "CV",
          TRUE ~ "tie"
        ),
        # Dynamically select the exposure based on user input from the dropdown
        selected_exposure = case_when(
          input$exposure_column == "prop0561" ~ prop0561,
          input$exposure_column == "prop0562" ~ prop0562,
          input$exposure_column == "prop0563" ~ prop0563,
          input$exposure_column == "prop054" ~ prop0564,
          input$exposure_column == "prop0565" ~ prop0565,
          input$exposure_column == "prop0566" ~ prop0566,
          input$exposure_column == "prop057" ~ prop0567,
          input$exposure_column == "propOR" ~ propOR,
          input$exposure_column == "propCA" ~ propCA,
          input$exposure_column == "propALL" ~ propALL,
        ),
        # Dynamically select the column based on user input from the dropdown
        selected_sensitivity = case_when(
          input$sens_column == "rescaled_DV" ~ rescaled_DV,
          input$sens_column == "rescaled_CV" ~ rescaled_CV,
          input$sens_column == "summed_sens" ~ summed_sens,
          input$sens_column == "highest_sens" ~ highest_sens
        ),
        
        # Use the dynamically selected column for sensitivity calculation
        rescaled_sensitivity = input$sens_low + (selected_sensitivity - min(selected_sensitivity)) * 
          (1/input$sens_low - input$sens_low) / 
          (max(selected_sensitivity) - min(selected_sensitivity))
      )
  })
  
  prioritizationdf <- reactive({
    rescaled_dat() %>%
      mutate(
        es = selected_exposure * rescaled_sensitivity,
        est = selected_exposure * rescaled_sensitivity * iucn_value,
        bin = cut(est, 4, labels = c("Low", "Moderate", "High", "Extreme"))
      )
  })
  
  # Create tables for each priority level
  output$extreme_table <- DT::renderDT({
    DT::datatable(
      setNames(
        prioritizationdf() %>%
          filter(bin == "Extreme") %>%
          select(common_name, selected_exposure, rescaled_CV, rescaled_DV, iucn_status, est) %>%
          arrange(desc(est)),
        c("Species", "Exposure", "Collision Vulnerability", "Displacement Vulnerability", "IUCN Status", "Priority Score")
      ),
      options = list(searching = FALSE, paging = FALSE)
    ) %>% 
      DT::formatRound(columns = c("Exposure", "Collision Vulnerability", "Displacement Vulnerability", "Priority Score"), digits = 4)
  })
  
  
  output$high_table <- DT::renderDT({
    DT::datatable(
      setNames(
        prioritizationdf() %>%
          filter(bin == "High") %>%
          select(common_name, selected_exposure, rescaled_CV, rescaled_DV, iucn_status, est) %>%
          arrange(desc(est)),
        c("Species", "Exposure", "Collision Vulnerability", "Displacement Vulnerability", "IUCN Status", "Priority Score")
      ),
      options = list(searching = FALSE, paging = FALSE)
    ) %>% 
      DT::formatRound(columns = c("Exposure", "Collision Vulnerability", "Displacement Vulnerability", "Priority Score"), digits = 4)
  })

  output$moderate_table <- DT::renderDT({
    DT::datatable(
      setNames(
        prioritizationdf() %>%
          filter(bin == "Moderate") %>%
          select(common_name, selected_exposure, rescaled_CV, rescaled_DV, iucn_status, est) %>%
          arrange(desc(est)),
        c("Species", "Exposure", "Collision Vulnerability", "Displacement Vulnerability", "IUCN Status", "Priority Score")
      ),
      options = list(searching = FALSE, paging = FALSE)
    ) %>% 
      DT::formatRound(columns = c("Exposure", "Collision Vulnerability", "Displacement Vulnerability", "Priority Score"), digits = 4)
  })
  
  
  output$low_table <- DT::renderDT({
    DT::datatable(
      setNames(
        prioritizationdf() %>%
          filter(bin == "Low") %>%
          select(common_name, selected_exposure, rescaled_CV, rescaled_DV, iucn_status, est) %>%
          arrange(desc(est)),
        c("Species", "Exposure", "Collision Vulnerability", "Displacement Vulnerability", "IUCN Status", "Priority Score")
      ),
      options = list(searching = FALSE, paging = FALSE)
    ) %>% 
      DT::formatRound(columns = c("Exposure", "Collision Vulnerability", "Displacement Vulnerability", "Priority Score"), digits = 4)
  })

  
  # Main panel
  output$priority_plot <- renderPlot({
    prioritizationdf() %>%
      mutate(origin = factor(selected_exposure)) %>%
      pivot_longer(c(selected_exposure, es, est), names_to = "x", values_to = "y") %>%
      mutate(x = factor(x, levels = c("selected_exposure", "es", "est"))) %>%
      ggplot(aes(x, y, group = alpha_code, color = origin)) +
      geom_line(aes(color = origin), show.legend = FALSE) +
      geom_point(aes(fill = bin), shape = 21, color = "white", size = 6) +
      geom_text(data = . %>%
                  group_by(alpha_code) %>%
                  filter(row_number() == n()),  # Label the last point
                aes(label = alpha_code), 
                vjust = 0.5, 
                hjust = -2, 
                size = 4, 
                position = position_jitter(width = 0.2)) +
      theme_classic() +
      guides(color = "none") +
      theme(
        legend.key.size = unit(0.1, 'cm'),
        legend.text = element_text(size = 6)
      ) +
      scale_x_discrete(labels = c(
        "selected_exposure" = "Exposure",
        "es" = "Exposure * Sensitivity",
        "est" = "Exposure * Sensitivity * Threat"
      ))
  })
}

shinyApp(ui, server)



