
library(tidyverse)
library(shiny)
library(plotly)

# Load the data (from the app folder, make sure it's the right csv!)
app_data <- readRDS("app_data.rds")
sp <- read_csv(here::here("data/raw_data/total_sp_list.csv"))

ui <- fluidPage(
  titlePanel("Pacific Seabird Species Prioritization"),
  h4("Please read the description of the tool and how it works ",
     a("here.", href = "https://boriellis.github.io/pacific_OWED_seabird_prioritization/", target = "_blank")),
  
  fluidRow(
    # Left column - controls
    column(3,
           wellPanel(
             h3("Select Inputs"),
             
             h4("Exposure"),
             p("Select which region you're interested in."),
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
                         ),
                         selected = "propALL"),
             
             h4("Sensitivity"),
             p("Select which sensitivity metric to use. Summed is recommended."),
             selectInput("sens_column", "Select Sensitivity Source:", 
                         choices = c(
                           "Summed Sensitivity" = "summed_sens",
                           "Highest Sensitivity" = "highest_sens",
                           "Collision Sensitivity" = "rescaled_CV", 
                           "Displacement Sensitivity" = "rescaled_DV"
                         ),
                         selected = "summed_sens"),
             
             h3("Adjust Weights"),
             p("Each component is rescaled to 0.5–2 before being multiplied together. 
       Adjust the exponent for each component to change its influence on the 
       final priority score. An exponent of 0 removes that component entirely."),
             
             h4("Exposure"),
             sliderInput("exp_exponent", "Exponent", 
                         min = 0, max = 3, value = 3, step = 1),
             
             h4("Sensitivity"),
             sliderInput("sens_exponent", "Exponent", 
                         min = 0, max = 3, value = 2, step = 1),
             
             h4("Threat"),
             sliderInput("threat_exponent", "Exponent", 
                         min = 0, max = 3, value = 1, step = 1)
           ),
    ),
    
    # Right column - tabbed outputs
    column(9,
           tabsetPanel(
             tabPanel("Tables",
                      DT::DTOutput("species_table")
             ),
             tabPanel("Priority Score Distribution",
                      plotOutput("boxplot", height = "850px")
             )
           )
    )
  )
)


server <- function(input, output, session) {
  
  insufficient_species <- app_data %>%
    filter(is.na(region)) %>%
    distinct(alpha_code, common_name, rl_category, raw_CV, raw_DV)
  
  # Mapping from selectInput values to region names in app_data
  region_map <- tibble(
    input_value = c("prop0561", "prop0562", "prop0563", "prop0564", "prop0565", 
                    "prop0566", "prop0567", "propCA", "propOR", "propALL"),
    region_name = c("OCS-P 0561", "OCS-P 0562", "OCS-P 0563", "OCS-P 0564", "OCS-P 0565",
                    "Oregon PSN - OCS-P 0566", "Oregon PSN - OCS-P 0567", "CA", "OR", "all")
  )
  
  analysis_data <- reactive({
    selected_region <- region_map$region_name[region_map$input_value == input$exposure_column]
    
    # Step 1: compute sensitivity on ALL species first (so rescaling is correct)
    all_data <- app_data %>%
      filter(!is.na(raw_CV), !is.na(raw_DV)) %>%
      mutate(
        sensitivity = switch(input$sens_column,
                             summed_sens  = log_rescale(rescale_01(raw_CV) + rescale_01(raw_DV)),
                             highest_sens = log_rescale(pmax(rescale_01(raw_CV), rescale_01(raw_DV))),
                             rescaled_CV  = log_rescale(raw_CV),
                             rescaled_DV  = log_rescale(raw_DV)
        ),
        scaled_overlap = rescale_overlap(outliers_rm),
        status = case_when(
          between(status, 0.4, 0.6) ~ 0.01,
          between(status, 0.7, 0.8) ~ 0.1,
          between(status, 0.9, 1.1) ~ 1,
          between(status, 1.4, 1.5) ~ 10,
          between(status, 1.9, 2.1) ~ 100,
        )
      )
    
    # Step 2: then filter to selected region and sufficient data
    all_data %>%
      filter(region == selected_region) %>%
      filter(!is.na(status)) %>%
      filter(!map_lgl(scaled_overlap, is.null))
  })
  

  # Reactive: run priority_mc_200
  rank_distribution <- reactive({
    data <- analysis_data()
    
    e <- data %>% select(alpha_code, region, scaled_overlap)
    se <- data %>% select(alpha_code, common_name, sensitivity)    
    st <- data %>% select(alpha_code, status)
    
    w <- c(input$exp_exponent, input$sens_exponent, input$threat_exponent)
    
    priority_mc_200(e, se, st, w = w)
  })
  
  # Reactive: summarize ranks
  rank_summary <- reactive({
    rank_distribution() %>%
      group_by(alpha_code) %>%
      summarize(
        rank_min = min(pri_rank),
        rank_max = max(pri_rank),
        .groups = "drop"
      )
  })
  
  # Reactive: compute priority scores
  priority_scores <- reactive({
    data <- analysis_data()
    
    e <- data %>% select(alpha_code, region, scaled_overlap)
    se <- data %>% select(alpha_code, common_name, sensitivity)    
    st <- data %>% select(alpha_code, status)
    w <- c(input$exp_exponent, input$sens_exponent, input$threat_exponent)
    calc_priority(e, se, st, w = w)
  })
  
  table_data <- reactive({
    scores <- priority_scores()
    ranks <- rank_summary()
    data <- analysis_data()
    
    # Outliers summary
    outliers_summary <- data %>%
      mutate(
        outliers_mean = map_dbl(outliers_rm, mean) * 100,
        outliers_min = map_dbl(outliers_rm, min) * 100,
        outliers_max = map_dbl(outliers_rm, max) * 100
      ) %>%
      select(alpha_code, outliers_mean, outliers_min, outliers_max)
    
    # Raw sensitivities
    raw_sensitivities <- data %>%
      select(alpha_code, raw_CV, raw_DV)
    
    # Join everything
    full_table <- scores %>%
      left_join(ranks, by = "alpha_code") %>%
      left_join(outliers_summary, by = "alpha_code") %>%
      left_join(raw_sensitivities, by = "alpha_code") %>%
      left_join(
        data %>% select(alpha_code, common_name, rl_category),
        by = "alpha_code"
      ) %>%
      select(common_name, outliers_mean, outliers_min, outliers_max,
             raw_CV, raw_DV, rl_category,
             ess, ess_lwr, ess_upr, rank_min, rank_max) %>%
      arrange(desc(ess)) %>%
      mutate(rank = row_number()) %>%
      select(common_name, outliers_mean, outliers_min, outliers_max,
             raw_CV, raw_DV, rl_category,
             ess, ess_lwr, ess_upr, rank, rank_min, rank_max)
    
    # Add insufficient data species
    insufficient <- insufficient_species %>%
      select(common_name, rl_category, raw_CV, raw_DV) %>%
      mutate(
        outliers_mean = NA_real_,
        outliers_min = NA_real_,
        outliers_max = NA_real_,
        ess = NA_real_,
        ess_lwr = NA_real_,
        ess_upr = NA_real_,
        rank = NA_integer_,
        rank_min = NA_integer_,
        rank_max = NA_integer_
      ) %>%
      select(common_name, outliers_mean, outliers_min, outliers_max,
             raw_CV, raw_DV, rl_category,
             ess, ess_lwr, ess_upr, rank, rank_min, rank_max)
    
    bind_rows(full_table, insufficient)
  })
  output$boxplot <- renderPlot({
    make_boxplot_app(priority_scores(), app_data)
  })
  
  output$species_table <- DT::renderDT({
    df <- table_data()
    
    DT::datatable(
      setNames(
        df,
        c("Species", "% Overlap (mean)", "% Overlap (lower)", "% Overlap (upper)",
          "Collision Sensitivity", "Displacement Sensitivity", "IUCN Status",
          "Priority Score", "Priority Score (lower)", "Priority Score (upper)",
          "Rank", "Rank Lower", "Rank Upper")
      ),
      options = list(searching = TRUE, paging = TRUE, pageLength = 25),
      rownames = FALSE
    ) %>%
      DT::formatRound(
        columns = c("% Overlap (mean)", "% Overlap (lower)", "% Overlap (upper)",
                    "Collision Sensitivity", "Displacement Sensitivity",
                    "Priority Score", "Priority Score (lower)", "Priority Score (upper)"),
        digits = 3
      )
  })
}


shinyApp(ui, server)



