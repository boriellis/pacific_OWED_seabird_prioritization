#these are the functions in the status branch of the work flow




# CLEAN AND COMBINE CV & DV VALUES ----------------------------------------



#' Clean and combine collision and displacement sensitivity values 
#'
#' @param sp loaded in total_sp_list.csv df
#' @param cv loaded in POCS_VulnIndex_CV.csv df
#' @param dv loaded in POCS_VulnIndex_DV.csv df
#'
#' @returns a cleaned df of species alpha code, common name, scientific name, following our local taxonomy, and the up to date CV and DV values from the Kelsey et al 2025 report 
#' @export
#'
#' @examples
clean_sens <- function(sp, cv, dv) {
  #fix typos in dv 
  dv <- dv %>% 
    mutate(AlphaCode = case_when(
      AlphaCode == "ROTE" ~ "ROYT",
      AlphaCode == "WIPE" ~ "AWPE",
      TRUE ~ AlphaCode
    ))
  #use the main list as the taxonomy
  clean_sens <- sp %>% 
    drop_na(alpha_code) %>% 
    select(alpha_code,
           common_name,
           scientific_name) %>%
    left_join(select(dv, AlphaCode, DV = DV_new), 
              by = c("alpha_code" = "AlphaCode")) %>% 
    left_join(select(cv, AlphaCode, CV = CV_new), 
              by = c("alpha_code" = "AlphaCode"))
  return(clean_sens)
}



# RESCALE SENSITIVITY VALUES ----------------------------------------------


#' Rescale sensitivity values 
#'
#' @param sp is the loaded in total_sp_list.csv df (used to select only regional species)
#' @param sens cleaned sensitivity value df (output from clean_sens)
#' @param sel user selected choice of CV, DV, the two summed, or whichever is highest
#'
#' @returns df of alpha codes, species names and rescaled selected sensitivity values (0.5-2)
rescale_sens <- function(sp, 
                         sens,
                         sel = c("CV", "DV", "sum", "highest")) {
  sel <- match.arg(sel)
  
  # Filter for regional species only
  regional_species <- sp %>% 
    drop_na(alpha_code) %>% 
    filter(regional == "Y") %>% 
    pull(alpha_code)  
  
  sens <- sens %>% 
    filter(alpha_code %in% regional_species) 
  
  sens$sensitivity <- switch(
    sel, 
    CV = log_rescale(sens$CV),
    DV = log_rescale(sens$DV),
    sum = log_rescale(
      rescale_01(sens$CV) +
        rescale_01(sens$DV)
    ),
    highest = {
      CV_01 <- rescale_01(sens$CV) 
      DV_01 <- rescale_01(sens$DV)
      CV_DV <- pmax(CV_01, DV_01)
      log_rescale(CV_DV)
    }
  )
  return(sens)
}


#rescale_sens(sp, sens, sel = "sum")

#subfunctions that get used in rescale_sens

#' Rescale to 0.5-2.0
#'
#' @param x is the vector of sensitivity numbers (either CV, DV, sum, or highest)
#'
#' @returns a rescaled vector where the lowest val in the range is 0.5 and the highest is 2.0

log_rescale <- function(x){
  log_y_rng <- log(c(0.5, 2.0))
  log_y <- log_y_rng[1] + (log_y_rng[2] - log_y_rng[1]) * (x - min(x)) / (max(x) - min(x))
  y <- exp(log_y)
  return(y)
}

#rescale a vector from 0-1
rescale_01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}






