##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ##################################
##########################################################################
# Status Functions Definitions #####################################
#-------------------------------------------------------------------------
#
# Functions called by scripts/03_sensitivity.R to assign IUCN categorites to 
# each species and assign a value to each such that the range matches the 
# framework's standard 0.5-2 scale
#-------------------------------------------------------------------------


# CLEAN STATUS SCORES ----------------------------------------------------

#' Clean IUCN Red List status scores
#'
#' Assigns each species a status weight derived from its IUCN Red List
#' category. Species scientific names are first reconciled to BirdLife
#' taxonomy (which differs from the project's local taxonomy for a handful
#' of species), then joined to the BirdLife/IUCN Red List export to obtain
#' each species' category. Categories are mapped to weights on the
#' framework's [0.5, 2.0] scale (see below).
#'
#' @param sp Master species list (total_sp_list.csv). Retains some rows for
#'   grouped model names, which are dropped here via the alpha_code filter.
#' @param iucn Raw IUCN Red List export (raw_iucn_list.csv; pulled from the
#'   BirdLife DataZone with the seabird/waterbird filter).
#'
#' @returns A tibble of alpha_code, rl_category (IUCN Red List code), and
#'   status (the corresponding [0.5, 2.0] weight) for each species.
#'
clean_statuses <- function(sp, iucn){
  
  # Crosswalk: local scientific names -> BirdLife scientific names, for the
  # species where the two taxonomies disagree. Used to make the IUCN join work.
  sp_iucn_sciname <- tibble(
    sp_sciname = c("Phalaropus tricolor", "Chroicocephalus philadelphia", 
                   "Stercorarius maccormicki", "Larus brachyrhynchus", 
                   "Sula brewsteri"), 
    iucn_sciname = c("Steganopus tricolor", "Larus philadelphia", 
                     "Catharacta maccormicki", "Larus delawarensis", 
                     "Sula leucogaster")
  )
  
  # Drop grouped-model rows (no alpha_code), then attach the BirdLife name,
  # falling back to the local name where no crosswalk entry exists.
  sp_clean <- drop_na(sp, alpha_code) %>% 
    left_join(sp_iucn_sciname, by = c(scientific_name = "sp_sciname")) %>% 
    mutate(iucn_sciname = coalesce(iucn_sciname, scientific_name))
  
  iucn_clean <- select(iucn, 
                       rl_category  = `RL Category`,
                       iucn_sciname = `Scientific name`)
  
  # IUCN category weights on the [0.5, 2.0] scale, with a constant ratio of
  # 4^(1/4) ~ 1.414 between successive categories:
  #   LC = 0.5, NT ~ 0.71, VU = 1.0, EN ~ 1.41, CR = 2.0
  rl_status <- 0.5 * (4^0.25)^(0:4)
  names(rl_status) <- c("LC", "NT", "VU", "EN", "CR")
  
  # Join category to each species and map it to its weight
  result <- sp_clean %>% 
    left_join(iucn_clean, by = "iucn_sciname") %>% 
    mutate(status = rl_status[rl_category]) %>% 
    select(alpha_code, rl_category, status)
  
  return(result)
}




