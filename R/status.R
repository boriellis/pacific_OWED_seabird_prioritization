#these are the functions in the status branch of the work flow



# CLEAN STATUS SCORES -----------------------------------------------------

#' Clean Status Scores
#'
#' @param sp loaded in total_sp_list.csv df
#' @param iucn loaded in raw_iucn_list.csv df (pulled from birdlife datazone, seabird/waterbird filter)
#'
#' @returns a df of common name, species alpha code, the local scientific name we use, the corresponding birdlife scientific name (sometimes different from taxonomic changes), and IUCN redlist status code for each of the 91 species 
#'
clean_statuses <- function(sp, iucn){
  sp_iucn_sciname <- tibble(
    sp_sciname = c("Phalaropus tricolor", "Chroicocephalus philadelphia", "Stercorarius maccormicki", "Larus brachyrhynchus", "Sula brewsteri"), 
    iucn_sciname = c("Steganopus tricolor", "Larus philadelphia", "Catharacta maccormicki", "Larus delawarensis", "Sula leucogaster")
  )
  #sp retains some rows for unused group model names
  sp_clean <- drop_na(sp, alpha_code) %>% 
    left_join(sp_iucn_sciname, by = c(scientific_name = "sp_sciname")) %>% 
    mutate(iucn_sciname = coalesce(iucn_sciname, scientific_name))
  iucn_clean <- select(iucn, 
                       rl_category = `RL Category`,
                       iucn_sciname = `Scientific name`)
  #set status values to RL categories from 0.5-2 with a fixed multiplier of 4^(1/4) between each 
  rl_status <- 0.5 * (4^0.25)^(0:4)
  names(rl_status) <- c("LC", "NT", "VU", "EN", "CR")
  result <- sp_clean %>% 
    left_join(iucn_clean, by = "iucn_sciname") %>% 
    mutate(status = rl_status[rl_category]) %>% 
    select(alpha_code, rl_category, status)
  return(result)
} 




# RESCALE STATUS ----------------------------------------------------------


