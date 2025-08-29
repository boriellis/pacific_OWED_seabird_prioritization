#these are the functions in the status branch of the work flow



# CLEAN STATUS SCORES -----------------------------------------------------

#' Clean Status Scores
#'
#' @param sp loaded in total_sp_list.csv df
#' @param iucn loaded in raw_iucn_list.csv df (pulled from birdlife datazone, seabird/waterbird filter)
#'
#' @returns a df of common name, species alpha code, the local scientific name we use, the corresponding birdlife scientific name (sometimes different from taxonomic changes), and IUCN redlist status code for each of the 91 species 
#'
join_statuses <- function(sp, iucn){
  namematches <- tibble(localname = c("Phalaropus tricolor", "Chroicocephalus philadelphia", "Stercorarius maccormicki", "Larus brachyrhynchus", "Sula brewsteri"), 
                                      nameref = c("Steganopus tricolor", "Larus philadelphia", "Catharacta maccormicki", "Larus delawarensis", "Sula leucogaster"))
  clean_iucn <- sp %>% 
    select(alpha_code,
           common_name,
           scientific_name) %>%
    left_join(namematches, by = c("scientific_name" = "localname")) %>%
    mutate(lookup_name = ifelse(is.na(nameref), scientific_name, nameref)) %>%
    select(-nameref) %>% 
    left_join(iucn, by = c("lookup_name" = "scientific_name")) %>%
    select(alpha_code, common_name = common_name.x, scientific_name, lookup_name, rl_category) %>% 
    filter(!is.na(alpha_code))
  return(clean_iucn)
} 




# RESCALE STATUS ----------------------------------------------------------


