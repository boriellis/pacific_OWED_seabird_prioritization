##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ##################################
##########################################################################
# Exposure Functions Definitions #####################################
#-------------------------------------------------------------------------
#
# Functions called by scripts/01_exposure.R to estimate seabird exposure to 
# offshore wind energy development (OWED) in the California Current. 

# Workflow:
#   clean_exweights()   - clean raw Qualtrics expert similarity weights
#   combine_seasons()   - sum seasonal bootstraps into annual distributions
#   combine_models()    - weight & combine models for elicited species
#   clean_weas()        - subset and aggregate WEA polygons
#   calculate_exposure()- proportional overlap of distributions with WEAs
#   clean_exposure()    - winsorize and rescale exposure values
#
#-------------------------------------------------------------------------



# CLEAN RAW EXPERT QUALTRICS WEIGHTS --------------------------------------

#' Clean raw Qualtrics expert similarity weights
#'
#' Reads the raw Qualtrics survey export and reshapes it into a long-format
#' data frame of expert-provided similarity weights. Each row is one
#' expert × elicited species × surrogate model combination, giving the
#' weight that expert assigned to that surrogate model when estimating the
#' distribution of the elicited species. The output feeds combine_models().
#'
#' @param csv_file_path Path to the raw Qualtrics CSV export. The file is
#'   expected to have Qualtrics' standard three-row header (question text on
#'   row 2, import IDs on row 3), with survey responses beginning on row 4.
#'
#' @returns A long-format tibble with columns: expert (integer ID),
#'   species (elicited species common name), model (survey column label),
#'   weight (proportion in [0, 1]), alpha_code (elicited species code),
#'   and model_name (surrogate model alpha code).
#'
clean_exweights <- function(csv_file_path) {
  
  # Lookup: elicited species common names -> alpha codes
  rare_codes <- tibble(
    common_name = c("Short-tailed Albatross", 
                    "Townsend's Storm-Petrel", 
                    "Hawaiian Petrel"), 
    alpha_code  = c("STAL", "TOSP", "HAPE")
  )
  
  # Surrogate model codes, in the column order they appear in the survey.
  model_names <- c(
    "SCOT", "PHAL", "PAJA-LTJA", "POJA", "SPSK", "RHAU", "TUPU", "CAAU", 
    "MAMU", "PIGU", "COMU", "ANMU", "SCMU-GUMU-CRMU", "BLKI", "SAGU", 
    "BOGU", "HEEG", "WEGU-WGWH-GWGU", "CAGU", "HERG-ICGU", "CATE", 
    "COTE-ARTE", "ROYT-ELTE", "WEGR-CLGR", "RTLO", "COLO", "LOON", "LAAL", 
    "BFAL", "FTSP", "LESP", "ASSP", "BLSP", "NOFU", "MUPE", "COPE", "PFSH", 
    "BULS", "STTS-SOSH-FFSH", "BVSH", "BRAC", "PECO", "DCCO", "BRPE"
  )
  
  # Qualtrics exports three header rows; pull column names from row 2 and
  # read the response data from row 4 onward.
  header <- read_csv(csv_file_path, skip = 1, n_max = 1, show_col_types = FALSE)
  raw_dataframe <- read_csv(
    csv_file_path,
    skip = 3,
    col_names = colnames(header),
    show_col_types = FALSE
  )
  
  cleaned_weights <- raw_dataframe %>% 
    mutate(expert = row_number()) %>% 
    slice(-9, -17, -18) %>%  # drop incomplete survey submissions
    select(expert,
           starts_with("Short-tailed Albatross"),
           starts_with("Townsend's Storm-Petrel"),
           starts_with("Hawaiian Petrel")) %>% 
    # Column labels are "<species> - <model>"; split into two columns
    pivot_longer(-expert, 
                 names_to  = c("species", "model"),
                 names_sep = " - ",
                 values_to = "weight") %>% 
    mutate(weight = weight / 100) %>%  # survey values are 0–100; convert to proportion
    left_join(rare_codes, by = c(species = "common_name"))
  
  # Attach surrogate model codes. Recycles model_names across the three
  # elicited species (same model set per species, same order).
  cleaned_weights$model_name <- rep(model_names, nrow(cleaned_weights) / length(model_names))
  
  return(cleaned_weights)
}




# COMBINE SEASONAL BOOTSTRAPS INTO ANNUAL DISTRIBUTIONS -------------------

#' Sum seasonal bootstrap grids into annual distributions
#'
#' Takes all the seasonal bootstrap rasters for one species/species group, 
#' stacked into a single SpatRaster, and sums the seasons together within each 
#' bootstrap iteration to produce annual distributions. This gives one annual 
#' grid per bootstrap iteration (200 total)
#'
#'
#' @param x A SpatRaster of one model's seasonal bootstraps, all seasons
#'   stacked together. Layers are named by iteration ("bootstrap_001", ...),
#'   and layers sharing an iteration number are summed across seasons. Build 
#'   this in the calling script by reading in that model's season
#'   files, e.g. rast(dir(..., pattern = "^PFSH_")).
#' @param model Character string naming the model (e.g. "PFSH"), used only
#'   to label the output layers.
#'
#' @returns A SpatRaster with one layer per bootstrap iteration, named
#'   "{model}_annual_{iteration}".
#'
combine_seasons <- function(x, model) {
  
  # Bootstrap iteration index, carried in the trailing digits of each layer
  # name ("bootstrap_007" -> 7). Identical across seasons, so this groups
  # the same iteration together regardless of how many seasons are stacked.
  iter <- as.integer(str_extract(names(x), "\\d+$"))
  
  # Sum all seasonal layers within each iteration -> one annual grid per
  # iteration. tapp groups by index, so any dropped iterations are handled
  # gracefully (only groups that exist are returned).
  annual <- terra::tapp(x, index = iter, fun = "sum")
  
  names(annual) <- str_glue("{model}_annual_{sort(unique(iter))}")
  return(annual)
}



# COMBINE SURROGATE MODELS FOR ELICITED SPECIES --------------------------

#' Blend surrogate model distributions for an elicited species
#'
#' For a species that lacks its own SDM (Short-tailed Albatross, Hawaiian
#' Petrel, Townsend's Storm-Petrel), builds an estimated distribution from a
#' single expert's judgement: a weighted combination of the annual bootstrap
#' distributions of surrogate models the expert deemed similar. Each surrogate
#' is normalized to a common scale before weighting, so the expert's weights
#' govern the relative contribution of each surrogate's spatial pattern rather
#' than being swamped by differences in absolute density between models.
#'
#' Weighting is done per bootstrap iteration (iteration i of surrogate A +
#' iteration i of surrogate B, ...), so the 200 bootstrap distributions
#' propagate through the elicited species the same way they do for modeled
#' species. 
#'
#' @param species Alpha code of the elicited species ("STAL", "HAPE", "TOSP").
#' @param expert Integer ID of the expert whose weights are being used.
#' @param dist_path Folder holding the annual bootstrap rasters produced by
#'   combine_seasons (one "{model}_annual_boot.tif" per surrogate model).
#' @param exweights Cleaned expert weights from clean_exweights().
#'
#' @returns A SpatRaster with one layer per bootstrap iteration, named
#'   "{species}_expert{expert}_{iteration}".
#'
combine_models <- function(species, expert, dist_path, exweights) {
  
  # Surrogate models this expert weighted for this species (weight > 0)
  ex <- filter(exweights, 
               expert     == !!expert, 
               alpha_code == !!species, 
               weight > 0)
  
  # Load each surrogate's annual bootstrap raster and normalize each layer
  # to its own max, so surrogates contribute on a common [0, 1] scale.
  # Pattern is anchored so e.g. "COLO" can't match a longer model name.
  surrogate_rasters <- map(ex$model_name, \(m) {
    r <- rast(dir(dist_path, pattern = str_glue("^{m}_annual"), full.names = TRUE))
    r / global(r, "max", na.rm = TRUE)[, 1]
  })
  
  # Weighted sum across surrogates, matched by bootstrap iteration
  result <- Reduce(`+`, Map(`*`, surrogate_rasters, ex$weight))
  
  names(result) <- str_glue("{species}_expert{expert}_{1:nlyr(result)}")
  return(result)
}


# CLEAN WIND ENERGY AREA POLYGONS ----------------------------------------

#' Assemble and aggregate wind energy area (WEA) polygons
#'
#' Pulls the California leases and Oregon planning areas of interest from the
#' two BOEM source files, projects them to the density models' coordinate
#' system, and stacks them into a single object at three spatial scales:
#' individual leases, state-level (all CA / all OR polygons dissolved), and
#' region-level (all polygons dissolved). The state and regional polygons let
#' exposure be summed across WEAs at those scales downstream.
#'
#' @param l Lease polygons (BOEM Wind Lease Outlines); the five CA leases are
#'   identified by an "OCS-P" prefix in LEASE_NUMB.
#' @param c Planning-area polygons (BOEM Wind Planning Area Outlines); the two
#'   OR areas are identified by an "OCS-P" prefix in ADDITIONAL.
#'
#' @returns A SpatVector of WEA polygons with columns name, state, and
#'   spatial_scale ("lease", "state", or "all"), containing the individual
#'   leases plus their state-level and region-level dissolved aggregates.
#'
clean_weas <- function(l, c) {
  
  # Coordinate system of the Leirness density rasters; WEAs must match it
  # so overlap extraction lines up spatially.
  crs <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +gamma=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  c <- project(c, crs)
  l <- project(l, crs)
  
  # Individual WEAs: CA leases from the lease file, OR areas from the
  # planning-area file, both filtered to the "OCS-P" polygons of interest.
  local_weas <- rbind(
    l %>% 
      filter(str_detect(LEASE_NUMB, "OCS-P")) %>% 
      select(name = LEASE_NUMB) %>% 
      mutate(state = "CA", spatial_scale = "lease"),
    c %>% 
      filter(str_detect(ADDITIONAL, "OCS-P")) %>% 
      select(name = ADDITIONAL) %>% 
      mutate(state = "OR", spatial_scale = "lease")
  )
  
  # State-level: dissolve individual WEAs within each state
  state_weas <- local_weas %>% 
    group_by(state) %>% 
    summarize() %>% 
    mutate(name = state, spatial_scale = "state")
  
  # Region-level: dissolve all WEAs into a single polygon
  all_weas <- aggregate(local_weas)
  all_weas$name <- "all"
  all_weas$state <- NA
  all_weas$spatial_scale <- "all"
  
  # Stack all three scales into one object
  rbind(local_weas, state_weas, all_weas)
}


# CALCULATE RAW EXPOSURE -------------------------------------------------

#' Calculate raw exposure (proportional WEA overlap) per species and scale
#'
#' For each species, computes the proportion of its predicted annual
#' distribution that falls within the wind energy areas, at each spatial
#' scale in `v` (lease, state, region). This is done separately for every
#' bootstrap iteration, yielding a distribution of proportional-overlap
#' values per species x scale that carries the SDMs' predictive uncertainty
#' through to exposure. Modeled species use their annual bootstrap rasters;
#' elicited species (STAL, HAPE, TOSP) use their per-expert bootstrap rasters,
#' pooled across experts.
#'
#' @param modeled_path Folder of annual bootstrap rasters for modeled species,
#'   one file per model (from combine_seasons).
#' @param elicited_path Folder of annual bootstrap rasters for elicited
#'   species, one file per species x expert (from combine_models).
#' @param v Cleaned WEA polygons (SpatVector) from clean_weas().
#' @param sp Species information table; must contain alpha_code, the
#'   exposure_model to use for each species, and a regional inclusion flag.
#'
#' @returns A tibble with one row per region x species, each holding a
#'   list-column (`raw_overlap`) of that combination's proportional-overlap
#'   values across all bootstrap iterations (and, for elicited species, all
#'   experts). These are raw, un-rescaled values; rescaling happens in
#'   clean_exposure().
#'
calculate_exposure <- function(modeled_path, elicited_path, v, sp) {
  
  elicited <- c("HAPE", "TOSP", "STAL")
  
  # Species to process: regionally-included modeled species, plus the three
  # elicited species (which map to themselves rather than a Leirness model).
  exposure_sp <- sp %>% 
    filter(!is.na(exposure_model), regional == "Y") %>% 
    select(alpha_code, exposure_model) %>% 
    rbind(tibble(alpha_code = elicited, exposure_model = elicited))
  
  map(exposure_sp$alpha_code, \(s) {
    message("Processing species: ", s)
    
    # Load this species' annual bootstrap raster. Elicited species match all
    # their per-expert files (pooled downstream); modeled species load the
    # single file for their assigned model. Patterns are anchored so a short
    # code can't match a longer name as a substring.
    d <- if (s %in% elicited) {
      rast(dir(elicited_path, pattern = str_glue("^{s}_"), full.names = TRUE))
    } else {
      model <- exposure_sp$exposure_model[exposure_sp$alpha_code == s]
      rast(dir(modeled_path, pattern = str_glue("^{model}_annual"), full.names = TRUE))
    }
    
    # Area-weighted extraction: each cell's density scaled by the fraction of
    # that cell inside the polygon, then summed per WEA.
    extracted_density <- terra::extract(d, v, exact = TRUE, touches = TRUE)
    in_wea_density <- as_tibble(extracted_density) %>%
      mutate(across(-c(ID, fraction), \(x) x * fraction)) %>% 
      group_by(ID) %>% 
      summarize(across(-fraction, sum)) %>% 
      rename(region = ID) %>% 
      mutate(region = v$name)
    
    # Total predicted density across the whole study area, per iteration,
    # named by layer so the division matches columns by name (not position).
    total_density <- global(d, "sum", na.rm = TRUE)$sum
    names(total_density) <- names(d)
    
    # Proportional overlap = in-WEA density / study-area total, per iteration
    prop_overlap <- in_wea_density %>% 
      mutate(across(-region, \(x) x / total_density[cur_column()]))
    
    # Long format: one row per region x layer (one layer = one bootstrap
    # iteration for modeled species; one expert x iteration for elicited).
    # The layer name itself isn't needed downstream, so it's dropped.
    pivot_longer(prop_overlap, 
                 -region, 
                 names_to  = "layer", 
                 values_to = "prop_overlap") %>% 
      select(-layer) %>% 
      mutate(alpha_code = s)
  }) %>% 
    list_rbind() %>% 
    # Pool each species x region's overlaps into one list-column
    group_by(region, alpha_code) %>% 
    summarize(raw_overlap = list(prop_overlap), .groups = "drop") 
}




#STOPPED HERE - COME BACK TO CONSIDER IF WINSORIZING NEEDS TO STAY AFTER GETTING RESULTS

# REMOVE OUTLIERS & RESCALE-----------------------------------------------------


clean_exposure <- function(x){
  x %>% 
    # winsorize each vector inside the list-column
    mutate(outliers_rm = map(raw_overlap, winsorize)) %>% 
    
    # Rescale overlaps within each region
    group_by(region) %>%
    mutate(scaled_overlap = rescale_overlap(outliers_rm)) %>%
    ungroup()
}

#' Subfunction to address outliers
#'
#' @param x a vector of numbers
#' @param probs the upper and lower quantiles we want to cap the vector to 
#'
#' @returns the vector where all values below the value that = the 2.5% quantile are = that value, and the same for values above tthe 97.5% quantile
#' @export
#'
#' @examples
winsorize <- function(x, probs = c(0.025, 0.975)) {
  q <- quantile(x, probs, na.rm = TRUE)
  x <- pmax(x, q[1])   # raise any values below 2.5th percentile
  x <- pmin(x, q[2])   # cap any values above 97.5th percentile
  return(x)
}



#' Subfunction to rescale exposure from 0.5-2
#'
#' @param overlap_list is, I think, the list of vectors of each species/region overlap values
#'
#' @returns those values rescaled from 0.5 min to 2.0 max 
#' @export
#'
#' @examples
rescale_overlap <- function(overlap_list) {
  all_overlaps <- unlist(overlap_list)
  min_overlap <- min(all_overlaps)
  max_overlap <- max(all_overlaps)
  log_rescale <- function(x) {
    log_y_rng <- log(c(0.5, 2.0))
    log_y <- log_y_rng[1] + 
      (log_y_rng[2] - log_y_rng[1]) * 
      (x - min_overlap) / (max_overlap - min_overlap)
    y <- exp(log_y)
    return(y)
  }
  map(overlap_list, log_rescale)
}


#plots to look at distributions of exposure values

# foo <- cleaned_exposure_1000sims %>%
#   unnest(scaled_overlap) %>%
#   filter(region == "CA")
# bar <- filter(foo, alpha_code %in% c("HAPE", "TOSP", "STAL"))
# ggplot(foo, aes(scaled_overlap, color = alpha_code)) +
#   geom_density() +
#   geom_density(aes(fill = alpha_code), bar, alpha = 0.5) +
#   scale_y_continuous(transform = "log1p") +
#   theme(legend.position = "none")
# ggplot(bar, aes(scaled_overlap, fill = alpha_code)) +
#   geom_density(alpha = 0.5) +
#   xlim(0, 1)
