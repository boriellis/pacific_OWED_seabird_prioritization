##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ##################################
##########################################################################
# Exposure Function Definitions #########################################
#-------------------------------------------------------------------------
#
# Functions called by scripts/01_exposure.R to estimate seabird exposure to
# offshore wind energy development (OWED) in the California Current.
#
# Workflow:
#   clean_exweights()    - clean raw Qualtrics expert similarity weights
#   build_keep_index()   - flag/drop outlier bootstrap iterations (median*k rule)
#   combine_seasons()    - sum retained seasonal bootstraps into annual distributions
#   combine_models()     - weight & combine surrogate models for elicited species
#   clean_weas()         - subset and aggregate WEA polygons
#   calculate_exposure() - proportional overlap of distributions with WEAs
#   clean_exposure()     - rescale exposure values to the [0.5, 2.0] scale
#
# Two processing parameters are exposed as arguments (with defaults set to the
# values used in the main analysis) so the sensitivity analysis can vary them:
#   - k       (build_keep_index): outlier cutoff multiplier; default 1000
#   - anchor  (rescale_overlap):  central quantile span for rescaling; default 0.99
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






# IDENTIFY BOOTSTRAP ITERATIONS TO KEEP -----------------------

#' Build a per-model keep-list of bootstrap iterations under a median*k rule
#'
#' For each model, flags bootstrap iterations whose peak predicted density
#' (cell max) exceeds k*median(cell max) within a season,an iteration is kept 
#' only if it survived in every season the
#' model was fit in (i.e. the union of flagged iterations across seasons is
#' dropped). The resulting keep-list is consumed by combine_seasons to sum
#' only the retained iterations into annual distributions.
#'
#' @param boot_dir Directory of seasonal bootstrap files, named
#'   "{MODEL}_{season}_...".
#' @param k Multiplier on the median cell-max. Iterations with
#'   max > k * median(max) within a season are flagged. Default 1000
#'   (the value used in the main analysis).
#'
#' @returns A named list, one element per model, each a sorted integer vector
#'   of the bootstrap iteration numbers to keep for that model.
#'
build_keep_index <- function(boot_dir, k = 1000) {
  
  # Season token used only as a landmark to split the model name off the
  # filename. Anchoring on the season (rather than the first "_") keeps
  # hyphenated group-model names intact, e.g. "PAJA-LTJA", "SCMU-GUMU-CRMU".
  season_pattern <- "_(spring|summer|fall|winter)_"
  
  files <- dir(boot_dir, pattern = "\\.tif$", full.names = TRUE)
  
  # per seasonal file: which iteration numbers are flagged as outliers
  flagged <- map(files, \(f) {
    r      <- rast(f)
    maxes  <- global(r, "max", na.rm = TRUE)[, 1]
    cutoff <- k * median(maxes, na.rm = TRUE)
    
    # iteration number parsed from the layer name (matches combine_seasons)
    iter   <- as.integer(str_extract(names(r), "\\d+$"))
    
    model  <- str_extract(basename(f), paste0("^.+(?=", season_pattern, ")"))
    
    list(
      model    = model,
      all_iter = iter,
      bad_iter = iter[maxes > cutoff]
    )
  })
  
  models <- unique(map_chr(flagged, "model"))
  
  # per model: keep = (all iterations present) minus (union of bad across seasons)
  keep <- map(models, \(m) {
    entries  <- flagged[map_chr(flagged, "model") == m]
    all_iter <- sort(unique(unlist(map(entries, "all_iter"))))
    bad_iter <- sort(unique(unlist(map(entries, "bad_iter"))))
    setdiff(all_iter, bad_iter)
  })
  names(keep) <- models
  
  keep
}





# COMBINE SEASONAL BOOTSTRAPS INTO ANNUAL DISTRIBUTIONS ------------------

#' Sum seasonal bootstrap grids into annual distributions
#'
#' Takes all the seasonal bootstrap rasters for one model, stacked into a
#' single SpatRaster, and sums the seasons together within each bootstrap
#' iteration to produce annual distributions (Leirness et al. 2021: build the
#' annual bootstrap sample first, summarize later). Seasons a species wasn't
#' modeled in are simply absent from the stack, correctly treated as zero.
#'
#' If a `keep` vector is supplied only those
#' bootstrap iterations are retained before summing — iterations flagged as
#' outliers in any season are dropped, so all seasons contribute the same
#' surviving set of iterations. Retained annuals keep their original iteration
#' numbers for traceability.
#'
#' @param x A SpatRaster of one model's seasonal bootstraps, all seasons
#'   stacked together. Layers are named by iteration ("bootstrap_001", ...),
#'   and layers sharing an iteration number are summed across seasons.
#' @param model Character string naming the model (e.g. "PFSH"), used to label
#'   the output layers.
#' @param keep Optional integer vector of bootstrap iteration numbers to retain
#'   (e.g. build_keep_index(...)[[model]]). If NULL (default), all iterations
#'   are used.
#'
#' @returns A SpatRaster of annual distributions, one layer per retained
#'   bootstrap iteration, named "{model}_annual_{iteration}".
#'
combine_seasons <- function(x, model, keep = NULL) {
  
  # Bootstrap iteration number carried in each layer's trailing digits
  # ("bootstrap_007" -> 7); identical across seasons, so this groups the
  # same iteration together regardless of how many seasons are stacked.
  iter <- as.integer(str_extract(names(x), "\\d+$"))
  
  if (!is.null(keep)) {
    in_keep <- iter %in% keep
    x    <- x[[in_keep]]
    iter <- iter[in_keep]
  }
  
  # Sum all seasonal layers within each surviving iteration -> one annual grid
  # per iteration. tapp groups by index, so seasons collapse together and any
  # already-absent iterations are simply skipped.
  annual <- terra::tapp(x, index = iter, fun = "sum")
  
  names(annual) <- str_glue("{model}_annual_{sort(unique(iter))}")
  return(annual)
}




# COMBINE SURROGATE MODELS FOR ELICITED SPECIES --------------------------

#' Blend surrogate model distributions for an elicited species
#'
#' For a species lacking its own SDM (STAL, HAPE, TOSP), builds an estimated
#' distribution from one expert's judgement: a weighted combination of the
#' annual bootstrap distributions of surrogate models the expert deemed
#' similar. Each surrogate is normalized to a common scale before weighting.
#'
#' The surrogate annual stacks are read from the outlier-cleaned modeled
#' directory, so outlier iterations have already been dropped upstream — and
#' different surrogates may therefore retain different iteration sets. Surrogates are 
#' reconciled by intersection: only bootstrap iterations surviving in every 
#' surrogate this expert used are combined,
#' matched by iteration number.
#'
#' @param species Alpha code of the elicited species ("STAL", "HAPE", "TOSP").
#' @param expert Integer ID of the expert whose weights are used.
#' @param dist_path Folder of outlier-cleaned annual bootstrap rasters
#'   (one "{model}_annual_boot.tif" per surrogate, from combine_seasons).
#' @param exweights Cleaned expert weights from clean_exweights().
#'
#' @returns A SpatRaster with one layer per retained (shared) bootstrap
#'   iteration, named "{species}_expert{expert}_{iteration}".
#'
combine_models <- function(species, expert, dist_path, exweights) {
  
  # Surrogate models this expert weighted for this species (weight > 0)
  ex <- filter(exweights,
               expert     == !!expert,
               alpha_code == !!species,
               weight > 0)
  
  # Load each surrogate's (already outlier-cleaned) annual stack, and record
  # the bootstrap iteration numbers each one retained.
  surrogates <- map(ex$model_name, \(m) {
    r <- rast(dir(dist_path, pattern = str_glue("^{m}_annual"), full.names = TRUE))
    list(r = r, iter = as.integer(str_extract(names(r), "\\d+$")))
  })
  
  common <- sort(Reduce(intersect, map(surrogates, "iter")))
  stopifnot("no shared bootstrap iterations across this expert's surrogates" =
              length(common) > 0)
  
  # Subset + reorder each surrogate to the shared iterations (same order for
  # all), then normalize each layer to its own max for a common [0,1] scale.
  aligned <- map(surrogates, \(s) {
    r_sub <- s$r[[ match(common, s$iter) ]]        # shared iterations, sorted
    r_sub / global(r_sub, "max", na.rm = TRUE)[, 1]
  })
  
  # Weighted sum across surrogates, now all matched by iteration
  result <- Reduce(`+`, Map(`*`, aligned, ex$weight))
  names(result) <- str_glue("{species}_expert{expert}_{common}")
  
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
#'   `raw_overlap` list-column of proportional-overlap values across all
#'   bootstrap iterations (and, for elicited species, all experts), plus
#'   `n_boot` (the number of draws pooled into each estimate). These are raw,
#'   un-rescaled values; rescaling happens in clean_exposure().
#'
calculate_exposure <- function(modeled_path, elicited_path, v, sp) {
  
  elicited <- c("HAPE", "TOSP", "STAL")
  
  exposure_sp <- sp %>%
    filter(!is.na(exposure_model), regional == "Y") %>%
    select(alpha_code, exposure_model) %>%
    rbind(tibble(alpha_code = elicited, exposure_model = elicited))
  
  map(exposure_sp$alpha_code, \(s) {
    message("Processing species: ", s)
    
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
    
    # Total predicted density across the study area, per iteration, named by
    # layer so the division matches columns by name (not position).
    total_density <- global(d, "sum", na.rm = TRUE)$sum
    names(total_density) <- names(d)
    
    # Proportional overlap = in-WEA density / study-area total, per iteration
    prop_overlap <- in_wea_density %>%
      mutate(across(-region, \(x) x / total_density[cur_column()]))
    
    pivot_longer(prop_overlap,
                 -region,
                 names_to  = "layer",
                 values_to = "prop_overlap") %>%
      select(-layer) %>%
      mutate(alpha_code = s)
  }) %>%
    list_rbind() %>%
    group_by(region, alpha_code) %>%
    summarize(raw_overlap = list(prop_overlap),
              n_boot = length(prop_overlap),   # draws pooled into this estimate
              .groups = "drop")
}



# RESCALE EXPOSURE -------------------------------------------------------

#' Rescale raw exposure to the [0.5, 2.0] range
#'
#' Rescales the raw proportional-overlap distributions onto the framework's
#' common [0.5, 2.0] multiplicative range, so exposure combines on equal
#' footing with sensitivity and status. Rescaling is done per region across
#' the pooled values of all species (see rescale_overlap), preserving the
#' relative differences among species within a region.
#'
#' @param x Raw exposure from calculate_exposure(): one row per species x
#'   region with a `raw_overlap` list-column of proportional-overlap values.
#' @param anchor Central quantile span used to set the rescaling endpoints
#'   (passed to rescale_overlap). Default 0.99 (the value used in the main
#'   analysis); varied in the sensitivity analysis.
#'
#' @returns `x` with an added `scaled_overlap` list-column holding the rescaled
#'   [0.5, 2.0] values.
#'
clean_exposure <- function(x, anchor = 0.99){
  x %>%
    # Rescale overlaps within each region, pooled across all species
    group_by(region) %>%
    mutate(scaled_overlap = rescale_overlap(raw_overlap, anchor = anchor)) %>%
    ungroup()
}


#' Rescale pooled exposure distributions to [0.5, 2.0]
#'
#' Rescales a list of per-species overlap vectors onto the [0.5, 2.0]
#' multiplicative range, pooled across all species so every species is placed
#' on a common regional scale. The endpoints are anchored on a central
#' quantile span rather than the raw min/max, so a few extreme draws cannot
#' set the scale; values beyond the anchored range are capped to [0.5, 2.0].
#' The shape of the underlying data is preserved: the anchored range maps
#' linearly (in log space) onto [0.5, 2.0], so where a species falls reflects
#' its overlap relative to the pooled distribution.
#'
#' @param overlap_list A list of numeric vectors, one per species, of overlap
#'   values for a single region.
#' @param anchor Central quantile span the rescaling endpoints are anchored on.
#'   e.g. 0.99 anchors on the 0.5th and 99.5th percentiles (0.5% clipped per
#'   side); 0.95 anchors on the 2.5th/97.5th, etc. Default 0.99.
#'
#' @returns A list of the same vectors rescaled onto [0.5, 2.0], with values
#'   beyond the anchored range capped to the bounds.
#'
rescale_overlap <- function(overlap_list, anchor = 0.99) {
  
  # per-side tail fraction clipped/anchored (e.g. anchor 0.99 -> 0.005 each side)
  tail <- (1 - anchor) / 2
  
  all_overlaps <- unlist(overlap_list)
  lwr_overlap <- unname(quantile(all_overlaps, tail,       na.rm = TRUE))
  upr_overlap <- unname(quantile(all_overlaps, 1 - tail,   na.rm = TRUE))
  
  # Output range, inset by the same tail fraction so the anchor quantiles land
  # just inside [0.5, 2.0] (in log space).
  log_bounds <- log(c(0.5, 2.0))
  span       <- log_bounds[2] - log_bounds[1]
  log_lwr    <- log_bounds[1] + tail * span
  log_upr    <- log_bounds[2] - tail * span
  
  log_rescale <- function(x) {
    log_y <- log_lwr + (log_upr - log_lwr) *
      (x - lwr_overlap) / (upr_overlap - lwr_overlap)
    exp(log_y)
  }
  
  # rescale, then cap anything beyond the anchored range to [0.5, 2.0]
  map(overlap_list, log_rescale) |>
    map(\(x) pmax(pmin(x, 2), 0.5))
}
