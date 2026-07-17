
#check out the exposure distributions all together:


# Plots to inspect distributions of the rescaled exposure values

# All species in the CA region, with the three elicited species highlighted
foo <- cleaned_exposure %>%
  unnest(scaled_overlap) %>%
  filter(region == "CA")

bar <- filter(foo, alpha_code %in% c("HAPE", "TOSP", "STAL"))

# Density of all species (lines), elicited species overlaid as filled
ggplot(foo, aes(scaled_overlap, color = alpha_code)) +
  geom_density() +
  geom_density(aes(fill = alpha_code), bar, alpha = 0.5) +
  scale_y_continuous(transform = "log1p") +
  theme(legend.position = "none")

# Just the three elicited species, zoomed in
ggplot(bar, aes(scaled_overlap, fill = alpha_code)) +
  geom_density(alpha = 0.5)




# Which species hold the extreme raw values in each region?
exposure_vals %>%
  mutate(
    p50  = map_dbl(raw_overlap, median),
    p975 = map_dbl(raw_overlap, \(x) quantile(x, 0.975, na.rm = TRUE)),
    max  = map_dbl(raw_overlap, max, na.rm = TRUE),
    ratio_max_to_p975 = max / p975
  ) %>%
  select(region, alpha_code, p50, p975, max, ratio_max_to_p975) %>%
  arrange(region, desc(max)) %>%
  group_by(region) %>%
  slice_head(n = 5) %>%   # top 5 species by max, per region
  print(n = Inf)

# swap in whoever tops the table
susp <- "BOGU"
exposure_vals %>%
  filter(alpha_code == susp) %>%
  pull(raw_overlap) %>%
  .[[1]] %>%
  summary()


bogu <- exposure_vals %>%
  filter(region == "OCS-P 0561", alpha_code == "BOGU") %>%
  pull(raw_overlap) %>% .[[1]]

sort(bogu, decreasing = TRUE)[1:10]   # top 10 values
sum(bogu > 0.05)                       # how many are "blown out"

# does BOGU have any bootstrap layers with a near-zero study-area sum?
bogu_annual <- rast(dir(model_dir, pattern = "^BOGU_annual", full.names = TRUE))
totals <- global(bogu_annual, "sum", na.rm = TRUE)$sum
sort(totals)[1:5]        # smallest study-area totals
summary(totals)


bogu_annual <- rast(dir(model_dir, pattern = "^BOGU_annual", full.names = TRUE))
totals <- global(bogu_annual, "sum", na.rm = TRUE)$sum

# which layer(s) are the corrupt ones?
bad_layers <- which(totals > 1e6)   # normal totals are ~1e4, so this is generous
bad_layers
totals[bad_layers]

# what's the actual max cell value in a bad layer vs a normal one?
global(bogu_annual[[bad_layers]], "max", na.rm = TRUE)
global(bogu_annual[[bad_layers[1]]], fun = \(x) sort(x, decreasing = TRUE)[1:10])  # top cells


bogu_raw <- rast(dir(boot_dir, pattern = "^BOGU_", full.names = TRUE))
raw_totals <- global(bogu_raw, "sum", na.rm = TRUE)$sum

# order layers by total, descending, and show name + total together
bad_order <- order(raw_totals, decreasing = TRUE)

tibble(
  layer = names(bogu_raw)[bad_order],
  total = raw_totals[bad_order]
) %>% print(n = 20)




all_boot <- dir(boot_dir, pattern = "\\.tif$", full.names = TRUE)
scan <- map_dfr(all_boot, \(f) {
  r <- rast(f)
  tot <- global(r, "sum", na.rm = TRUE)$sum
  tibble(
    file = basename(f),
    max_total = max(tot, na.rm = TRUE),
    median_total = median(tot, na.rm = TRUE),
    n_blown = sum(tot > 100 * median(tot, na.rm = TRUE), na.rm = TRUE)  # layers >100x that file's median
  )
}) %>% arrange(desc(max_total))
print(scan, n = 30)




library(terra)
library(tidyterra)
library(tidyverse)

colo <- rast(dir(boot_dir, pattern = "^COLO_spring", full.names = TRUE))
tots <- global(colo, "sum", na.rm = TRUE)$sum

# rank layers: which are blown out, which are normal
med <- median(tots[is.finite(tots)], na.rm = TRUE)
ord <- order(tots, decreasing = TRUE)

tibble(layer = names(colo)[ord], total = tots[ord]) %>% print(n = 25)

# grab a few of the worst + one normal for comparison
worst  <- ord[1:3]                              # three most inflated
normal <- ord[which(is.finite(tots[ord]) & tots[ord] < 3*med)][1]   # a well-behaved one



# how many cells are actually crazy, and how detached are they?
walk(worst, \(i) {
  v <- values(colo[[i]], mat = FALSE)
  v <- v[!is.na(v)]
  cat("\n", names(colo)[i], "— total:", tots[i], "\n")
  cat("  n cells:", length(v),
      "| n non-finite:", sum(!is.finite(v)),
      "| n > 1e6:", sum(v > 1e6, na.rm = TRUE),
      "| n normal (<100):", sum(v < 100, na.rm = TRUE), "\n")
  cat("  top 20 finite:", paste(round(sort(v[is.finite(v)], decreasing = TRUE)[1:20]), collapse = ", "), "\n")
})


# clamp extreme values just for visualization (does NOT alter data, only display)
plot_layer <- function(i, cap = quantile(values(colo[[i]]), 0.999, na.rm = TRUE)) {
  r <- colo[[i]]
  r_disp <- clamp(r, upper = cap, values = TRUE)   # squash the blowout so structure shows
  ggplot() +
    geom_spatraster(data = r_disp) +
    geom_spatvector(data = weas, fill = NA, color = "red", linewidth = 0.4) +
    scale_fill_viridis_c(trans = "log1p", na.value = "transparent") +
    labs(title = names(colo)[i],
         subtitle = str_glue("total = {format(tots[i], scientific = TRUE, digits = 3)}")) +
    theme_minimal()
}

# worst three vs a normal one
plot_layer(worst[1])
plot_layer(worst[2])
plot_layer(worst[3])
plot_layer(normal)




# For a corrupt species: do the corrupt-denominator draws visibly drag down overlap?
# Compare overlap distribution using all draws vs. only clean-denominator draws.
colo_annual <- rast(dir(model_dir, pattern = "^COLO_annual", full.names = TRUE))
tots <- global(colo_annual, "sum", na.rm = TRUE)$sum
clean <- which(tots < 100 * median(tots[is.finite(tots)]))   # non-corrupt iterations

# COLO's raw overlap values, all vs clean-only
colo_overlap <- exposure_vals %>% filter(alpha_code == "COLO", region == "CA") %>% pull(raw_overlap) %>% .[[1]]
summary(colo_overlap)             # all draws
summary(colo_overlap[clean])      # only clean-denominator draws



# For a corrupt layer, WHERE are the bad cells? Same locations across iterations?
colo <- rast(dir(boot_dir, pattern = "^COLO_spring", full.names = TRUE))

bad_cells <- map(1:nlyr(colo), \(i) {
  which(values(loon[[i]], mat = FALSE) > 1e6)   # cell indices of corrupt cells
})

# are the same cell indices corrupt across many layers?
table(unlist(bad_cells)) %>% sort(decreasing = TRUE) %>% head(20)

# and map them — do they cluster somewhere meaningful (edges? a mask boundary?)
bad_idx <- unique(unlist(bad_cells))
bad_pts <- xyFromCell(loon, bad_idx)
plot(loon[[1]] |> clamp(upper = 100, values = TRUE), col = "grey90")
points(bad_pts, col = "red", pch = 20, cex 0.1)


#testing on a few different maps:

library(terra)
library(scales)

map_bad_cells <- function(prefix, boot_dir, ref = NULL,
                          pt_cex = 0.5, pt_pch = 20) {
  files <- dir(boot_dir, pattern = prefix, full.names = TRUE)
  stopifnot("no files matched — check the prefix/spelling" = length(files) > 0)
  r <- rast(files)
  
  finite_vals <- values(r, mat = FALSE)
  finite_vals <- finite_vals[is.finite(finite_vals)]
  hi <- quantile(finite_vals, 0.9999, na.rm = TRUE)
  thresh <- max(1e6, 1000 * hi)
  
  bad_cells <- map(1:nlyr(r), \(i) {
    v <- values(r[[i]], mat = FALSE)
    which(is.infinite(v) | (is.finite(v) & v > thresh))   # <-- Inf or huge, NOT NA
  })
  bad_idx <- unique(unlist(bad_cells))
  bad_pts <- xyFromCell(r, bad_idx)
  
  disp_cap <- quantile(finite_vals, 0.99, na.rm = TRUE)
  plot(clamp(r[[1]], upper = disp_cap, values = TRUE),
       main = str_glue("{prefix}  ({length(bad_idx)} bad cells)"))
  if (!is.null(ref)) plot(ref, add = TRUE, border = "blue")
  points(bad_pts, col = alpha("red", 0.7), pch = pt_pch, cex = pt_cex)
  
  invisible(list(threshold = thresh, n_bad = length(bad_idx),
                 bad_idx = bad_idx, bad_pts = bad_pts))
}


lesp <- map_bad_cells("^LESP_fall",           boot_dir, ref = weas, pt_cex = 0.1)
stts <- map_bad_cells("^STTS-SOSH-FFSH_fall", boot_dir, ref = weas, pt_cex = 0.1)
nofu <- map_bad_cells("^NOFU_fall",           boot_dir, ref = weas, pt_cex = 0.1)
colo <- map_bad_cells("^COLO_spring",         boot_dir, ref = weas, pt_cex = 0.1)






#backing way up, just looking at some of these wonky and normal maps.
# pick one file and one layer

#BOGU normal
r <- rast(dir(boot_dir, pattern = "^BOGU_fall", full.names = TRUE))
vals <- values(r[["bootstrap_001"]], mat = FALSE)
vals <- vals[!is.na(vals)]
ggplot(tibble(v = vals), aes(v)) +
  geom_histogram(bins = 100) +
  scale_y_log10() +
  labs(x = "cell value", y = "count (log scale)", title = "bootstrap_001") +
  theme_minimal()

ggplot(tibble(v = vals), aes(v, y = 0)) +
  geom_jitter(height = 0.1, alpha = 0.3, size = 0.5) +
  labs(x = "cell value", y = NULL, title = "bootstrap_001") +
  theme_minimal()

#BOGU abnormal
r <- rast(dir(boot_dir, pattern = "^BOGU_fall", full.names = TRUE))
vals <- values(r[["bootstrap_112"]], mat = FALSE)
vals <- vals[!is.na(vals)]
ggplot(tibble(v = vals), aes(v, y = 0)) +
  geom_jitter(height = 0.1, alpha = 0.3, size = 0.5) +
  labs(x = "cell value", y = NULL, title = "bootstrap_112") +
  theme_minimal()


#BOGU abnormal
r <- rast(dir(boot_dir, pattern = "^BOGU_spring", full.names = TRUE))
vals <- values(r[["bootstrap_125"]], mat = FALSE)
vals <- vals[!is.na(vals)]
ggplot(tibble(v = vals), aes(v, y = 0)) +
  geom_jitter(height = 0.1, alpha = 0.3, size = 0.5) +
  labs(x = "cell value", y = NULL, title = "bootstrap_125") +
  theme_minimal()

#BOGU CULPRIT (one massive cell, 841619.4, in lease area)
r <- rast(dir(boot_dir, pattern = "^BOGU_fall", full.names = TRUE))
vals <- values(r[["bootstrap_159"]], mat = FALSE)
vals <- vals[!is.na(vals)]
ggplot(tibble(v = vals), aes(v, y = 0)) +
  geom_jitter(height = 0.1, alpha = 0.3, size = 0.5) +
  labs(x = "cell value", y = NULL, title = "bootstrap_159") +
  theme_minimal()


#COLO normal
r <- rast(dir(boot_dir, pattern = "^COLO_spring", full.names = TRUE))
vals <- values(r[["bootstrap_185"]], mat = FALSE)
vals <- vals[!is.na(vals)]
ggplot(tibble(v = vals), aes(v, y = 0)) +
  geom_jitter(height = 0.1, alpha = 0.3, size = 0.5) +
  labs(x = "cell value", y = NULL, title = "bootstrap_185") +
  theme_minimal()

#COLO abnormal
r <- rast(dir(boot_dir, pattern = "^COLO_spring", full.names = TRUE))
vals <- values(r[["bootstrap_037"]], mat = FALSE)
vals <- vals[!is.na(vals)]
ggplot(tibble(v = vals), aes(v, y = 0)) +
  geom_jitter(height = 0.1, alpha = 0.3, size = 0.5) +
  labs(x = "cell value", y = NULL, title = "bootstrap_037") +
  theme_minimal()


#COLO abnormal
r <- rast(dir(boot_dir, pattern = "^COLO_spring", full.names = TRUE))
vals <- values(r[["bootstrap_010"]], mat = FALSE)
vals <- vals[!is.na(vals)]
ggplot(tibble(v = vals), aes(v, y = 0)) +
  geom_jitter(height = 0.1, alpha = 0.3, size = 0.5) +
  labs(x = "cell value", y = NULL, title = "bootstrap_010") +
  theme_minimal()



#look into the wayward bogu map:

r <- rast(dir(boot_dir, pattern = "^BOGU_fall", full.names = TRUE))
layer <- r[["bootstrap_159"]]

flag <- ifel(layer > 1e6, 1, 0)

plot(flag,
     col = c("grey85", "red"),
     legend = FALSE,
     main = "BOGU_fall bootstrap_159 — cells > 1e6, with lease 0561")

# outline just the 0561 lease in thin green
lease_0561 <- weas[weas$name == "OCS-P 0561", ]
plot(lease_0561, add = TRUE, border = "green", lwd = 1, col = NA)

lease_0561 <- weas[weas$name == "OCS-P 0561", ]

# values of bootstrap_159 within the 0561 polygon
in_lease <- terra::extract(layer, lease_0561)

# are any of them corrupt?
sum(in_lease[[2]] > 1e5, na.rm = TRUE)   # count of >1e6 cells inside the lease
max(in_lease[[2]], na.rm = TRUE)          # and the biggest value in there





#check to see if the bad cells cluster:

library(terra)
library(tidyverse)

cluster_check <- function(layer, thresh = 1e6) {
  bad <- ifel(layer > thresh, 1, NA)
  n_bad <- global(bad, "sum", na.rm = TRUE)[[1]]
  if (is.na(n_bad) || n_bad == 0) {
    return(tibble(n_bad_cells = 0, n_clusters = 0, largest_cluster = 0,
                  cluster_sizes = list(integer(0))))
  }
  clumps <- patches(bad, directions = 8, zeroAsNA = TRUE)
  fq <- freq(clumps)
  tibble(
    n_bad_cells     = n_bad,
    n_clusters      = nrow(fq),
    largest_cluster = max(fq$count),
    cluster_sizes   = list(sort(fq$count, decreasing = TRUE))
  )
}

r    <- rast(dir(boot_dir, pattern = "^PHAL_spring", full.names = TRUE))
tots <- global(r, "sum", na.rm = TRUE)$sum
blown <- which(tots > 100 * median(tots[is.finite(tots)]))

# run on each blown layer, tagging with the bootstrap name
results <- map(blown, \(i) {
  cluster_check(r[[i]]) %>%
    mutate(bootstrap = names(r)[i], .before = 1)
}) %>%
  list_rbind()

results %>% select(-cluster_sizes) %>% print(n = Inf)   # summary table
         # sizes per bootstrap if you want them

# a reference raster layer (any single layer — just need the grid geometry)
ref <- rast(dir(boot_dir, pattern = "^BOGU_fall", full.names = TRUE))[[1]]

# the five CA leases
ca_leases <- weas[weas$spatial_scale == "lease" & weas$state == "CA", ]

# count cells whose centroid falls in each lease, and total
extracted <- terra::extract(ref, ca_leases, cells = TRUE)
extracted %>%
  count(ID, name = "n_cells")          # cells per lease
nrow(extracted)                         # total across all 5 CA leases





# MASK BIOLOGICALLY IMPLAUSIBLE CELLS ------------------------------------

#' Set biologically implausible cell values to NA
#'
#' Replaces cells whose predicted density exceeds a plausibility ceiling with
#' NA, across every layer of a seasonal bootstrap stack. This removes the
#' extreme, biologically impossible values present in some raw bootstrap
#' predictions (which otherwise corrupt both the numerator and denominator of
#' the exposure calculation) before seasons are combined or overlaps computed.
#' The ceiling is species/model-specific and should be set from the species'
#' plausible maximum density; cells at or below it are left untouched, so
#' legitimate high-density predictions are preserved.
#'
#' @param x A SpatRaster (a model's seasonal bootstrap stack, or any raster
#'   stack of density values).
#' @param cutoff Numeric plausibility ceiling. Cells with values strictly
#'   greater than this are set to NA. Applied identically to every layer.
#'
#' @returns A SpatRaster of the same dimensions and layer names as `x`, with
#'   implausible cells set to NA.
#'
mask_outliers <- function(x, cutoff) {
  stopifnot(is.numeric(cutoff), length(cutoff) == 1)
  
  out <- ifel(x > cutoff, NA, x)   # cells above cutoff -> NA, all others kept
  names(out) <- names(x)           # preserve layer (bootstrap) names
  return(out)
}

colo_sp <- rast(dir(boot_dir, pattern = "^COLO_spring", full.names = TRUE))
colo_sp_clean <- mask_outliers(colo_sp, 700000)



vals <- values(colo_sp_clean[["bootstrap_037"]], mat = FALSE)
vals <- vals[!is.na(vals)]
ggplot(tibble(v = vals), aes(v, y = 0)) +
  geom_jitter(height = 0.1, alpha = 0.3, size = 0.5) +
  labs(x = "cell value", y = NULL, title = "clean bootstrap 037") +
  theme_minimal()




bogu_f <- rast(dir(boot_dir, pattern = "^BOGU_fall", full.names = TRUE))
bogu_f_clean <- mask_outliers(bogu_f, 700000)


vals <- values(bogu_f_clean[["bootstrap_159"]], mat = FALSE)
vals <- vals[!is.na(vals)]
ggplot(tibble(v = vals), aes(v, y = 0)) +
  geom_jitter(height = 0.1, alpha = 0.3, size = 0.5) +
  labs(x = "cell value", y = NULL, title = "bootstrap_159") +
  theme_minimal()

vals <- values(colo_sp_clean[["bootstrap_037"]], mat = FALSE)
vals <- vals[!is.na(vals)]
ggplot(tibble(v = vals), aes(v, y = 0)) +
  geom_jitter(height = 0.1, alpha = 0.3, size = 0.5) +
  labs(x = "cell value", y = NULL, title = "clean bootstrap 037") +
  theme_minimal()



overlap_dist <- function(d, v) {
  # Area-weighted extraction of density within each WEA polygon
  extracted_density <- terra::extract(d, v, exact = TRUE, touches = TRUE)
  in_wea_density <- as_tibble(extracted_density) %>%
    mutate(across(-c(ID, fraction), \(x) x * fraction)) %>%
    group_by(ID) %>%
    summarize(across(-fraction, sum)) %>%
    rename(region = ID) %>%
    mutate(region = v$name)
  
  # Study-area total per layer (na.rm drops the masked NA cells)
  total_density <- global(d, "sum", na.rm = TRUE)$sum
  names(total_density) <- names(d)
  
  # Proportional overlap per layer
  in_wea_density %>%
    mutate(across(-region, \(x) x / total_density[cur_column()])) %>%
    pivot_longer(-region, names_to = "layer", values_to = "prop_overlap")
}


raw_dist   <- overlap_dist(colo_sp,   weas) %>% rename(prop_raw   = prop_overlap)
clean_dist <- overlap_dist(colo_sp_clean, weas) %>% rename(prop_clean = prop_overlap)

comparison <- left_join(raw_dist, clean_dist, by = c("region", "layer"))

# summary of the shift, per region
comparison %>%
  group_by(region) %>%
  summarize(
    mean_raw    = mean(prop_raw,   na.rm = TRUE),
    mean_clean  = mean(prop_clean, na.rm = TRUE),
    median_raw   = median(prop_raw,   na.rm = TRUE),
    median_clean = median(prop_clean, na.rm = TRUE),
    max_raw     = max(prop_raw,   na.rm = TRUE),
    max_clean   = max(prop_clean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = Inf)



raw_dist   <- overlap_dist(bogu_f,   weas) %>% rename(prop_raw   = prop_overlap)
clean_dist <- overlap_dist(bogu_f_clean, weas) %>% rename(prop_clean = prop_overlap)

comparison <- left_join(raw_dist, clean_dist, by = c("region", "layer"))

# summary of the shift, per region
comparison %>%
  group_by(region) %>%
  summarize(
    mean_raw    = mean(prop_raw,   na.rm = TRUE),
    mean_clean  = mean(prop_clean, na.rm = TRUE),
    median_raw   = median(prop_raw,   na.rm = TRUE),
    median_clean = median(prop_clean, na.rm = TRUE),
    max_raw     = max(prop_raw,   na.rm = TRUE),
    max_clean   = max(prop_clean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = Inf)



# For a corrupt species: do the corrupt-denominator draws visibly drag down overlap?
# Compare overlap distribution using all draws vs. only clean-denominator draws.
colo_annual <- rast(dir(model_dir, pattern = "^COLO_annual", full.names = TRUE))
tots <- global(colo_annual, "sum", na.rm = TRUE)$sum
clean <- which(tots < 100 * median(tots[is.finite(tots)]))   # non-corrupt iterations

# COLO's raw overlap values, all vs clean-only
colo_overlap <- exposure_vals %>% filter(alpha_code == "COLO", region == "CA") %>% pull(raw_overlap) %>% .[[1]]
summary(colo_overlap)             # all draws
summary(colo_overlap[clean])      # only clean-denominator draws



r <- rast(dir(boot_dir, pattern = "^FTSP_fall", full.names = TRUE))
vals <- sort(values(r[["bootstrap_098"]], mat = FALSE))   # cell values in cell order (row-major)

plot(vals,
     type = "p", pch = ".",
     xlab = "cell index (raster order)",
     ylab = "cell value",
     main = "FTSP_spring bootstrap_098")

tail(vals, 500)


r <- rast(dir(boot_dir, pattern = "^FTSP_fall", full.names = TRUE))

maxes <- global(r, "max", na.rm = TRUE)$max   # max cell value per layer
names(maxes) <- names(r)

names(which(maxes > 1e5))



r <- rast(dir(boot_dir, pattern = "^PHAL_spring", full.names = TRUE))
vals <- sort(values(r[["bootstrap_098"]], mat = FALSE))   # cell values in cell order (row-major)

plot(vals,
     type = "p", pch = ".",
     xlab = "cell index (raster order)",
     ylab = "cell value",
     main = "FTSP_spring bootstrap_098")

tail(vals, 500)


r <- rast(dir(boot_dir, pattern = "^FTSP_fall", full.names = TRUE))

maxes <- global(r, "max", na.rm = TRUE)$max   # max cell value per layer
names(maxes) <- names(r)

names(which(maxes > 1e5))



r <- rast(dir(boot_dir, pattern = "^PHAL_spring", full.names = TRUE))

maxes <- global(r, "max", na.rm = TRUE)$max   # max cell value per layer
moremil <- global(r >= 1e6, "sum", na.rm = TRUE)$sum
hist(moremil)

names(maxes) <- names(r)

names(which(maxes > 1e6))   # which bootstrap layers have a cell over the cutoff




#more investigation scratch


# side-by-side summary of the two species, per region
comparison <- raw_exposure %>%
  filter(alpha_code %in% c("BOGU", "PFSH")) %>%
  mutate(
    n      = map_int(raw_overlap, length),
    min    = map_dbl(raw_overlap, min,    na.rm = TRUE),
    q25    = map_dbl(raw_overlap, \(x) quantile(x, 0.25, na.rm = TRUE)),
    median = map_dbl(raw_overlap, median, na.rm = TRUE),
    mean   = map_dbl(raw_overlap, mean,   na.rm = TRUE),
    q975   = map_dbl(raw_overlap, \(x) quantile(x, 0.975, na.rm = TRUE)),
    max    = map_dbl(raw_overlap, max,    na.rm = TRUE),
    max_over_q975 = max / q975
  ) %>%
  select(region, alpha_code, n, min, q25, median, mean, q975, max, max_over_q975)

comparison %>% arrange(region, alpha_code) %>% print(n = Inf)
# the two vectors for one region, sorted descending
raw_exposure %>%
  filter(alpha_code %in% c("BOGU", "PFSH"), region == "CA") %>%
  select(alpha_code, raw_overlap) %>%
  mutate(top_20 = map(raw_overlap, \(x) sort(x, decreasing = TRUE)[1:20])) %>%
  select(alpha_code, top_20) %>%
  unnest_wider(top_20, names_sep = "_")


plot_dat <- raw_exposure %>%
  filter(alpha_code %in% c("COLO", "STAL", "BOGU", "HAPE", "TOSP", "PFSH", "CAAU")) %>%
  unnest(raw_overlap)

# one region at a time is clearest — 0561 is where BOGU's blowout lives
plot_dat %>%
  filter(region == "all") %>%
  ggplot(aes(x = raw_overlap, y = alpha_code, color = alpha_code)) +
  geom_jitter(height = 0.15, alpha = 0.5, size = 1) +
  labs(x = "raw proportional overlap ", y = NULL,
       title = "BOGU vs PFSH — every bootstrap draw, all") +
  theme_minimal() +
  theme(legend.position = "none")






#' Summarize cell values by bootstrap iteration
#'
#' @param x A SpatRaster (seasonal bootstrap stack; one layer per iteration).
#' @param na.rm Drop NA cells before summarizing. Default TRUE.
#'
#' @returns A tibble with one row per layer, giving n (non-NA cells), mean,
#'   median, min, max, and the 2.5% and 97.5% quantiles of that layer's cells.
#'
summarize_cells <- function(x, na.rm = TRUE, thresh = NULL) {
  map_dfr(1:nlyr(x), \(i) {
    message(i, "/", nlyr(x), " - ", names(x)[i])
    
    v <- values(x[[i]], mat = FALSE)
    if (na.rm) v <- v[!is.na(v)]
    out <- tibble(
      bootstrap = names(x)[i],
      n         = length(v),
      mean      = mean(v),
      median    = median(v),
      min       = min(v),
      q025      = quantile(v, 0.025, names = FALSE),
      q975      = quantile(v, 0.975, names = FALSE),
      max       = max(v)
    )
    if (!is.null(thresh)) out$n_over <- sum(v > thresh)
    out
  })
}

bogu_f <- rast(dir(boot_dir, pattern = "^BOGU_fall", full.names = TRUE))
bogu_f_cell_summary <- summarize_cells(bogu_f)

phal_sp <- rast(dir(boot_dir, pattern = "^PHAL_spring", full.names = TRUE))
phal_sp_cell_summary <- summarize_cells(phal_sp)

loon_f <- rast(dir(boot_dir, pattern = "^LOON_fall", full.names = TRUE))
loon_f_cell_summary <- summarize_cells(loon_f)

lesp_f <- rast(dir(boot_dir, pattern = "^LESP_fall", full.names = TRUE))
lesp_f_cell_summary <- summarize_cells(lesp_f)

shear_f <- rast(dir(boot_dir, pattern = "^STTS-SOSH-FFSH_fall", full.names = TRUE))
shear_f_cell_summary <- summarize_cells(shear_f)

sagu_f <- rast(dir(boot_dir, pattern = "^SAGU_fall", full.names = TRUE))
sagu_f_cell_summary <- summarize_cells(sagu_f)

comu_su <- rast(dir(boot_dir, pattern = "^COMU_summer", full.names = TRUE))
comu_su_cell_summary <- summarize_cells(comu_su)

anmu_sp <- rast(dir(boot_dir, pattern = "^ANMU_spring", full.names = TRUE))
anmu_sp_cell_summary <- summarize_cells(anmu_sp)

bvsh_w<- rast(dir(boot_dir, pattern = "^BVSH_winter", full.names = TRUE))
bvsh_w_cell_summary <- summarize_cells(bvsh_w)

brpe_su <- rast(dir(boot_dir, pattern = "^BRPE_summer", full.names = TRUE))
brpe_su_cell_summary <- summarize_cells(brpe_su)


x <- comu_su
maxes <- global(x, "max", na.rm = TRUE)[, 1]
sums  <- global(x, "sum", na.rm = TRUE)[, 1]

par(mfrow = c(1, 2))
plot(sort(maxes), log = "y", main = "max per layer", ylab = "max (log)")
plot(sort(sums),  log = "y", main = "sum per layer", ylab = "sum (log)")
par(mfrow = c(1, 1))

#' Flag bootstrap iterations whose value is a MAD-based outlier (log scale)
#'
#' @param df Data frame, one row per bootstrap.
#' @param value_col Column to test (e.g. "max"). 
#' @param id_col Bootstrap identifier column.
#' @param k Number of MADs above the median to flag. Default 3.
#' @param log Work in log space (recommended given the value range). Default TRUE.
#'
#' @returns list with flagged IDs, the threshold, and kept/trimmed subsets.
#'
flag_mad_outliers <- function(df, value_col = "max", id_col = "bootstrap",
                              k = 3, log = TRUE) {
  v  <- df[[value_col]]
  id <- df[[id_col]]
  
  x <- if (log) log10(v) else v          # log10 for interpretability
  
  med <- median(x, na.rm = TRUE)
  md  <- mad(x, na.rm = TRUE)            # mad() already includes the 1.4826 scaling
  
  cutoff <- med + k * md
  is_out <- x > cutoff                    # one-sided: only flag the high end
  
  list(
    median   = med,
    mad      = md,
    cutoff_log   = cutoff,
    cutoff_value = if (log) 10^cutoff else cutoff,
    n_trimmed = sum(is_out, na.rm = TRUE),
    flagged   = id[is_out],
    trimmed   = df[is_out, , drop = FALSE],
    kept      = df[!is_out, , drop = FALSE]
  )
}


res <- flag_mad_outliers(comu_su_cell_summary, value_col = "max", k = 8)

res$n_trimmed
res$flagged
res$cutoff_value    # the actual max-value threshold, back on the raw scale





#' Flag bootstrap iterations whose value exceeds k times the median
#'
#' @param df Data frame, one row per bootstrap.
#' @param value_col Column to test (e.g. "max").
#' @param id_col Bootstrap identifier column.
#' @param k Multiplier on the median. Default 10.
#'
#' @returns list with the cutoff, flagged IDs, and kept/trimmed subsets.
#'
flag_median_mult <- function(df, value_col = "max", id_col = "bootstrap", k = 10) {
  v  <- df[[value_col]]
  id <- df[[id_col]]
  
  med    <- median(v, na.rm = TRUE)
  cutoff <- k * med
  is_out <- v > cutoff
  
  list(
    median    = med,
    cutoff    = cutoff,
    n_trimmed = sum(is_out, na.rm = TRUE),
    flagged   = id[is_out],
    trimmed   = df[is_out, , drop = FALSE],
    kept      = df[!is_out, , drop = FALSE]
  )
}



res <- flag_median_mult(comu_su_cell_summary, value_col = "max", k = 10)

res$cutoff
res$n_trimmed
res$flagged


compare_cutoffs_overlay <- function(df, value_col = "max",
                                    k_mad = 5, k_med = 100, model_name = "") {
  v <- df[[value_col]]
  
  logv    <- log10(v)
  mad_cut <- 10^(median(logv, na.rm = TRUE) + k_mad * mad(logv, na.rm = TRUE))
  med_cut <- k_med * median(v, na.rm = TRUE)
  
  n_mad <- sum(v > mad_cut, na.rm = TRUE)
  n_med <- sum(v > med_cut, na.rm = TRUE)
  
  sorted <- sort(v)
  
  plot(sorted, log = "y", pch = 1,
       xlab = "index", ylab = paste0(value_col, " (log)"),
       main = paste0(model_name, " — sorted ", value_col))
  
  abline(h = mad_cut, col = "red",  lwd = 2)
  abline(h = med_cut, col = "blue", lwd = 2)
  
  # label each line with its cutoff value, at the left edge, just above the line
  text(x = 1, y = mad_cut, labels = paste0("MAD = ", signif(mad_cut, 3)),
       col = "red",  pos = 3, adj = 0, cex = 0.8)
  text(x = 1, y = med_cut, labels = paste0(k_med, "\u00d7med = ", signif(med_cut, 3)),
       col = "blue", pos = 3, adj = 0, cex = 0.8)
  
  legend("topleft", bty = "n",
         legend = c(paste0("MAD k=", k_mad, " (", n_mad, " trimmed)"),
                    paste0(k_med, "\u00d7median (", n_med, " trimmed)")),
         col = c("red", "blue"), lwd = 2, cex = 0.8)
  
  invisible(list(mad_cutoff = mad_cut, med_cutoff = med_cut,
                 n_mad = n_mad, n_med = n_med))
}

compare_cutoffs_overlay(phal_sp_cell_summary, model_name = "PHAL_sp")
compare_cutoffs_overlay(comu_su_cell_summary, model_name = "COMU_su")
compare_cutoffs_overlay(sagu_f_cell_summary, model_name = "SAGU_f")
compare_cutoffs_overlay(shear_f_cell_summary, model_name = "Shear_f")
compare_cutoffs_overlay(lesp_f_cell_summary, model_name = "LESPf")
compare_cutoffs_overlay(loon_f_cell_summary, model_name = "loon_f")
compare_cutoffs_overlay(bogu_f_cell_summary, model_name = "BOGU_f")
compare_cutoffs_overlay(anmu_sp_cell_summary, model_name = "ANMUsp")
compare_cutoffs_overlay(bvsh_w_cell_summary, model_name = "bvsh_w")
compare_cutoffs_overlay(brpe_su_cell_summary, model_name = "BRPE_su")



#' Report bootstrap losses under Option C (drop union of bad indices per model)
#'
#' Scans every seasonal bootstrap file in a directory, flags iterations whose
#' cell-max exceeds k * median(max) within that seasonal stack, then groups by
#' model to report the union of flagged indices across seasons — i.e. the
#' iterations that would be dropped from that model's annual combination.
#'
#' @param boot_dir Directory of seasonal bootstrap .tif files, named
#'   "{MODEL}_{season}_...".
#' @param k Multiplier on the median cell-max for flagging. 
#' @param season_pattern Regex capturing the season token, used to parse the
#'   model name (everything before it).
#'
#' @returns A list with `per_file` (flagged indices per seasonal stack) and
#'   `per_model` (union of flagged indices per model, and retained count).
#'
boot_loss_report <- function(boot_dir, k = 100,
                             season_pattern = "_(spring|summer|fall|winter)_") {
  files <- dir(boot_dir, pattern = "\\.tif$", full.names = TRUE)
  
  # --- per file: flag indices whose max > k * median(max) ---
  per_file <- map_dfr(files, \(f) {
    r      <- rast(f)
    maxes  <- global(r, "max", na.rm = TRUE)[, 1]
    cutoff <- k * median(maxes, na.rm = TRUE)
    bad_lgl <- maxes > cutoff
    
    model  <- str_extract(basename(f), paste0("^.+(?=", season_pattern, ")"))
    season <- str_extract(basename(f), season_pattern) |> str_remove_all("_")
    
    tibble(
      file         = basename(f),
      model        = model,
      season       = season,
      n_layers     = nlyr(r),
      cutoff       = cutoff,
      n_flagged    = sum(bad_lgl, na.rm = TRUE),
      flagged_idx  = list(sort(which(bad_lgl)))   # index positions, in order
    )
  })
  
  # --- per model: union of flagged indices across that model's seasons ---
  per_model <- per_file %>%
    group_by(model) %>%
    summarize(
      n_seasons        = n(),
      layers_per_season = max(n_layers),          # should be 200 each
      union_bad_idx    = list(sort(unique(unlist(flagged_idx)))),
      n_dropped        = length(unique(unlist(flagged_idx))),
      n_retained       = max(n_layers) - length(unique(unlist(flagged_idx))),
      .groups = "drop"
    ) %>%
    arrange(n_retained)   # worst-hit models first
  
  list(per_file = per_file, per_model = per_model)
}

loss <- boot_loss_report(boot_dir, k = 100)

# per-model summary: how many annuals you'd retain after combining
loss$per_model %>% select(model, n_seasons, n_dropped, n_retained) %>% print(n = Inf)

# drill into a specific model's flagged indices, per season
loss$per_file %>%
  filter(model == "LOON") %>%
  select(season, n_flagged, flagged_idx)

# see the actual union of dropped indices for a model
loss$per_model %>% filter(model == "LOON") %>% pull(union_bad_idx)



loss_by_k <- map_dfr(c(10, 100, 1000), \(kk) {
  boot_loss_report(boot_dir, k = kk)$per_model %>%
    mutate(k = kk, .before = 1) %>%
    select(k, model, n_dropped, n_retained)
})

# wide view: retained count per model at each k
loss_by_k %>%
  select(k, model, n_retained) %>%
  pivot_wider(names_from = k, values_from = n_retained, names_prefix = "k")

