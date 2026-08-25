

set.seed(123)

# Orange: moderate-high exposure, fairly tight distribution
orange <- c(0.0038, 0.0041, 0.0043, 0.0044, 0.0045, 0.0046, 0.0047, 0.0048,
            0.0049, 0.0050, 0.0050, 0.0051, 0.0052, 0.0053, 0.0054, 0.0055,
            0.0056, 0.0057, 0.0035, 0.0058)

# Purple: lower bulk than orange, but a right tail that exceeds orange's max
purple <- c(0.0000, 0.0015, 0.0020, 0.0022, 0.0025, 0.0027, 0.0028, 0.0030,
            0.0031, 0.0032, 0.0033, 0.0034, 0.0036, 0.0038, 0.0040, 0.0045,
            0.0055, 0.0062, 0.0068, 0.0070)

# Blue: low mean, low variation
blue <- c(0.0002, 0.0003, 0.0003, 0.0004, 0.0004, 0.0004, 0.0005, 0.0005,
          0.0005, 0.0005, 0.0006, 0.0006, 0.0006, 0.0006, 0.0007, 0.0007,
          0.0007, 0.0008, 0.0008, 0.0009)

species_raw <- list(orange = orange, purple = purple, blue = blue)

map_dfr(species_raw, \(x) tibble(
  n = length(x), mean = mean(x), median = median(x),
  min = min(x), max = max(x), sd = sd(x)
), .id = "species")

library(tidyverse)

species_colors <- c(orange = "#E8823C", purple = "#8A5B8C", blue = "#3D6FA8")

raw_df <- map_dfr(species_raw, \(x) tibble(raw_overlap = x), .id = "species") %>%
  mutate(species = factor(species, levels = c("orange", "purple", "blue")))

mean_df <- raw_df %>%
  group_by(species) %>%
  summarize(mean_val = mean(raw_overlap))

ggplot(raw_df, aes(raw_overlap, fill = species)) +
  geom_histogram(binwidth = 0.0003, boundary = 0, color = "white", linewidth = 0.2) +
  geom_vline(data = mean_df, aes(xintercept = mean_val),
             linetype = "dashed", color = "grey20", linewidth = 0.5) +
  facet_wrap(~ species, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = species_colors, guide = "none") +
  scale_x_continuous(limits = c(0, 0.007), expand = c(0, 0)) +
  labs(x = "Raw proportional overlap", y = "Count") +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0)
  )


raw_sensitivity <- tribble(
  ~species, ~collision, ~displacement,
  "blue",   0.00, 0.10,
  "orange", 0.00, 0.60,
  "purple", 0.18, 0.25
)

rescaled <- raw_sensitivity %>%
  mutate(
    collision_r    = (collision - min(collision)) / (max(collision) - min(collision)),
    displacement_r = (displacement - min(displacement)) / (max(displacement) - min(displacement)),
    combined       = collision_r + displacement_r
  )

final <- rescaled %>%
  mutate(
    ratio = (combined - min(combined)) / (max(combined) - min(combined)),
    sensitivity = 0.5 * 4^ratio
  )

final %>% select(species, collision, collision_r, displacement, displacement_r, combined, sensitivity)



library(tidyverse)

# --- exposure: rescale the raw draws to [0.5, 2.0], pooled across the 3 species ---
# (plain min-max rescale, matching Eq. 1 in the general framework figure)
rescale_pool <- function(x_list) {
  all_x <- unlist(x_list)
  lo <- min(all_x); hi <- max(all_x)
  log_y <- log(0.5) + (log(2) - log(0.5)) * (all_x - lo) / (hi - lo)
  split(exp(log_y), rep(names(x_list), lengths(x_list)))
}

scaled_exposure <- rescale_pool(species_raw)   # species_raw = orange/purple/blue raw draws from earlier


scaled_df <- map_dfr(scaled_exposure, \(x) tibble(scaled_overlap = x), .id = "species") %>%
  mutate(species = factor(species, levels = c("orange", "purple", "blue")))

scaled_mean_df <- scaled_df %>%
  group_by(species) %>%
  summarize(mean_val = mean(scaled_overlap))

ggplot(scaled_df, aes(scaled_overlap, fill = species)) +
  geom_histogram(binwidth = 0.1, boundary = 0.5, color = "white", linewidth = 0.2) +
  geom_vline(data = scaled_mean_df, aes(xintercept = mean_val),
             linetype = "dashed", color = "grey20", linewidth = 0.5) +
  facet_wrap(~ species, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = species_colors, guide = "none") +
  scale_x_continuous(limits = c(0.5, 2), expand = c(0, 0)) +
  labs(x = "Rescaled exposure", y = "Count") +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0)
  )

# --- sensitivity: from your locked-in table ---
sensitivity <- c(blue = 0.5, orange = 1.45, purple = 2.0)

# --- status: standard 5-category mapping on the framework's 0.5-2 scale ---
status_scale <- c(LC = 0.5, NT = 0.71, VU = 1.0, EN = 1.41, CR = 2.0)

# <<< EDIT THESE THREE TO TRY DIFFERENT IUCN CATEGORIES >>>
status <- c(
  blue   = unname(status_scale["CR"]),
  orange = unname(status_scale["NT"]),
  purple = unname(status_scale["VU"])
)

w <- c(3, 2, 1)   # exposure, sensitivity, status weight exponents

# --- build the full priority score distribution per species ---
priority_dist <- imap_dfr(scaled_exposure, \(x, sp) {
  tibble(
    species = sp,
    priority = x^w[1] * sensitivity[sp]^w[2] * status[sp]^w[3]
  )
}) %>%
  mutate(species = factor(species, levels = c("orange", "purple", "blue")))

# --- summary ---
priority_dist %>%
  group_by(species) %>%
  summarize(mean = mean(priority), median = median(priority), lower = quantile(priority, 0.025), upper = quantile(priority, 0.975),
            min = min(priority), max = max(priority), .groups = "drop")


priority_dist_fig <- priority_dist  %>% 
  group_by(species) %>%
  summarize(mean = mean(priority), median = median(priority), lower = quantile(priority, 0.025), upper = quantile(priority, 0.975),
            min = min(priority), max = max(priority), .groups = "drop")
# --- quick visual ---
species_colors <- c(orange = "#E8823C", purple = "#8A5B8C", blue = "#3D6FA8")
ggplot(priority_dist, aes(priority, fill = species)) +
  geom_histogram(bins = 20, color = "white", linewidth = 0.2) +
  geom_vline(data = priority_dist_fig, aes(xintercept = mean),
             linetype = "dashed", color = "grey20", linewidth = 0.5) +
  geom_vline(data = priority_dist_fig, aes(xintercept = lower),
             linetype = "dashed", color = "pink", linewidth = 0.5) +
  geom_vline(data = priority_dist_fig, aes(xintercept = upper),
             linetype = "dashed", color = "pink", linewidth = 0.5) +
  facet_wrap(~ species, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = species_colors, guide = "none") +
  labs(x = "Vulnerability score", y = "Count") +
  theme_classic() +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold", hjust = 0))



set.seed(1)
n_mc <- 20

rank_mc <- map_dfr(1:n_mc, \(i) {
  draw <- imap_dfr(scaled_exposure, \(x, sp) tibble(species = sp, exposure_draw = sample(x, 1)))
  draw %>%
    mutate(
      priority = exposure_draw^w[1] * sensitivity[species]^w[2] * status[species]^w[3],
      rank     = rank(-priority, ties.method = "min"),
      iteration = i
    )
})

rank_mc <- rank_mc %>% mutate(species = factor(species, levels = c("orange", "purple", "blue")))

# summary: rank range per species across the 20 draws
rank_mc %>%
  group_by(species) %>%
  summarize(min_rank = min(rank), max_rank = max(rank), mean_rank = mean(rank), .groups = "drop")

