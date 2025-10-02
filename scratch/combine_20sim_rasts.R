#this is the (ugly) rough outline of how to do this if I was just combining two, but I'm not sure how to make it work with all 50 without giving R a small meltdown. Let's ask max



library(terra)

test_stack_1 <- rast("/Volumes/seagate/20_distribution_rasts_1.tif")
nlyr(test_stack_1)

test_stack_2 <- rast("/Volumes/seagate/20_distribution_rasts_2.tif")
nlyr(test_stack_2)

combined_stack <- c(test_stack_1, test_stack_2)

nlyr(combined_stack)

current_names <- names(combined_stack)



# Split names into groups by species (and expert if applicable)
library(dplyr)  # or you can do this with base R

# Create a data frame to work with
name_df <- data.frame(
  original_name = current_names,
  index = 1:length(current_names)
)

# Extract grouping key (species + expert if present)
name_df$group_key <- sapply(current_names, function(name) {
  if (grepl("_expert_", name)) {
    # Remove the sim number but keep expert
    sub("_sim_[0-9]+_expert_", "_expert_", name)
  } else {
    # Remove the sim number
    sub("_sim_[0-9]+$", "", name)
  }
})

# Number within each group
name_df <- name_df %>%
  group_by(group_key) %>%
  mutate(new_sim_num = row_number()) %>%
  ungroup()

# Reconstruct names
name_df$new_name <- sapply(1:nrow(name_df), function(i) {
  key <- name_df$group_key[i]
  num <- name_df$new_sim_num[i]
  
  if (grepl("_expert_", key)) {
    paste0(key, "_sim_", num)
  } else {
    paste0(key, "_sim_", num)
  }
})

# Assign new names
names(combined_stack) <- name_df$new_name


names(combined_stack)


# Get current names
current_names <- names(combined_stack)

# Create a sorting key with proper numeric ordering
sort_key <- sapply(current_names, function(name) {
  # Extract the base (species + expert if present)
  base <- sub("_sim_[0-9]+$", "", name)
  
  # Extract sim number
  sim_num <- as.numeric(sub(".*_sim_", "", name))
  
  # Create sortable string: pad sim number with zeros
  paste0(base, "_sim_", sprintf("%03d", sim_num))
})

# Sort based on this key
sort_order <- order(sort_key)

# Reorder the stack
combined_stack <- combined_stack[[sort_order]]





