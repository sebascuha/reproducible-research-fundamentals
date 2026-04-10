# Reproducible Research Fundamentals 
# 02. Data Construction

library(tidyverse)

# Instructions: 
# 1. Replace placeholders (e.g., "path/to/your/dataset.csv") with your actual file paths.
# 2. Update column names as needed (e.g., 'area_variable', 'consumption_variable').
# 3. Follow each exercise step, ensuring reproducible workflows.

# Preliminary - Load Data ----
# Load Household (HH) data
hh_data <- read.csv("path/to/your/intermediate_data.csv")

# Load Household-Member data (if applicable)
# member_data <- read.csv("path/to/your/member_data.csv")

# Exercise 1: Standardize Conversion Values ----
# Define conversion rates (update if needed)
acre_conv <- YOUR_CONVERSION_FACTOR  # e.g., 2.47 for hectare to acre
usd_conv <- YOUR_CURRENCY_CONVERSION  # e.g., 0.00037 for local currency to USD

# Data Construction: Household-Level ----

# Convert units (e.g., farming area from hectares to acres)
hh_data <- hh_data %>%
    mutate(area_acre = case_when(
        area_unit_column == "Acre" ~ area_variable,  # If already in acres
        area_unit_column == "Hectare" ~ area_variable * acre_conv  # Convert hectares to acres
    )) %>%
    mutate(area_acre = replace_na(area_acre, 0)) 

# Convert consumption values to USD (or other currency)
hh_data <- hh_data %>%
    mutate(across(c(food_consumption, nonfood_consumption), 
                  ~ .x * usd_conv, 
                  .names = "{.col}_usd"))

# Exercise 2: Handle Outliers ----

# Customized function for Winsorization
winsor_function <- function(dataset, var, min = 0.05, max = 0.95){
    var_sym <- sym(var)
    
    percentiles <- quantile(
        dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
    )
    
    min_percentile <- percentiles[1]
    max_percentile <- percentiles[2]
    
    dataset %>%
        mutate(
            !!paste0(var, "_w") := case_when(
                is.na(!!var_sym) ~ NA_real_,
                !!var_sym <= min_percentile ~ percentiles[1],
                !!var_sym >= max_percentile ~ percentiles[2],
                TRUE ~ !!var_sym
            )
        )
}

# Apply Winsorization to selected variables
win_vars <- c("area_acre", "food_consumption_usd", "nonfood_consumption_usd")  # Replace with actual column names

for (var in win_vars) {
    hh_data <- winsor_function(hh_data, var)
}

# Exercise 3: Merge Household and Treatment Data ----

# Load treatment status data
treat_status <- read.csv("path/to/your/treatment_data.csv")

# Merge household data with treatment data
final_hh_data <- hh_data %>%
    left_join(treat_status, by = "merge_column")  # Replace 'merge_column' with actual key column

# Exercise 4: Save Final Dataset ----

# Save the final cleaned and merged dataset
write.csv(final_hh_data, "path/to/your/final_data.csv", row.names = FALSE)

# Notes:
# - Ensure all file paths are correct.
# - Verify column names match your dataset.
# - Customize the winsorization threshold if needed.
