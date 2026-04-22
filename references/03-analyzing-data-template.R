# Reproducible Research Fundamentals 
# 03. Data Analysis

# Libraries
library(dplyr)
library(modelsummary)
library(stargazer)
library(ggplot2)
library(tidyr)

# Load data 

# Household-level data
# change your path 
hh_data <- read.csv("Data/Final/TZA_CCT_analysis.csv")

# Exercise 1:
# Summary statistics ----

# TODO: Select variables for summary statistics
# Replace the placeholders with the correct variables

summary_table <- datasummary(
  VAR1 + VAR2 + VAR3 + VAR4 + VAR5 ~ 
    as.factor(district_name) * (Mean + SD), 
  data = hh_data,
  title = "Summary Statistics by District",
  output = file.path("Outputs", "summary_table.csv")  # Save as CSV
)

# Exercise 2:
# Regressions ----

# TODO: Define the dependent and independent variables
# Model 1: Replace DEPENDENT_VAR with the variable of interest
model1 <- lm(DEPENDENT_VAR ~ treatment, data = hh_data)

# TODO: Add control variables
# Model 2: Modify the list of control variables as needed
model2 <- lm(DEPENDENT_VAR ~ treatment + CONTROL_VAR1 + CONTROL_VAR2, data = hh_data)

# TODO: Customize regression table output
stargazer(
  model1, model2,
  title = "Regression Table",
  covariate.labels = c("Treatment", "CONTROL_VAR1", "CONTROL_VAR2"),
  dep.var.labels = c("DEPENDENT_VAR Label"),
  header = FALSE,
  notes = "Standard errors in parentheses",
  out = file.path("Outputs", "regression_table.tex")
)

# Exercise 3: Histogram for treatment group ----

# TODO: Select a variable for the histogram
ggplot(hh_data %>% 
         filter(treatment == 1), aes(x = HISTOGRAM_VAR)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "purple", alpha = 0.7) +  
  labs(
    title = "Distribution of HISTOGRAM_VAR in Treatment Group",
    x = "HISTOGRAM_VAR Label",
    y = "Frequency"
  ) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "none"
  )

# Exercise 4: Save the plot ----

# TODO: Save the histogram output
ggsave(file.path("Outputs", "fig1.png"), width = 10, height = 6)
