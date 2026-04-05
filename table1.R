library(dplyr)
library(tidyr)
library(gt)

# 1. Read data from file
df <- readRDS("data/processed/data_processed.rds")

# 2. Data preprocessing (df_prep)
df_prep <- df %>%
  mutate(
    # Calculate real wage from ln_wage
    wage_val = exp(ln_wage),
    # Convert mismatch to numeric (already 0/1)
    mismatch_val = as.numeric(mismatch),
    # Define gender groups for column splitting
    female_group = ifelse(female == 1, "Female", "Male")
  ) %>%
  # Select only the variables used in the model
  select(female_group, wage_val, mismatch_val, exp, exp2, urban, marital)

# 3. Calculate Stats for Gender and Full Sample
# Group: Male & Female
stats_by_gender <- df_prep %>%
  pivot_longer(cols = -female_group, names_to = "Variable", values_to = "Value") %>%
  group_by(female_group, Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

# Group: Full Sample
stats_full <- df_prep %>%
  pivot_longer(cols = -female_group, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(female_group = "Full Sample")

# 4. Combine data and Pivot Wider
table1_final <- bind_rows(stats_full, stats_by_gender) %>%
  mutate(female_group = factor(female_group, levels = c("Full Sample", "Male", "Female"))) %>%
  pivot_wider(
    names_from = female_group,
    values_from = c(Mean, SD, Min, Max),
    names_glue = "{female_group}_{.value}"
  ) %>%
  mutate(Variable = case_when(
    Variable == "wage_val" ~ "wage_val (VND)",
    Variable == "mismatch_val" ~ "mismatch_val (1=Yes)",
    Variable == "exp" ~ "exp (years)",
    Variable == "exp2" ~ "exp2",
    Variable == "urban" ~ "urban (1=Yes)",
    Variable == "marital" ~ "marital (1=Yes)",
    TRUE ~ Variable
  )) %>%
  # 5. Render table using gt
  gt() %>%
  fmt_number(columns = contains("Mean") | contains("SD"), decimals = 2) %>%
  fmt_number(columns = contains("Min") | contains("Max"), decimals = 0) %>%
  tab_spanner(label = "Full Sample", columns = starts_with("Full Sample_")) %>%
  tab_spanner(label = "Male", columns = starts_with("Male_")) %>%
  tab_spanner(label = "Female", columns = starts_with("Female_")) %>%
  cols_label(
    Full Sample_Mean = "Mean", Full Sample_SD = "SD", Full Sample_Min = "Min", Full Sample_Max = "Max",
    Male_Mean = "Mean", Male_SD = "SD", Male_Min = "Min", Male_Max = "Max",
    Female_Mean = "Mean", Female_SD = "SD", Female_Min = "Min", Female_Max = "Max"
  ) %>%
  tab_header(
    title = "Table 1: Descriptive Statistics",
    subtitle = "Analysis of Wage Penalty and Work Experience"
  )

# Display the final table
table1_final