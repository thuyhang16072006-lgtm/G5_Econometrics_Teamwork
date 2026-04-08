library(dplyr)
library(tidyr)
library(gt)

# 1. Read data
df <- readRDS("data/processed/data_processed.rds")

# 2. Preprocessing
df_prep <- df %>%
  mutate(
    wage_val = exp(ln_wage),
    mismatch_val = as.numeric(mismatch),
    female_group = ifelse(female == 1, "Female", "Male")
  ) %>%
  select(female_group, wage_val, mismatch_val, exp, exp2, urban, marital)

# 3. Calculate Stats
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
  mutate(female_group = "Full_Sample") # ĐỔI THÀNH GẠCH DƯỚI

# 4. Combine and Pivot
table1_final <- bind_rows(stats_full, stats_by_gender) %>%
  mutate(female_group = factor(female_group, levels = c("Full_Sample", "Male", "Female"))) %>%
  pivot_wider(
    names_from = female_group,
    values_from = c(Mean, SD, Min, Max),
    names_glue = "{female_group}_{.value}"
  ) %>%
  mutate(Variable = case_when(
    Variable == "wage_val" ~ "Wage (VND)",
    Variable == "mismatch_val" ~ "Mismatch (1=Yes)",
    Variable == "exp" ~ "Experience (years)",
    Variable == "exp2" ~ "Experience Squared",
    Variable == "urban" ~ "Urban (1=Yes)",
    Variable == "marital" ~ "Marital (1=Yes)",
    TRUE ~ Variable
  )) %>%
  # 5. Render table using gt
  gt() %>%
  fmt_number(columns = matches("Mean|SD"), decimals = 2) %>%
  fmt_number(columns = matches("Min|Max"), decimals = 0) %>%
  tab_spanner(label = "Full Sample", columns = starts_with("Full_Sample_")) %>%
  tab_spanner(label = "Male", columns = starts_with("Male_")) %>%
  tab_spanner(label = "Female", columns = starts_with("Female_")) %>%
  cols_label(
    # Dùng dấu huyền để bảo vệ tên cột nếu cần, ở đây đã dùng Full_Sample_ nên an toàn
    Full_Sample_Mean = "Mean", Full_Sample_SD = "SD", Full_Sample_Min = "Min", Full_Sample_Max = "Max",
    Male_Mean = "Mean", Male_SD = "SD", Male_Min = "Min", Male_Max = "Max",
    Female_Mean = "Mean", Female_SD = "SD", Female_Min = "Min", Female_Max = "Max"
  ) %>%
  tab_header(
    title = "Table 1: Descriptive Statistics",
    subtitle = "Analysis of Wage Penalty and Work Experience"
  )

# Display
table1_final