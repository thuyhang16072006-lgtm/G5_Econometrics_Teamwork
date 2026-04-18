# =============================================================================
# 02_eda.R  —  Exploratory Data Analysis & Descriptive Statistics
# Project  : Horizontal Mismatch × Experience × Gender  (LFS 2018)
# Input    : data/processed/data_processed.rds
# Output   : output/tables/Table1_summary_stats.html
#            output/tables/Table2_wage_gender_mismatch.html
#            output/tables/Table3_wage_gap_exp.csv
#            output/tables/Table4_subsample_comparison.html
#            output/figures/Fig1_density_lnwage.png
#            output/figures/Fig2_wage_exp_profile.png
#            output/figures/Fig3_wage_gap_trend.png
# =============================================================================

source("scripts/00_setup.R")   # packages, OUT_TAB, OUT_FIG, options

library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)


# --------------------------------------------------------------------------- #
#  0.  Load & prep                                                             #
# --------------------------------------------------------------------------- #

df <- readRDS("data/processed/data_processed.rds")

# wage_monthly used across all descriptive tables (spec requirement)
df <- df %>%
  mutate(
    mismatch_label = ifelse(mismatch == 1, "Mismatched", "Matched"),
    gender_lab     = ifelse(female   == 1, "Female",     "Male")
  )


# ============================================================================ #
#  TABLE 1  —  Descriptive Statistics: Full / Male / Female                   #
# ============================================================================ #

df_t1 <- df %>%
  select(gender_lab, wage_monthly, mismatch, exp, edu_level, urban, marital)

stats_full <- df_t1 %>%
  pivot_longer(cols = -gender_lab, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD   = sd(Value,   na.rm = TRUE),
    Min  = min(Value,  na.rm = TRUE),
    Max  = max(Value,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(female_group = "Full_Sample")

stats_gender <- df_t1 %>%
  pivot_longer(cols = -gender_lab, names_to = "Variable", values_to = "Value") %>%
  group_by(female_group = gender_lab, Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD   = sd(Value,   na.rm = TRUE),
    Min  = min(Value,  na.rm = TRUE),
    Max  = max(Value,  na.rm = TRUE),
    .groups = "drop"
  )

table1 <- bind_rows(stats_full, stats_gender) %>%
  mutate(female_group = factor(female_group,
                               levels = c("Full_Sample", "Male", "Female"))) %>%
  pivot_wider(
    names_from  = female_group,
    values_from = c(Mean, SD, Min, Max),
    names_glue  = "{female_group}_{.value}"
  ) %>%
  mutate(Variable = case_when(
    Variable == "wage_monthly" ~ "Monthly Wage (VND '000)",
    Variable == "mismatch"     ~ "Mismatch (1=Yes)",
    Variable == "exp"          ~ "Experience (years)",
    Variable == "edu_level"    ~ "Education Level",
    Variable == "urban"        ~ "Urban (1=Yes)",
    Variable == "marital"      ~ "Married (1=Yes)",
    TRUE ~ Variable
  )) %>%
  gt() %>%
  fmt_number(columns = matches("Mean|SD|Min|Max"), decimals = 2) %>%
  tab_spanner(label = "Full Sample", columns = starts_with("Full_Sample_")) %>%
  tab_spanner(label = "Male",        columns = starts_with("Male_"))        %>%
  tab_spanner(label = "Female",      columns = starts_with("Female_"))      %>%
  cols_label(
    Full_Sample_Mean = "Mean", Full_Sample_SD = "SD",
    Full_Sample_Min  = "Min",  Full_Sample_Max = "Max",
    Male_Mean   = "Mean", Male_SD   = "SD", Male_Min   = "Min", Male_Max   = "Max",
    Female_Mean = "Mean", Female_SD = "SD", Female_Min = "Min", Female_Max = "Max"
  ) %>%
  tab_header(
    title    = "Table 1: Descriptive Statistics",
    subtitle = "Full sample and by gender — LFS 2018 Manufacturing, College+"
  )

gtsave(table1, paste0(OUT_TAB, "Table1_summary_stats.html"))
cat("✓ Table 1 done\n")


# ============================================================================ #
#  TABLE 2  —  Wage by Gender × Mismatch  (+ t-test)                         #
# ============================================================================ #

table2_data <- df %>%
  group_by(female) %>%
  mutate(mismatch_f = factor(mismatch))
  summarise(
    wage_matched    = mean(wage_monthly[mismatch == 0], na.rm = TRUE),
    wage_mismatched = mean(wage_monthly[mismatch == 1], na.rm = TRUE),
    pct_mismatch    = mean(mismatch == 1, na.rm = TRUE) * 100,
    p_val           = t.test(wage_monthly ~ mismatch,
                             data = pick(everything()))$p.value,
    .groups = "drop"
  ) %>%
  mutate(Gender = ifelse(female == 1, "Female", "Male")) %>%
  select(Gender, wage_matched, wage_mismatched, pct_mismatch, p_val)

table2 <- table2_data %>%
  gt() %>%
  fmt_number(columns = c(wage_matched, wage_mismatched), decimals = 0) %>%
  fmt_number(columns = pct_mismatch, decimals = 1)                      %>%
  fmt_number(columns = p_val,        decimals = 3)                      %>%
  cols_label(
    Gender          = "Gender",
    wage_matched    = "Avg Wage — Matched (VND '000)",
    wage_mismatched = "Avg Wage — Mismatched (VND '000)",
    pct_mismatch    = "Mismatch Rate (%)",
    p_val           = "p-value (t-test)"
  ) %>%
  tab_header(
    title    = "Table 2: Wage by Gender and Mismatch Status",
    subtitle = "Mean monthly wages and t-test for wage equality"
  )

gtsave(table2, paste0(OUT_TAB, "Table2_wage_gender_mismatch.html"))
cat("✓ Table 2 done\n")


# ============================================================================ #
#  TABLE 3  —  Wage Gap by Experience Group                                   #
# ============================================================================ #

wage_gap_exp <- df %>%
  mutate(exp_group = case_when(
    exp <  5  ~ "0–5 years",
    exp < 10  ~ "5–10 years",
    exp < 20  ~ "10–20 years",
    TRUE      ~ "20+ years"
  ),
  exp_group = factor(exp_group,
                     levels = c("0–5 years", "5–10 years",
                                "10–20 years", "20+ years"))
  ) %>%
  group_by(exp_group, mismatch) %>%
  summarise(
    mean_wage = mean(wage_monthly, na.rm = TRUE),
    n         = n(),
    .groups   = "drop"
  ) %>%
  pivot_wider(
    names_from  = mismatch,
    values_from = c(mean_wage, n),
    names_glue  = "{.value}_{mismatch}"
  ) %>%
  rename(
    wage_matched    = mean_wage_0,
    wage_mismatched = mean_wage_1,
    n_matched       = n_0,
    n_mismatched    = n_1
  ) %>%
  mutate(
    wage_gap  = round(wage_matched - wage_mismatched, 0),
    gap_pct   = round(wage_gap / wage_matched * 100, 1),
    wage_matched    = round(wage_matched,    0),
    wage_mismatched = round(wage_mismatched, 0)
  )
# If gap_pct narrows with exp -> "remedy"; widens -> "widen"

write.csv(wage_gap_exp, paste0(OUT_TAB, "Table3_wage_gap_exp.csv"),
          row.names = FALSE)
cat("✓ Table 3 done\n")


# ============================================================================ #
#  TABLE 4  —  Subsample Comparison: Male / Re-entrant / Female other         #
# ============================================================================ #

df_t4 <- df %>%
  mutate(group = case_when(
    re_entrant == 1               ~ "Re-entrant",
    female == 0                   ~ "Male",
    female == 1 & re_entrant == 0 ~ "Female other"
  ))

pval_t4 <- df_t4 %>%
  group_by(group) %>%
  summarise(
    p_value = ifelse(
      n_distinct(mismatch) == 2,
      t.test(ln_wage ~ mismatch, data = pick(everything()))$p.value,
      NA_real_
    ),
    .groups = "drop"
  )

table4_data <- df_t4 %>%
  group_by(group, mismatch) %>%
  summarise(
    N         = n(),
    mean_wage = mean(wage_monthly, na.rm = TRUE),
    sd_wage   = sd(wage_monthly,   na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(mismatch = ifelse(mismatch == 1, "Mismatched", "Matched")) %>%
  left_join(pval_t4, by = "group") %>%
  # Show p_value only on first row per group to avoid duplication
  group_by(group) %>%
  mutate(p_value = ifelse(row_number() == 1, p_value, NA_real_)) %>%
  ungroup() %>%
  arrange(group, mismatch)

table4 <- table4_data %>%
  gt(groupname_col = "group") %>%
  fmt_number(columns = c(mean_wage, sd_wage), decimals = 0) %>%
  fmt_number(columns = p_value, decimals = 3)               %>%
  cols_label(
    mismatch  = "Match Status",
    N         = "N",
    mean_wage = "Mean Wage (VND '000)",
    sd_wage   = "SD Wage",
    p_value   = "p-value"
  ) %>%
  tab_header(
    title    = "Table 4: Subsample Comparison",
    subtitle = "Male vs Re-entrant vs Female other — justification for subsamples"
  )

gtsave(table4, paste0(OUT_TAB, "Table4_subsample_comparison.html"))
cat("✓ Table 4 done\n")


# ============================================================================ #
#  FIGURE 1  —  Density of ln_wage: Matched vs Mismatched, by Gender          #
# ============================================================================ #

fig1 <- ggplot(df,
               aes(x     = ln_wage,
                   fill  = mismatch_label,
                   color = mismatch_label)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, alpha = 0.3, position = "identity") +
  geom_density(linewidth = 1, alpha = 0.15) +
  facet_wrap(~ gender_lab) +
  scale_fill_manual(values  = c("Matched" = "#1f77b4", "Mismatched" = "#d62728")) +
  scale_color_manual(values = c("Matched" = "#1f77b4", "Mismatched" = "#d62728")) +
  labs(
    title    = "Figure 1: Distribution of Log Wages",
    subtitle = "By mismatch status and gender",
    x        = "Log Monthly Wage",
    y        = "Density",
    fill     = "Mismatch Status",
    color    = "Mismatch Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    strip.text      = element_text(face = "bold")
  )

ggsave(paste0(OUT_FIG, "Fig1_density_lnwage.png"),
       plot = fig1, width = 9, height = 5, dpi = 300)
cat("✓ Figure 1 done\n")


# ============================================================================ #
#  FIGURE 2  —  Wage–Experience Profiles: 4 lines                             #
# ============================================================================ #

df_fig2 <- df %>%
  mutate(
    Group = case_when(
      female == 0 & mismatch == 0 ~ "Male: Matched",
      female == 0 & mismatch == 1 ~ "Male: Mismatched",
      female == 1 & mismatch == 0 ~ "Female: Matched",
      female == 1 & mismatch == 1 ~ "Female: Mismatched"
    ),
    Group = factor(Group, levels = c("Male: Matched",    "Male: Mismatched",
                                     "Female: Matched",  "Female: Mismatched"))
  )

fig2 <- ggplot(df_fig2, aes(x = exp, y = ln_wage,
                            color    = Group,
                            linetype = Group)) +
  geom_smooth(method = "loess", se = TRUE,
              linewidth = 1.2, alpha = 0.1) +
  scale_color_manual(values = c(
    "Male: Matched"      = "#2c3e50",
    "Male: Mismatched"   = "#7f8c8d",
    "Female: Matched"    = "#e74c3c",
    "Female: Mismatched" = "#c0392b"
  )) +
  scale_linetype_manual(values = c(
    "Male: Matched"      = "solid",
    "Male: Mismatched"   = "dashed",
    "Female: Matched"    = "solid",
    "Female: Mismatched" = "dashed"
  )) +
  labs(
    title    = "Figure 2: Wage–Experience Profiles",
    subtitle = "By gender and horizontal education–job mismatch",
    x        = "Years of Potential Experience",
    y        = "Log Monthly Wage",
    color    = "Labor Group",
    linetype = "Labor Group",
    caption  = "Source: LFS 2018. Smoothing via LOESS."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5),
    axis.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(paste0(OUT_FIG, "Fig2_wage_exp_profile.png"),
       plot = fig2, width = 9, height = 6, dpi = 300)
cat("✓ Figure 2 done\n")


# ============================================================================ #
#  FIGURE 3  —  Wage Gap ~ Experience, by Gender (LOESS)                      #
# ============================================================================ #

exp_cap <- quantile(df$exp, 0.99)   # data-driven cap, no hardcoding

gap_data <- df %>%
  filter(exp >= 0 & exp <= exp_cap) %>%
  mutate(exp_bin = floor(exp / 2) * 2) %>%
  group_by(gender_lab, exp_bin, mismatch) %>%
  summarise(mean_ln_wage = mean(ln_wage, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = mismatch,
    values_from = mean_ln_wage,
    names_glue  = "status_{mismatch}"
  ) %>%
  mutate(wage_gap = status_0 - status_1) %>%   # positive = matched earns more
  filter(!is.na(wage_gap))

fig3 <- ggplot(gap_data,
               aes(x = exp_bin, y = wage_gap,
                   color = gender_lab, fill = gender_lab)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "loess", se = TRUE,
              span = 0.75, linewidth = 1.2) +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  scale_color_manual(values = c("Female" = "#d95f02", "Male" = "#1b9e77")) +
  scale_fill_manual( values = c("Female" = "#d95f02", "Male" = "#1b9e77")) +
  labs(
    title    = "Figure 3: Wage Gap by Years of Experience",
    subtitle = "Matched minus mismatched log wage (2-year bins), by gender",
    x        = "Years of Experience",
    y        = "Wage Gap (log points)",
    color    = "Gender", fill = "Gender",
    caption  = "Note: Positive gap = matched workers earn more. LOESS span = 0.75.\nSource: LFS 2018."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 14),
    axis.title      = element_text(face = "bold")
  )

ggsave(paste0(OUT_FIG, "Fig3_wage_gap_trend.png"),
       plot = fig3, width = 10, height = 7, dpi = 300)
cat("✓ Figure 3 done\n")

cat("\n========== 02_eda.R completed ==========\n")
