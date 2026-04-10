# ============================================================
# 02_eda.R — Exploratory Data Analysis & Descriptive Stats
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# Input:  data/processed/data_processed.rds
# Output: output/tables/Table_1_*.html
#         output/figures/Fig1_density.png
#         output/figures/Fig2_exp_wage.png
#         output/figures/Fig3_boxplot_exp.png
#         data/processed/eda_results.rds
# ============================================================
# ============================================================
# 02_eda.R — Exploratory Data Analysis & Descriptive Stats
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# ============================================================

# ============================================================
# 02_eda.R — Tạo bảng 2x2 chuẩn (Mismatch x Female)
# ============================================================

# ============================================================
# 02_eda.R — Cập nhật: Thống kê đầy đủ Mean, SD, N, % Group
# ============================================================

# ============================================================
# 02_eda.R — Updated: Descriptive Statistics (English Version)
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# ============================================================

# ============================================================
# 02_eda.R — Descriptive Statistics (English Version)
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# ============================================================

library(tidyverse)
library(kableExtra)

# 1. Load processed data
df <- readRDS("data/processed/data_processed.rds")
total_n <- nrow(df)

# 2. Calculate cell-specific statistics (mismatch x female)
stats_raw <- df |>
  group_by(female, mismatch) |>
  summarise(
    n_cell = n(),
    mean_w = mean(exp(ln_wage), na.rm = TRUE),
    sd_w   = sd(exp(ln_wage), na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(
    pct_sample = (n_cell / total_n) * 100, 
    # Mean and SD in "Mean (SD)" format, rounded to 2 decimal places for readability
    mean_sd = paste0(round(mean_w, 2), " (", round(sd_w, 2), ")")
  )

# 3. Calculate P-value and within-gender Mismatch Rate
test_results <- df |>
  group_by(female) |>
  summarise(
    p_val = t.test(ln_wage ~ mismatch, data = cur_data())$p.value,
    pct_mismatch_gender = mean(mismatch == 1, na.rm = TRUE) * 100
  )

# 4. Pivot to 2x2 structure
table_final <- stats_raw |>
  mutate(
    gender_lab = ifelse(female == 1, "Female", "Male"),
    match_lab  = ifelse(mismatch == 1, "Mismatched", "Matched")
  ) |>
  select(gender_lab, match_lab, mean_sd, n_cell, pct_sample) |>
  pivot_wider(
    names_from = match_lab, 
    values_from = c(mean_sd, n_cell, pct_sample)
  ) |>
  bind_cols(test_results |> select(pct_mismatch_gender, p_val))

# 5. Export professional HTML table with 3-decimal rounding
table_final |>
  mutate(across(where(is.numeric), ~ round(., 3))) |>
  select(
    Gender = gender_lab,
    `Wage: Mean (SD)` = mean_sd_Matched, `N` = n_cell_Matched, `% of Sample` = pct_sample_Matched,
    `Wage: Mean (SD) ` = mean_sd_Mismatched, `N ` = n_cell_Mismatched, `% of Sample ` = pct_sample_Mismatched,
    `Mismatch Rate (%)` = pct_mismatch_gender,
    `P-value` = p_val
  ) |>
  kbl(format = "html", caption = "Table 2: Descriptive Statistics of Wages, Sample Size, and Group Shares") |>
  kable_styling(bootstrap_options = c("striped", "bordered", "condensed"), font_size = 13) |>
  add_header_above(c(" " = 1, "MATCHED GROUP" = 3, "MISMATCHED GROUP" = 3, "STATISTICAL TEST" = 2)) |>
  footnote(general = "Wage units: Million VND. Standard Deviation (SD) in parentheses. All numeric values are rounded to 3 decimal places.") |>
  save_kable(file = "output/tables/table2_wages.html")

cat("Table with p-value rounded to 3 decimal places saved at: output/tables/table2_wages.html\n")

# ============================================================
# 02_eda.R — Visualization: Wage Gap Trend (LOESS)
# ============================================================

library(tidyverse)
library(ggplot2)

# 1. Load data
df <- readRDS("data/processed/data_processed.rds")

# 2. Xử lý dữ liệu: Tạo bin 2 năm và tính Wage Gap
# Wage Gap = Mean ln_wage (Matched) - Mean ln_wage (Mismatched)
gap_data <- df |>
  filter(exp >= 0 & exp <= 35) |> 
  mutate(
    # Tạo bin 2 năm: 0-1 -> 0, 2-3 -> 2, ...
    exp_bin = floor(exp / 2) * 2,
    gender_lab = ifelse(female == 1, "Female", "Male")
  ) |>
  group_by(gender_lab, exp_bin, mismatch) |>
  summarise(mean_ln_wage = mean(ln_wage, na.rm = TRUE), .groups = "drop") |>
  # Xoay ngang để tính hiệu số giữa Matched (0) và Mismatched (1)
  pivot_wider(names_from = mismatch, values_from = mean_ln_wage, names_prefix = "status_") |>
  mutate(
    # Wage Gap tính bằng log points
    wage_gap = status_0 - status_1 
  ) |>
  filter(!is.na(wage_gap)) # Loại bỏ các bin không đủ dữ liệu cả 2 nhóm

# 3. Vẽ biểu đồ Scatter + LOESS Smoothing
fig4_wage_gap <- ggplot(gap_data, aes(x = exp_bin, y = wage_gap, color = gender_lab, fill = gender_lab)) +
  # Vẽ các điểm gap thô (scatter)
  geom_point(alpha = 0.4, size = 2) +
  # Vẽ đường mượt LOESS với dải tin cậy (confidence band)
  geom_smooth(method = "loess", se = TRUE, span = 0.75, size = 1.2) +
  # Đường nằm ngang y = 0 làm mốc tham chiếu
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  # Tùy chỉnh trục và nhãn
  scale_x_continuous(limits = c(0, 35), breaks = seq(0, 35, 5)) +
  scale_color_manual(values = c("Female" = "#d95f02", "Male" = "#1b9e77")) +
  scale_fill_manual(values = c("Female" = "#d95f02", "Male" = "#1b9e77")) +
  labs(
    title = "Wage Gap Trend: Matched vs. Mismatched Workers",
    subtitle = "Gross wage gap (log points) by years of experience and gender",
    x = "Years of Experience (2-year bins)",
    y = "Wage Gap (Log Points)",
    color = "Gender",
    fill = "Gender",
    caption = "Note: Positive gap indicates Matched workers earn more. Smoothing via LOESS (span = 0.75)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

# 4. Lưu file PNG
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
ggsave("output/figures/Fig4_wage_gap_trend.png", plot = fig4_wage_gap, width = 10, height = 7, dpi = 300)

cat("Biểu đồ xu hướng Wage Gap đã được lưu tại: output/figures/Fig4_wage_gap_trend.png\n")