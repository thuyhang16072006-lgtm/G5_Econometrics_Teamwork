# Gọi thư viện
library(tidyverse)
library(kableExtra)

# 1. Đọc dữ liệu
# Đảm bảo file LFS_2018.csv nằm trong thư mục làm việc (Working Directory)
df <- read.csv("LFS_2018.csv")

# 2. Xử lý dữ liệu và tính toán
table_data <- df %>%
  dplyr::group_by(female) %>%
  dplyr::summarise(
    wage_match = mean(wage[mismatch == 0], na.rm = TRUE),
    wage_mismatch = mean(wage[mismatch == 1], na.rm = TRUE),
    pct_mismatch = mean(mismatch == 1, na.rm = TRUE) * 100,
    # Thực hiện t-test so sánh lương giữa 2 nhóm mismatch
    p_val = t.test(wage ~ mismatch, data = dplyr::cur_data())$p.value
  ) %>%
  # Chuyển đổi mã số (0,1) sang chữ cho dễ đọc
  dplyr::mutate(Gender = ifelse(female == 1, "Female", "Male")) %>%
  dplyr::select(Gender, wage_match, wage_mismatch, pct_mismatch, p_val)

# 3. Xuất bảng ra file HTML
final_table <- table_data %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 3))) %>%
  dplyr::rename(
    `Avg Wage (Matched)` = wage_match,
    `Avg Wage (Mismatched)` = wage_mismatch,
    `Mismatch Rate (%)` = pct_mismatch,
    `P-value` = p_val
  ) %>%
  kableExtra::kbl(format = "html", caption = "Table 2: Wage Analysis by Gender and Mismatch") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "bordered"))

# Lưu file
kableExtra::save_kable(final_table, file = "table2_wages.html")

print("Xong! Bạn hãy kiểm tra file table2_wages.html trong thư mục của mình.")