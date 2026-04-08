library(ggplot2)
library(dplyr)

# 1. Đảm bảo đọc dữ liệu (Nếu Ánh làm file riêng)
df <- readRDS("data/processed/data_processed.rds")

# 2. Xử lý dữ liệu với nhãn Tiếng Anh chuyên nghiệp
plot_data <- df %>%
  mutate(Group = case_when(
    female == 0 & mismatch == 0 ~ "Male: Matched",
    female == 0 & mismatch == 1 ~ "Male: Mismatched",
    female == 1 & mismatch == 0 ~ "Female: Matched",
    female == 1 & mismatch == 1 ~ "Female: Mismatched"
  )) %>%
  # Sắp xếp thứ tự để Legend hiện ra theo ý muốn
  mutate(Group = factor(Group, levels = c("Male: Matched", "Male: Mismatched", 
                                          "Female: Matched", "Female: Mismatched")))

# 3. Vẽ biểu đồ "Professional Look"
p2 <- ggplot(plot_data, aes(x = exp, y = ln_wage, color = Group, linetype = Group)) +
  # Vẽ đường xu hướng (linewidth giúp tránh warning)
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.2, alpha = 0.1) + 
  
  # Palette màu chuyên nghiệp (Tone xanh cho Nam, tone đỏ/cam cho Nữ)
  scale_color_manual(values = c("#2c3e50", "#34495e", "#e74c3c", "#c0392b")) +
  # Matched là nét liền (solid), Mismatched là nét đứt (dashed) để cực kỳ dễ phân biệt
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed")) +
  
  # Nhãn tiếng Anh chuẩn bài báo quốc tế
  labs(
    title = "Figure 2: Wage-Experience Profiles",
    subtitle = "By Gender and Horizontal Education-Job Mismatch",
    x = "Years of Potential Experience",
    y = "Log of Hourly Wage",
    caption = "Source: Estimated from LFS 2018 using LOESS smoothing.",
    color = "Labor Group",
    linetype = "Labor Group"
  ) +
  
  # Giao diện sạch sẽ (Minimal)
  theme_minimal(base_family = "Arial") + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank() # Bỏ lưới phụ cho đỡ rối
  )

# 4. Xem biểu đồ trực tiếp trong RStudio
print(p2)

# 5. Lưu file chất lượng cao (300 DPI là chuẩn in ấn)
ggsave("fig2_wage_exp.png", plot = p2, width = 9, height = 6, dpi = 300)