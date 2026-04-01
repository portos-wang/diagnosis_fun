# 安装包（首次使用时执行）
# install.packages(c("ggplot2", "ggExtra", "ggpubr", "dplyr", "tidyr", "ggridges", "patchwork", "gghalves"))

# 1. 加载包
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(dplyr)
library(tidyr)
library(ggridges)
library(patchwork)
library(gghalves)

# 3.1 规范 p 值显示
# 将 p 值按 “<0.001/<0.01/<0.05 / 保留 3 位小数” 格式输出：
format_p_value <- function(p) {
  if (p < 0.001) {
    return("< 0.001")
  } else if (p < 0.01) {
    return("< 0.01")
  } else if (p < 0.05) {
    return("< 0.05")
  } else {
    return(paste0("= ", format(round(p, 3), nsmall = 3)))
  }
}

# 3.2 统一的密度图范围

get_density_range <- function(data, variable) {
  densities <- data %>%
    group_by(Group) %>%
    group_map(~ density(.x[[variable]], na.rm = TRUE)$y) %>%
    unlist()
  return(range(densities, na.rm = TRUE))
}

# 3.3 统一的直方图范围

calculate_unified_count_range <- function(
  data,
  variable_x,
  variable_y,
  bins = 20
) {
  x_counts <- data %>%
    group_by(Group) %>%
    group_map(
      ~ hist(
        .x[[variable_x]],
        breaks = seq(
          min(data[[variable_x]]),
          max(data[[variable_x]]),
          length.out = bins + 1
        ),
        plot = FALSE
      )$counts
    ) %>%
    unlist()
  y_counts <- data %>%
    group_by(Group) %>%
    group_map(
      ~ hist(
        .x[[variable_y]],
        breaks = seq(
          min(data[[variable_y]]),
          max(data[[variable_y]]),
          length.out = bins + 1
        ),
        plot = FALSE
      )$counts
    ) %>%
    unlist()
  max_count <- max(c(x_counts, y_counts), na.rm = TRUE) * 1.1
  return(c(0, max_count))
}

# 4. 对每个分组计算 X 与 Y 的相关系数（R）和显著性（p 值），并格式化为标注文本：
# 计算相关系数
cor_data <- long_data %>%
  group_by(Group) %>%
  summarize(
    r_value = round(cor(X, Y), 3),
    p_value_raw = cor.test(X, Y)$p.value,
    p_value_formatted = format_p_value(p_value_raw)
  ) %>%
  mutate(
    legend_label = Group,
    stat_label = paste0(Group, ": R = ", r_value, ", p ", p_value_formatted) # 最终标注文本
  )
# 将统计结果合并到主数据
long_data <- long_data %>%
  left_join(cor_data, by = "Group")


# 5.1 计算扩展坐标范围

# 原始数据范围
x_range <- range(long_data$X, na.rm = TRUE)
y_range <- range(long_data$Y, na.rm = TRUE)
# 扩展范围函数（增加10%空白）
expand_range <- function(r) {
  range_diff <- diff(r)
  return(c(r[1] - 0.1 * range_diff, r[2] + 0.1 * range_diff))
}
# 最终使用的扩展范围
expanded_x_range <- expand_range(x_range)
expanded_y_range <- expand_range(y_range)

# 5.2 设置相关系数标注位置
# 将标注固定在图形左上角：

x_pos <- expanded_x_range[1] + 0.01 * diff(expanded_x_range) # X轴2%位置
n_groups <- nrow(cor_data) # 分组数量
y_spacing <- 0.08 * diff(expanded_y_range) # 分组标注垂直间距
# 按分组顺序分配Y轴位置（从上到下）
cor_data <- cor_data %>%
  arrange(Group) %>%
  mutate(
    y_pos = expanded_y_range[2] -
      (0:(n_groups - 1)) * y_spacing -
      0.02 * diff(expanded_y_range)
  )

# 5.3 定义主题样式

# 两套主题分别用于主散点图和边缘统计图：

# 1. 主散点图主题（带坐标轴、边框、加粗文本）
Main_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # 标题居中加粗
    axis.title = element_text(face = "bold", size = 14), # 轴标签加粗
    axis.text = element_text(size = 12, face = "bold", color = "black"), # 轴刻度加粗
    axis.ticks = element_line(color = "black", linewidth = 0.5), # 轴刻度线
    axis.ticks.length = unit(0.2, "cm"), # 刻度线长度
    legend.position = "none", # 隐藏图例（标注已包含分组）
    panel.grid.major = element_blank(), # 隐藏主网格线
    panel.grid.minor = element_blank(), # 隐藏次网格线
    plot.background = element_rect(fill = "white", color = NA), # 白色背景
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1) # 图形边框
  )
# 2. 边缘图主题（无坐标轴、无网格，减小边距）
no_axis_theme <- theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0, 0, 0, 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
# 3. 配色（3组默认，可按分组数量调整）
sci_colors <- c('#96CCEA', '#B2A3DD', '#ED949A')[
  1:length(unique(long_data$Group))
]
# 轴标签（替换为你的变量名称）
x_label <- "X value（如：浓度）"
y_label <- "Y value（如：吸光度）"

# 6.1 绘制主散点图

# 包含分组散点、线性拟合线、相关系数标注，是图形的核心：

combined_plot <- ggplot(
  long_data,
  aes(x = X, y = Y, color = legend_label, fill = legend_label)
) +
  geom_point(size = 3, alpha = 0.8, shape = 21, color = "black", stroke = 0.6) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    alpha = 0.2,
    linewidth = 1.0,
    linetype = "dashed"
  ) +
  labs(x = x_label, y = y_label) +
  Main_theme +
  scale_color_manual(values = sci_colors) +
  scale_fill_manual(values = sci_colors) +
  geom_text(
    data = cor_data,
    aes(x = x_pos, y = y_pos, label = stat_label, color = Group),
    hjust = 0,
    size = 4.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  ylim(expanded_y_range) +
  xlim(expanded_x_range) +
  theme(plot.margin = margin(0, 0, 0, 0))

# 6.2 绘制 6 种边缘统计图

# 示例：密度图边缘图完整代码

# 顶部X轴密度图
x_density <- ggplot(
  long_data,
  aes(x = X, fill = legend_label, color = legend_label)
) +
  geom_density(alpha = 0.5, linewidth = 0.8) +
  no_axis_theme +
  scale_fill_manual(values = sci_colors) +
  scale_color_manual(values = sci_colors) +
  xlim(expanded_x_range) +
  ylim(c(
    0,
    max(
      get_density_range(long_data, "X")[2],
      get_density_range(long_data, "Y")[2]
    ) *
      1.1
  )) +
  theme(plot.margin = margin(0, 0, -5, 0))
# 右侧Y轴密度图（加coord_flip()翻转）
y_density <- ggplot(
  long_data,
  aes(x = Y, fill = legend_label, color = legend_label)
) +
  geom_density(alpha = 0.5, linewidth = 0.8) +
  no_axis_theme +
  scale_fill_manual(values = sci_colors) +
  scale_color_manual(values = sci_colors) +
  xlim(expanded_y_range) +
  ylim(c(
    0,
    max(
      get_density_range(long_data, "X")[2],
      get_density_range(long_data, "Y")[2]
    ) *
      1.1
  )) +
  coord_flip() +
  theme(plot.margin = margin(0, 0, 0, -5))

# 7.1 定义组合函数

create_final_plot <- function(top_plot, right_plot) {
  layout <- top_plot +
    plot_spacer() +
    combined_plot +
    right_plot +
    plot_layout(
      ncol = 2,
      nrow = 2,
      widths = c(4, 1), # 主图:右侧图 = 4:1
      heights = c(1, 4) # 顶部图:主图 = 1:4
    )
  wrap_elements(full = layout) +
    theme(plot.margin = margin(5, 5, 5, 5))
}

# 7.2 生成最终图形并保存
# 密度图版本
final_plot_density <- create_final_plot(x_density, y_density)
ggsave(
  "散点图_边缘密度图.png",
  final_plot_density,
  width = 15,
  height = 15,
  units = "cm",
  dpi = 600,
  bg = "white"
)
