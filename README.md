# R 函数库说明

本仓库包含几个自编实用 R 函数，用于数据可视化和分析。以下是每个函数的详细说明。

---

## 1. `create_boxplot.r`

### 功能
用于绘制箱线图（Boxplot），支持单变量和多变量的可视化。

### 主要函数
1. `create_boxplot_single`：绘制单个变量的箱线图。
2. `create_boxplot_multiple`：绘制多个变量的箱线图。

### 参数说明
- `data`：数据框，包含需要分析的变量。
- `y_var`：字符串，表示 y 轴变量的列名。
- `x_var`：字符串，表示 x 轴变量的列名。
- `comparisons`：列表或字符串，指定需要比较的组对。
- `help`：逻辑值，是否显示帮助信息。

### 返回值
返回一个 `ggplot` 对象，可以直接显示或进一步编辑。

### 示例
```r
# 绘制单个变量的箱线图
create_boxplot_single(data = my_data, y_var = "value", x_var = "group")

# 绘制多个变量的箱线图
create_boxplot_multiple(data = my_data, response = "value", vars = c("group1", "group2"))
```

---

## 2. `map_roc.R`

### 功能
用于批量计算 ROC 曲线及其相关指标（如 AUC、灵敏度、特异度等）。

### 主要函数
`map_roc`：批量计算 ROC 曲线并生成可视化图表。

### 参数说明
- `vars_to_calc_roc`：向量，包含需要计算 ROC 的变量名。
- `response`：字符串，表示响应变量的列名。
- `data_roc`：数据框，包含二分类变量（0 和 1）。
- `best_method`：字符串，指定计算灵敏度和特异度的方法（默认为 "c"）。
- `plot`：逻辑值，是否绘制 ROC 曲线图。

### 返回值
返回一个列表，包含以下内容：
- `roc_list`：所有 ROC 模型的结果。
- `auc_ci_se_sp`：包含 AUC、灵敏度和特异度的数据框。
- `roc_curve`：ROC 曲线图（如果 `plot = TRUE`）。

### 示例
```r
# 计算 ROC 曲线
result <- map_roc(
  vars_to_calc_roc = c("var1", "var2"),
  response = "outcome",
  data_roc = my_data
)

# 显示 ROC 曲线
print(result$roc_curve)
```

---

## 3. `risk_tile.R`

### 功能
用于生成风险热图，展示不同分层组合下的风险比例。

### 主要函数
`risk_tile`：生成风险热图。

### 参数说明
- `data_risk`：数据框，包含需要分析的风险数据。
- `reference`：字符串，指定参考变量的列名。
- `tiers`：向量，指定分层变量的列名。
- `tier_labels`：向量，可选参数，指定分层变量的标签。
- `title`：字符串，热图的标题。

### 返回值
返回一个列表，包含以下内容：
- `strategy`：分层策略的数据框。
- `risk_df`：风险数据框。
- `plot`：生成的热图对象。

### 示例
```r
# 生成风险热图
result <- risk_tile(
  data_risk = my_data,
  reference = "outcome",
  tiers = c("tier1", "tier2")
)

# 显示热图
print(result$plot)
```

---

## 安装与使用
1. 将上述文件保存到本地。
2. 在 R 中使用 `source` 函数加载文件：
   ```r
   source("create_boxplot.r")
   source("map_roc.R")
   source("risk_tile.R")
   ```
3. 按照示例调用函数。

## 依赖包
- `tidyverse`
- `ggplot2`
- `pROC`
- `ggpubr`
- `scales`
- `RColorBrewer`

请确保在使用前安装这些包。

---

## 作者
JH-work

## 最后更新
2025/10/15