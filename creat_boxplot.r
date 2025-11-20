# 画出单独的 boxplot
require(tidyverse)
require(ggpubr)
require(ggsci)

create_boxplot_single <- function(
  data,
  y_var,
  x_var,
  theme = theme_bw(),
  color_theme = scale_fill_lancet(),
  x_lab = NULL,
  y_lab = NULL,
  comparisons = "all",
  help = FALSE
) {
  # 导入包
  require(tidyverse)
  require(ggpubr)
  require(ggsci)

  # 说明
  if (isTRUE(help)) {
    cat("create_boxplot(data, y_var, x_var, comparisons, help)\n")
    cat("data: 数据框，包含y_var和x_var列\n")
    cat("y_var: 字符串，表示y轴变量的列名\n")
    cat("x_var: 字符串，表示x轴变量的列名\n")
    cat(
      "comparisons: 列表，包含要比较的组对，例如list(c('A', 'B'), c('A', 'C'))\n"
    )
    cat("help: 逻辑值，如果为TRUE则显示帮助信息\n")
    return(NULL)
  }

  # if (is.null(comparisons)) {
  #   comparisons <- combn(levels(data[[x_var]]), 2, FUN = c, simplify = FALSE)
  # }

  # 计算每个分组的样本数量（考虑method分面因素）
  count_data_path <- data %>%
    filter(!is.na(x_var)) %>%
    count(!!rlang::sym(x_var)) %>% # 使用转换后的符号
    mutate(label = paste0(!!rlang::sym(x_var), "\n(n=", n, ")"))

  # 获取标签数据
  method_labels <- count_data_path %>%
    select(!!rlang::sym(x_var), label) %>%
    distinct() %>%
    pull(label, !!rlang::sym(x_var))

  p <- data %>%
    # filter(!is.na(Pathology)) %>%
    ggplot(aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]])) +
    geom_boxplot() +
    theme +
    color_theme +
    theme(legend.position = "") +
    labs(
      x = ifelse(
        is.null(x_lab),
        ifelse(
          is.null(attr(data[[x_var]], "label")),
          x_var,
          attr(data[[x_var]], "label")
        ),
        x_lab
      ),
      y = ifelse(
        is.null(y_lab),
        ifelse(
          is.null(attr(data[[y_var]], "label")),
          x_var,
          attr(data[[y_var]], "label")
        ),
        y_lab
      )
    ) +
    scale_x_discrete(labels = method_labels)

  if (comparisons == "all") {
    p <- p + stat_compare_means()
  } else if (comparisons == "separated") {
    # comparisons = combn(levels(data[[vars[1]]]), 2, FUN = c, simplify = FALSE)
    sig_comparisons <- compare_means(
      data = data,
      formula = as.formula(paste(response, "~", i))
    ) %>%
      filter(p < 0.05) %>%
      select(group1, group2) %>%
      purrr::pmap(c)
    if (length(sig_comparisons) > 0) {
      p <- p +
        stat_compare_means(
          comparisons = sig_comparisons,
          method = "wilcox",
          label = "p.signif"
        )
    } else {
      p <- p + stat_compare_means()
    }
  }

  print(p)
}


## --------------------------------------------------------------------------------------------------------------##
# 画出单个指标在多个分类标准上的分布

create_boxplot_multiple <- function(
  data,
  response,
  vars = NULL,
  comparisons = "all", # "separated" or "all"
  theme = theme_bw(),
  color_theme = scale_fill_lancet(),
  x_lab = TRUE,
  y_lab = NULL,
  ncol = 3,
  figure_type = "boxplot", # 'boxplot', 'violin', 'beeswarm'
  box_width = 0.9,
  angle = 30,
  help = FALSE
) {
  # --- 0. 加载依赖包 ---
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
  library(patchwork)
  library(ggbeeswarm)
  library(ggsci)
  library(rlang)

  # --- 1. 帮助信息 ---
  if (isTRUE(help)) {
    message(
      "create_boxplot_multiple(data, response, vars, ncol, figure_type, help)"
    )
    message("  data: 数据框")
    message("  response: 响应变量列名 (Y轴)")
    message("  vars: 分类变量列名向量 (X轴)")
    message("  figure_type: 'boxplot'(默认), 'violin', 'beeswarm'")
    message(
      "  comparisons: 'all'(总体检验) 或 'separated'(仅显示显著的两两比较)"
    )
    return(invisible(NULL))
  }

  # --- 2. 变量检查与初始化 ---
  # 检查依赖包是否已加载 (可选，这里假设用户已加载或环境已配置)

  if (is.null(vars)) {
    vars <- data %>% select(where(is.factor)) %>% colnames()
  }

  # 获取Y轴标签 (避免在循环中重复逻辑)
  y_label_text <- if (is.null(y_lab)) {
    attr(data[[response]], "label") %||% response
  } else {
    y_lab
  }

  figure_list <- list()

  # --- 3. 循环绘图 (核心逻辑合并) ---
  for (i in vars) {
    # 3.1 数据准备：一次性过滤NA，避免后续重复filter
    # 使用 .data[[var]] 语法替代 !!sym(var)，更符合tidy eval推荐
    plot_data <- data %>%
      filter(!is.na(.data[[i]]), !is.na(.data[[response]]))

    if (nrow(plot_data) == 0) {
      warning(paste("变量", i, "在去除NA后无数据，跳过。"))
      next
    }

    # 3.2 生成带有样本量(n)的标签
    # 计算 counts 并 merge 回去，或者直接在绘图时使用命名向量替换 label
    count_stats <- plot_data %>%
      count(.data[[i]]) %>%
      mutate(new_label = paste0(.data[[i]], "\n(n=", n, ")"))

    # 创建命名向量用于 scale_x_discrete (labels = ...)
    label_map <- setNames(count_stats$new_label, count_stats[[i]])

    # 获取X轴标签
    x_label_text <- if (isTRUE(x_lab)) {
      attr(data[[i]], "label") %||% i
    } else {
      ""
    }

    # 3.3 初始化 ggplot 对象
    # 注意：Beeswarm通常用 color，Boxplot/Violin通常用 fill，这里做统一处理
    aes_mapping <- aes(x = .data[[i]], y = .data[[response]])

    if (figure_type == "beeswarm") {
      aes_mapping <- utils::modifyList(aes_mapping, aes(color = .data[[i]]))
    } else {
      aes_mapping <- utils::modifyList(aes_mapping, aes(fill = .data[[i]]))
    }

    p <- ggplot(plot_data, aes_mapping)

    # 3.4 根据类型添加几何对象
    if (figure_type == "beeswarm") {
      p <- p + geom_beeswarm()

      # Beeswarm 特有的均值线逻辑
      # 使用 stat_summary 替代手动计算 mean_data 和 geom_segment，代码更简洁
      p <- p +
        stat_summary(
          fun = mean,
          geom = "crossbar",
          width = 0.6,
          linewidth = 0.4,
          aes(color = .data[[i]]) # 保持颜色一致
        )
    } else if (figure_type == "violin") {
      p <- p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE)
    } else {
      # 默认为 boxplot
      p <- p + geom_boxplot(width = box_width)
    }

    # 3.5 统一设置主题、标签和颜色
    p <- p +
      scale_x_discrete(labels = label_map) +
      labs(x = x_label_text, y = y_label_text) +
      theme +
      theme(legend.position = "none")

    # 应用颜色 (判断是 fill 还是 color)
    if (figure_type == "beeswarm") {
      # 如果 color_theme 是 scale_fill_xxx，通常对应 scale_color_xxx
      # 这里假设用户传入的是 scale_fill，我们需要尽量兼容或使用 scale_color
      # 简单的处理是：如果是 beeswarm，尝试寻找对应的 color scale，或者用户需要传入正确的 scale
      # 为了兼容原代码逻辑，这里简单处理，beeswarm 原代码用的 color_theme 也是直接加的
      # 如果 color_theme 是 scale_fill_lancet()，加到 color aesthetic 上可能无效
      # 这里做一个简单的转换尝试 (仅针对 ggsci/ggplot2 标准对象)，或者假定用户传入了正确的
      tryCatch(
        {
          if (
            inherits(color_theme, "ScaleDiscrete") &&
              "fill" %in% color_theme$aesthetics
          ) {
            # 尝试将 fill scale 转换为 color scale (这是一个hack，如果不严谨可以删掉)
            # 更好的方式是建议用户传入 scale_color_lancet()
            # 这里保留原逻辑：直接相加
            p <- p + color_theme
          } else {
            p <- p + color_theme
          }
        },
        error = function(e) p <- p + color_theme
      )
    } else {
      p <- p + color_theme
    }

    # 3.6 统计检验逻辑
    if (comparisons == "all") {
      # 总体比较 (Anova / Kruskal)
      p <- p + stat_compare_means(label.x = 1.5)
    } else if (comparisons == "separated") {
      # 计算显著的两两比较
      # 使用 tryCatch 防止计算 p 值出错中断循环
      sig_comparisons <- tryCatch(
        {
          compare_means(
            as.formula(paste(response, "~", i)),
            data = plot_data
          ) %>%
            filter(p < 0.05) %>%
            select(group1, group2)
        },
        error = function(e) NULL
      )

      if (!is.null(sig_comparisons) && nrow(sig_comparisons) > 0) {
        comp_list <- as.list(as.data.frame(
          t(sig_comparisons),
          stringsAsFactors = FALSE
        ))
        p <- p +
          stat_compare_means(
            comparisons = comp_list,
            method = "wilcox",
            label = "p.signif"
          )
      } else {
        # 如果没有显著差异，回退到显示总体 p 值
        p <- p + stat_compare_means(label.x = 1.5)
      }
    }

    figure_list[[i]] <- p
  }

  # --- 4. 拼图 ---
  patched_plots <- wrap_plots(figure_list, ncol = ncol) &
    theme(axis.text.x = element_text(angle = angle, hjust = 1))

  return(list("figure_list" = figure_list, "patched_plots" = patched_plots))
}
