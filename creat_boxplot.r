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
  comparisons = "all",
  theme = theme_bw(),
  color_theme = scale_fill_lancet(),
  x_lab = TRUE,
  y_lab = NULL,
  ncol = 3,
  figure_type = "boxplot",
  box_width = 0.9,
  angle = 30,
  help = FALSE
) {
  # 说明
  if (isTRUE(help)) {
    cat(
      "create_boxplot_multiple(data, response, vars, ncol, figure_type, help)\n"
    )
    cat("data: 数据框，包含response和vars列\n")
    cat("response: 字符串，表示响应变量的列名\n")
    cat("vars: 字符串向量，表示分类变量的列名\n")
    cat(
      "comparisons: 字符串，'separated' 表示排列组合所有的levels进行两两比较，或'all'表示进行多组比较，默认值为'all'\n"
    )
    cat("ncol: 整数，表示每行显示的图形数量，默认值为3\n")
    cat(
      "figure_type: 字符串，表示图形类型，可选'boxplot', 'violin','beeswarm'，默认值为'boxplot'\n"
    )
    cat("help: 逻辑值，如果为TRUE则显示帮助信息\n")
    return(NULL)
  }

  if (is.null(vars)) {
    vars <- data %>%
      select_if(is.factor) %>%
      colnames()
  }

  figure_list <- list()

  if (figure_type == "beeswarm") {
    library(ggbeeswarm)
    for (i in vars) {
      #  comparisons = combn(levels(data[[i]]), 2, FUN = c, simplify = FALSE)
      # 计算每个分组的样本数量和均值
      count_data_path <- data %>%
        filter(!is.na(!!sym(i))) %>%
        count(!!rlang::sym(i)) %>%
        mutate(label = paste0(!!rlang::sym(i), "\n(n=", n, ")"))

      # 计算每个组的均值
      mean_data <- data %>%
        filter(!is.na(!!sym(i))) %>%
        group_by(!!rlang::sym(i)) %>%
        summarise(mean_response = mean(!!sym(response)), .groups = "drop")

      # 获取标签数据
      method_labels <- count_data_path %>%
        select(!!rlang::sym(i), label) %>%
        distinct() %>%
        pull(label, !!rlang::sym(i))

      p <- data %>%
        filter(!is.na(!!sym(i))) %>%
        ggplot(aes(x = !!sym(i), y = !!sym(response), colour = !!sym(i))) +
        geom_beeswarm() +
        # 添加短横线表示均值 (x范围限制在组内)
        geom_segment(
          data = mean_data,
          aes(
            x = as.numeric(!!rlang::sym(i)) - 0.3, # 线段起始x
            xend = as.numeric(!!rlang::sym(i)) + 0.3, # 线段结束x
            y = mean_response, # 均值位置
            yend = mean_response,
            color = !!rlang::sym(i)
          ),
          size = 0.8
        ) +
        scale_x_discrete(labels = method_labels) +
        color_theme +
        theme +
        labs(
          # title = paste0(response,"在", i, "中的分布"),
          x = ifelse(
            isTRUE(x_lab),
            ifelse(
              is.null(attr(data[[i]], "label")),
              i,
              attr(data[[i]], "label")
            ),
            ""
          ),
          y = ifelse(
            is.null(y_lab),
            ifelse(
              is.null(attr(data[[response]], "label")),
              response,
              attr(data[[response]], "label")
            ),
            y_lab
          )
        ) +
        theme(legend.position = "none")

      if (comparisons == "all") {
        p <- p + stat_compare_means(label.x = 1.5)
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
          p <- p + stat_compare_means(label.x = 1.5)
        }
      }

      figure_list[[i]] <- p
    }
  } else if (figure_type == "boxplot") {
    library(ggplot2)
    library(ggpubr)

    for (i in vars) {
      # comparisons = combn(levels(data[[i]]), 2, FUN = c, simplify = FALSE)
      # 计算每个分组的样本数量和均值
      count_data_path <- data %>%
        filter(!is.na(!!sym(i))) %>%
        count(!!rlang::sym(i)) %>%
        mutate(label = paste0(!!rlang::sym(i), "\n(n=", n, ")"))

      # 计算每个组的均值
      mean_data <- data %>%
        filter(!is.na(!!sym(i))) %>%
        group_by(!!rlang::sym(i)) %>%
        summarise(mean_response = mean(!!sym(response)), .groups = "drop")

      # 获取标签数据
      method_labels <- count_data_path %>%
        select(!!rlang::sym(i), label) %>%
        distinct() %>%
        pull(label, !!rlang::sym(i))

      p <- data %>%
        filter(!is.na(!!sym(i))) %>%
        ggplot(aes(x = !!sym(i), y = !!sym(response), fill = !!sym(i))) +
        geom_boxplot(width = box_width) +
        scale_x_discrete(labels = method_labels) +
        color_theme +
        theme +
        labs(
          # title = paste0(response,"在", i, "中的分布"),
          x = ifelse(
            isTRUE(x_lab),
            ifelse(
              is.null(attr(data[[i]], "label")),
              i,
              attr(data[[i]], "label")
            ),
            ""
          ),
          y = ifelse(
            is.null(y_lab),
            ifelse(
              is.null(attr(data[[response]], "label")),
              response,
              attr(data[[response]], "label")
            ),
            y_lab
          )
        ) +
        theme(legend.position = "none")

      if (comparisons == "all") {
        p <- p + stat_compare_means(label.x = 1.5)
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
          p <- p + stat_compare_means(label.x = 1.5)
        }
      }

      figure_list[[i]] <- p
    }
  } else if (figure_type == "violin") {
    library(ggplot2)
    library(ggpubr)

    for (i in vars) {
      # comparisons = combn(levels(data[[i]]), 2, FUN = c, simplify = FALSE)
      # 计算每个分组的样本数量和均值
      count_data_path <- data %>%
        filter(!is.na(!!sym(i))) %>%
        count(!!rlang::sym(i)) %>%
        mutate(label = paste0(!!rlang::sym(i), "\n(n=", n, ")"))

      # 计算每个组的均值
      mean_data <- data %>%
        filter(!is.na(!!sym(i))) %>%
        group_by(!!rlang::sym(i)) %>%
        summarise(mean_response = mean(!!sym(response)), .groups = "drop")

      # 获取标签数据
      method_labels <- count_data_path %>%
        select(!!rlang::sym(i), label) %>%
        distinct() %>%
        pull(label, !!rlang::sym(i))

      p <- data %>%
        filter(!is.na(!!sym(i))) %>%
        ggplot(aes(x = !!sym(i), y = !!sym(response), fill = !!sym(i))) +
        geom_violin(draw_quantiles = 1) +
        scale_x_discrete(labels = method_labels) +
        color_theme +
        theme +
        labs(
          # title = paste0(response,"在", i, "中的分布"),
          x = ifelse(
            isTRUE(x_lab),
            ifelse(
              is.null(attr(data[[i]], "label")),
              i,
              attr(data[[i]], "label")
            ),
            ""
          ),
          y = ifelse(
            is.null(y_lab),
            ifelse(
              is.null(attr(data[[response]], "label")),
              response,
              attr(data[[response]], "label")
            ),
            y_lab
          )
        ) +
        theme(legend.position = "none")

      if (comparisons == "all") {
        p <- p + stat_compare_means(label.x = 1.5)
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
          p <- p + stat_compare_means(label.x = 1.5)
        }
      }

      figure_list[[i]] <- p
    }
  }

  patchwork::wrap_plots(
    figure_list,
    ncol = ncol
  ) &
    theme(axis.text.x = element_text(angle = angle, hjust = 1))
}
