#' 风险热图绘制函数
#'
#' 该函数用于根据输入的数据和分层变量生成风险热图，展示不同分层组合下的风险比例。
#'
#' @param data_risk 数据框，包含需要分析的风险数据。
#' @param reference 字符向量，指定参考变量列名。
#' @param tiers 字符向量，指定分层变量列名。
#' @param tier_labels 字符向量，可选参数，指定分层变量的标签。如果未提供，则默认使用 `tiers` 的值。
#' @param title 字符串，可选参数，指定热图的标题，默认为 "Risk plot"。
#' @param show_n 逻辑值，是否在每个方块中显示样本量，默认为 TRUE。
#' @param show_ci 逻辑值，是否在每个方块中显示 95% 置信区间，默认为 TRUE。
#' @param ci_newline 逻辑值，TRUE 时 CI 显示在百分比下一行，FALSE 时 CI 显示在百分比同行之后，默认为 TRUE。
#' @param digits 整数，控制百分比数值的小数点位数，默认为 2。
#'
#' @return 返回一个列表，包含以下内容：
#' \itemize{
#'   \item \code{strategy}: 分层策略的数据框，包含比例、样本量和置信区间。
#'   \item \code{risk_df}: 风险数据框，包含计算后的风险比例。
#'   \item \code{plot}: 生成的热图对象。
#' }
#'
#' @details
#' 该函数支持1到3个分层变量的分析，并根据分层变量的组合生成热图。热图使用颜色渐变表示风险比例，红色表示高风险，蓝色表示低风险。
#' 95% 置信区间使用 Wilson score 方法计算，适用于小样本和极端比例的情况。
#'
#' @examples
#' \dontrun{
#' # 示例用法
#' result <- risk_tile(
#'   data_risk = my_data,
#'   reference = "outcome",
#'   tiers = c("tier1", "tier2"),
#'   tier_labels = c("Tier 1", "Tier 2"),
#'   title = "My Risk Plot",
#'   show_n = TRUE,
#'   show_ci = TRUE
#' )
#' print(result$plot)
#' }
#'
#' @importFrom tidyverse %>%
#' @importFrom ggplot2 ggplot geom_tile scale_fill_distiller theme_minimal labs theme
#' @importFrom scales percent
#' @importFrom RColorBrewer brewer.pal
#' @export

risk_tile = function(
  data_risk = NULL,
  reference = NULL,
  tiers = NULL,
  tier_labels = NULL,
  title = "Risk plot",
  show_n = TRUE,
  show_ci = TRUE,
  ci_newline = FALSE,
  digits = 2
) {
  require(tidyverse)

  # 0. 条件检查
  if (is.null(data_risk) | is.null(reference) | is.null(tiers)) {
    stop("Please provide data_risk, reference, tiers")
  }
  if (!is.data.frame(data_risk) | !is.character(tiers)) {
    stop("data_risk must be a data frame and tiers must be a character vector")
  }

  if (is.null(tier_labels)) {
    tier_labels = tiers # 默认给tier_labels赋值为tiers
  }

  # 文本大小：启用 n 或 CI 时使用较小字号以适应多行标签
  text_size <- if (show_n || show_ci) 3 else 4

  # 1. Wilson 95% 置信区间计算函数
  wilson_ci <- function(p, n, z = 1.96) {
    if (is.na(n) || n == 0) return(c(NA_real_, NA_real_))
    denom <- 1 + z^2 / n
    center <- (p + z^2 / (2 * n)) / denom
    margin <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
    c(lower = center - margin, upper = center + margin)
  }

  # 2. 标签格式化函数：根据 show_n 和 show_ci 生成标签文本（向量化）
  make_label <- function(p, n = NULL, ci_lower = NULL, ci_upper = NULL) {
    mapply(function(p_i, n_i, ci_l, ci_u) {
      pct <- scales::percent(round(p_i, digits + 2), accuracy = 10^-digits)
      label <- pct
      if (show_ci && !is.null(ci_l) && !is.na(ci_l)) {
        ci_txt <- paste0("[", scales::percent(round(ci_l, digits + 2), accuracy = 10^-digits),
                         " - ", scales::percent(round(ci_u, digits + 2), accuracy = 10^-digits), "]")
        sep <- if (ci_newline) "\n" else " "
        label <- paste0(label, sep, ci_txt)
      }
      if (show_n && !is.null(n_i) && !is.na(n_i)) {
        label <- paste0(label, "\nn = ", n_i)
      }
      label
    }, p, n, ci_lower, ci_upper, USE.NAMES = FALSE)
  }

  # 3. 设置颜色匹配函数
  color_mapper <- function(x) {
    require(scales)
    require(RColorBrewer)

    # x: 0~1 之间的数值（可向量）
    if (any(x < 0 | x > 1, na.rm = TRUE)) {
      stop("输入的数值必须在 0 到 1 之间")
    }

    if (
      !requireNamespace("RColorBrewer", quietly = TRUE) ||
        !requireNamespace("scales", quietly = TRUE)
    ) {
      stop("请先安装 RColorBrewer 和 scales 包")
    }

    cols <- RColorBrewer::brewer.pal(11, "RdYlBu")
    cols <- rev(cols)

    pal <- scales::gradient_n_pal(
      cols,
      values = scales::rescale(seq(0, 1, length.out = 11))
    )

    pal(x)
  }

  # 4. 辅助函数：计算频率表并附加样本量
  compute_freq_table <- function(data, ref, var_cols) {
    filtered <- data
    for (v in var_cols) {
      filtered <- filtered %>% filter(!!sym(v$name) == v$value)
    }
    tab <- filtered %>%
      select(!!sym(ref)) %>%
      mutate(!!sym(ref) := factor(!!sym(ref), levels = c(0, 1))) %>%
      table()

    n_val <- sum(tab)

    df <- tab %>%
      prop.table() %>%
      as.data.frame()

    for (v in var_cols) {
      df <- df %>% mutate(!!sym(v$name) := v$value)
    }
    df <- df %>% mutate(.n = n_val)

    df
  }

  # 5. 辅助函数：为 strategy 添加 CI 列
  add_ci <- function(strategy_df) {
    if (nrow(strategy_df) == 0) return(strategy_df)
    strategy_df$.ci_lower <- sapply(
      seq_len(nrow(strategy_df)),
      function(j) wilson_ci(strategy_df$Freq[j], strategy_df$.n[j])[1]
    )
    strategy_df$.ci_upper <- sapply(
      seq_len(nrow(strategy_df)),
      function(j) wilson_ci(strategy_df$Freq[j], strategy_df$.n[j])[2]
    )
    strategy_df
  }

  # 检查 tiers 是否为多个值的情况
  if (length(tiers) == 1) {
    strategy1 <- data.frame()

    for (i in c(0, 1)) {
      if (!tiers[1] %in% names(data_risk)) {
        stop(paste("Column", tiers[1], "not found in data_risk"))
      }
      df <- compute_freq_table(data_risk, reference, list(list(name = tiers[1], value = i)))
      strategy1 <- bind_rows(strategy1, df)
    }

    strategy1 = strategy1 %>% filter(!!sym(reference) == 1)
    strategy1 <- add_ci(strategy1)

    risk_df1 = strategy1 %>%
      mutate(
        "One Tier Risk" = Freq,
        Combo = if_else(
          !!sym(tiers[1]) == 0,
          paste(tier_labels[1], "(-)"),
          paste(tier_labels[1], "(+)")
        ),
        label = make_label(Freq, .n, .ci_lower, .ci_upper)
      )

    p1 = risk_df1 %>%
      pivot_longer(
        cols = c("One Tier Risk"),
        names_to = "Methods",
        values_to = "Values"
      ) %>%
      ggplot(aes(x = Methods, y = Combo, fill = Values)) +
      geom_tile(color = "white") +
      scale_fill_distiller(
        palette = "RdYlBu",
        direction = -1,
        name = "Positive %",
        limits = c(0, 1)
      ) +
      geom_text(aes(label = label), size = text_size) +
      theme_minimal() +
      labs(
        title = title,
        x = "",
        y = ""
      ) +
      theme(axis.text.x = element_text(face = "bold", hjust = 0.8))

    return(
      risk_tile = list(strategy = strategy1, risk_df = risk_df1, plot = p1)
    )
  }

  if (length(tiers) == 2) {
    # Tier 1
    strategy1 <- data.frame()
    for (i in c(0, 1)) {
      if (!tiers[1] %in% names(data_risk)) {
        stop(paste("Column", tiers[1], "not found in data_risk"))
      }
      df <- compute_freq_table(data_risk, reference, list(list(name = tiers[1], value = i)))
      strategy1 <- bind_rows(strategy1, df)
    }
    strategy1 = strategy1 %>% filter(!!sym(reference) == 1)
    strategy1 <- add_ci(strategy1)

    # Tier 2
    strategy2 <- data.frame()
    for (i in c(0, 1)) {
      for (j in c(0, 1)) {
        if (!tiers[1] %in% names(data_risk)) {
          stop(paste("Column", tiers[1], "not found in data_risk"))
        } else if (!tiers[2] %in% names(data_risk)) {
          stop(paste("Column", tiers[2], "not found in data_risk"))
        }
        df <- compute_freq_table(data_risk, reference,
          list(list(name = tiers[1], value = i), list(name = tiers[2], value = j)))
        strategy2 <- bind_rows(strategy2, df)
      }
    }
    strategy2 = strategy2 %>% filter(!!sym(reference) == 1)
    strategy2 <- add_ci(strategy2)

    risk_df2 = strategy2 %>%
      mutate(
        "One Tier Risk" = if_else(
          !!sym(tiers[1]) == 0,
          round(strategy1$Freq[1]),
          round(strategy1$Freq[2])
        ),
        Combo = case_when(
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 0 ~
            paste0(tier_labels[1], "(-)", " \u2192 ", tier_labels[2], "(-)"),
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 1 ~
            paste0(tier_labels[1], "(-)", " \u2192 ", tier_labels[2], "(+)"),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 0 ~
            paste0(tier_labels[1], "(+)", " \u2192 ", tier_labels[2], "(-)"),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 1 ~
            paste0(tier_labels[1], "(+)", " \u2192 ", tier_labels[2], "(+)"),
        ),
        "Two Tiers Risk" = Freq,
        label_two = make_label(Freq, .n, .ci_lower, .ci_upper)
      ) %>%
      select(
        !!sym(tiers[1]),
        !!sym(tiers[2]),
        Combo,
        `One Tier Risk`,
        `Two Tiers Risk`,
        label_two
      )

    p2 = risk_df2 %>%
      pivot_longer(
        cols = c("One Tier Risk", "Two Tiers Risk"),
        names_to = "Methods",
        values_to = "Values"
      ) %>%
      mutate(
        display_label = if_else(
          Methods == "Two Tiers Risk",
          label_two,
          scales::percent(round(Values, 4))
        )
      ) %>%
      ggplot(aes(x = Methods, y = Combo, fill = Values)) +
      geom_tile(color = "white") +
      scale_fill_distiller(
        palette = "RdYlBu",
        direction = -1,
        name = "Positive %",
        limits = c(0, 1)
      ) +
      geom_text(aes(label = display_label), size = text_size) +
      theme_minimal() +
      labs(
        title = title,
        x = "",
        y = ""
      ) +
      theme(axis.text.x = element_text(face = "bold", hjust = 0.8)) +
      annotate(
        "rect",
        xmin = 1 - 0.5,
        xmax = 1 + 0.5,
        ymin = 4 - 3.5,
        ymax = 4 - 1.5,
        fill = color_mapper(strategy1$Freq[1]),
        color = "white"
      ) +
      annotate(
        "text",
        x = 1,
        y = 2 - 0.5,
        label = make_label(strategy1$Freq[1], strategy1$.n[1],
                           strategy1$.ci_lower[1], strategy1$.ci_upper[1]),
        size = text_size
      ) +
      annotate(
        "rect",
        xmin = 1 - 0.5,
        xmax = 1 + 0.5,
        ymin = 4 - 1.5,
        ymax = 4 + 0.5,
        fill = color_mapper(strategy1$Freq[2]),
        color = "white"
      ) +
      annotate(
        "text",
        x = 1,
        y = 4 - 0.5,
        label = make_label(strategy1$Freq[2], strategy1$.n[2],
                           strategy1$.ci_lower[2], strategy1$.ci_upper[2]),
        size = text_size
      )

    return(
      risk_tile = list(strategy = strategy2, risk_df = risk_df2, plot = p2)
    )
  }

  if (length(tiers) == 3) {
    # Tier 1
    strategy1 <- data.frame()
    for (i in c(0, 1)) {
      if (!tiers[1] %in% names(data_risk)) {
        stop(paste("Column", tiers[1], "not found in data_risk"))
      } else if (!tiers[2] %in% names(data_risk)) {
        stop(paste("Column", tiers[2], "not found in data_risk"))
      } else if (!tiers[3] %in% names(data_risk)) {
        stop(paste("Column", tiers[3], "not found in data_risk"))
      }
      df <- compute_freq_table(data_risk, reference, list(list(name = tiers[1], value = i)))
      strategy1 <- bind_rows(strategy1, df)
    }
    strategy1 = strategy1 %>% filter(!!sym(reference) == 1)
    strategy1 <- add_ci(strategy1)

    # Tier 2
    strategy2 <- data.frame()
    for (i in c(0, 1)) {
      for (j in c(0, 1)) {
        if (!tiers[1] %in% names(data_risk)) {
          stop(paste("Column", tiers[1], "not found in data_risk"))
        } else if (!tiers[2] %in% names(data_risk)) {
          stop(paste("Column", tiers[2], "not found in data_risk"))
        }
        df <- compute_freq_table(data_risk, reference,
          list(list(name = tiers[1], value = i), list(name = tiers[2], value = j)))
        strategy2 <- bind_rows(strategy2, df)
      }
    }
    strategy2 = strategy2 %>% filter(!!sym(reference) == 1)
    strategy2 <- add_ci(strategy2)

    # Tier 3
    strategy3 <- data.frame()
    for (i in c(0, 1)) {
      for (j in c(0, 1)) {
        for (k in c(0, 1)) {
          df <- compute_freq_table(data_risk, reference,
            list(list(name = tiers[1], value = i),
                 list(name = tiers[2], value = j),
                 list(name = tiers[3], value = k)))
          strategy3 <- bind_rows(strategy3, df)
        }
      }
    }
    strategy3 = strategy3 %>% filter(!!sym(reference) == 1)
    strategy3 <- add_ci(strategy3)

    risk_df3 = strategy3 %>%
      mutate(
        "One Tier Risk" = if_else(
          !!sym(tiers[1]) == 0,
          round(strategy1$Freq[1]),
          round(strategy1$Freq[2])
        ),
        "Two Tiers Risk" = case_when(
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 0 ~
            round(strategy2$Freq[1]),
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 1 ~
            round(strategy2$Freq[2]),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 0 ~
            round(strategy2$Freq[3]),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 1 ~
            round(strategy2$Freq[4]),
        ),
        Combo = case_when(
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 0 & !!sym(tiers[3]) == 0 ~
            paste0(tier_labels[1], "(-)", " \u2192 ", tier_labels[2], "(-)", " \u2192 ", tier_labels[3], "(-)"),
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 0 & !!sym(tiers[3]) == 1 ~
            paste0(tier_labels[1], "(-)", " \u2192 ", tier_labels[2], "(-)", " \u2192 ", tier_labels[3], "(+)"),
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 1 & !!sym(tiers[3]) == 0 ~
            paste0(tier_labels[1], "(-)", " \u2192 ", tier_labels[2], "(+)", " \u2192 ", tier_labels[3], "(-)"),
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 1 & !!sym(tiers[3]) == 1 ~
            paste0(tier_labels[1], "(-)", " \u2192 ", tier_labels[2], "(+)", " \u2192 ", tier_labels[3], "(+)"),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 0 & !!sym(tiers[3]) == 0 ~
            paste0(tier_labels[1], "(+)", " \u2192 ", tier_labels[2], "(-)", " \u2192 ", tier_labels[3], "(-)"),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 0 & !!sym(tiers[3]) == 1 ~
            paste0(tier_labels[1], "(+)", " \u2192 ", tier_labels[2], "(-)", " \u2192 ", tier_labels[3], "(+)"),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 1 & !!sym(tiers[3]) == 0 ~
            paste0(tier_labels[1], "(+)", " \u2192 ", tier_labels[2], "(+)", " \u2192 ", tier_labels[3], "(-)"),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 1 & !!sym(tiers[3]) == 1 ~
            paste0(tier_labels[1], "(+)", " \u2192 ", tier_labels[2], "(+)", " \u2192 ", tier_labels[3], "(+)"),
        ),
        "Three Tiers Risk" = Freq,
        label_three = make_label(Freq, .n, .ci_lower, .ci_upper)
      ) %>%
      select(
        !!sym(tiers[1]),
        !!sym(tiers[2]),
        !!sym(tiers[3]),
        Combo,
        `One Tier Risk`,
        `Two Tiers Risk`,
        `Three Tiers Risk`,
        label_three
      )

    p3 = risk_df3 %>%
      pivot_longer(
        cols = c("One Tier Risk", "Two Tiers Risk", "Three Tiers Risk"),
        names_to = "Methods",
        values_to = "Values"
      ) %>%
      mutate(
        Methods = factor(
          Methods,
          levels = c("One Tier Risk", "Two Tiers Risk", "Three Tiers Risk")
        ),
        display_label = case_when(
          Methods == "Three Tiers Risk" ~ label_three,
          TRUE ~ scales::percent(round(Values, 4))
        )
      ) %>%
      ggplot(aes(x = Methods, y = Combo, fill = Values)) +
      geom_tile(color = "white") +
      scale_fill_distiller(
        palette = "RdYlBu",
        direction = -1,
        name = "Positive %",
        limits = c(0, 1)
      ) +
      geom_text(aes(label = display_label), size = text_size) +
      theme_minimal() +
      labs(
        title = title,
        x = "",
        y = ""
      ) +
      theme(axis.text.x = element_text(face = "bold", hjust = 0.8)) +
      # tier 1 annotate
      annotate(
        "rect",
        xmin = 0.5,
        xmax = 1.5,
        ymin = 0.5,
        ymax = 4.5,
        fill = color_mapper(strategy1$Freq[1]),
        color = "white"
      ) +
      annotate(
        "text",
        x = 1,
        y = 2.5,
        label = make_label(strategy1$Freq[1], strategy1$.n[1],
                           strategy1$.ci_lower[1], strategy1$.ci_upper[1]),
        size = text_size
      ) +
      annotate(
        "rect",
        xmin = 0.5,
        xmax = 1.5,
        ymin = 4.5,
        ymax = 8.5,
        fill = color_mapper(strategy1$Freq[2]),
        color = "white"
      ) +
      annotate(
        "text",
        x = 1,
        y = 6.5,
        label = make_label(strategy1$Freq[2], strategy1$.n[2],
                           strategy1$.ci_lower[2], strategy1$.ci_upper[2]),
        size = text_size
      ) +
      # tier 2 annotate
      annotate(
        "rect",
        xmin = 1.5,
        xmax = 2.5,
        ymin = 0.5,
        ymax = 2.5,
        fill = color_mapper(strategy2$Freq[1]),
        color = "white"
      ) +
      annotate(
        "text",
        x = 2,
        y = 1.5,
        label = make_label(strategy2$Freq[1], strategy2$.n[1],
                           strategy2$.ci_lower[1], strategy2$.ci_upper[1]),
        size = text_size
      ) +
      annotate(
        "rect",
        xmin = 1.5,
        xmax = 2.5,
        ymin = 2.5,
        ymax = 4.5,
        fill = color_mapper(strategy2$Freq[2]),
        color = "white"
      ) +
      annotate(
        "text",
        x = 2,
        y = 3.5,
        label = make_label(strategy2$Freq[2], strategy2$.n[2],
                           strategy2$.ci_lower[2], strategy2$.ci_upper[2]),
        size = text_size
      ) +
      annotate(
        "rect",
        xmin = 1.5,
        xmax = 2.5,
        ymin = 4.5,
        ymax = 6.5,
        fill = color_mapper(strategy2$Freq[3]),
        color = "white"
      ) +
      annotate(
        "text",
        x = 2,
        y = 5.5,
        label = make_label(strategy2$Freq[3], strategy2$.n[3],
                           strategy2$.ci_lower[3], strategy2$.ci_upper[3]),
        size = text_size
      ) +
      annotate(
        "rect",
        xmin = 1.5,
        xmax = 2.5,
        ymin = 6.5,
        ymax = 8.5,
        fill = color_mapper(strategy2$Freq[4]),
        color = "white"
      ) +
      annotate(
        "text",
        x = 2,
        y = 7.5,
        label = make_label(strategy2$Freq[4], strategy2$.n[4],
                           strategy2$.ci_lower[4], strategy2$.ci_upper[4]),
        size = text_size
      )

    return(
      risk_tile = list(strategy = strategy3, risk_df = risk_df3, plot = p3)
    )
  }
}
