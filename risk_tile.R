#' 风险热图绘制函数
#'
#' 该函数用于根据输入的数据和分层变量生成风险热图，展示不同分层组合下的风险比例。
#'
#' @param data_risk 数据框，包含需要分析的风险数据。
#' @param reference 字符向量，指定参考变量列名。
#' @param tiers 字符向量，指定分层变量列名。
#' @param tier_labels 字符向量，可选参数，指定分层变量的标签。如果未提供，则默认使用 `tiers` 的值。
#' @param title 字符串，可选参数，指定热图的标题，默认为 "Risk plot"。
#'
#' @return 返回一个列表，包含以下内容：
#' \itemize{
#'   \item \code{strategy}: 分层策略的数据框。
#'   \item \code{risk_df}: 风险数据框，包含计算后的风险比例。
#'   \item \code{plot}: 生成的热图对象。
#' }
#'
#' @details
#' 该函数支持1到3个分层变量的分析，并根据分层变量的组合生成热图。热图使用颜色渐变表示风险比例，红色表示高风险，蓝色表示低风险。
#'
#' @examples
#' \dontrun{
#' # 示例用法
#' result <- risk_tile(
#'   data_risk = my_data,
#'   reference = "outcome",
#'   tiers = c("tier1", "tier2"),
#'   tier_labels = c("Tier 1", "Tier 2"),
#'   title = "My Risk Plot"
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
  title = "Risk plot"
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

  # 1. 设置颜色匹配函数
  color_mapper <- function(x) {
    require(scales)
    require(RColorBrewer)

    # x: 0~1 之间的数值（可向量）
    if (any(x < 0 | x > 1, na.rm = TRUE)) {
      stop("输入的数值必须在 0 到 1 之间")
    }

    # 加载需要的包
    if (
      !requireNamespace("RColorBrewer", quietly = TRUE) ||
        !requireNamespace("scales", quietly = TRUE)
    ) {
      stop("请先安装 RColorBrewer 和 scales 包")
    }

    # 1. 获取 RdYlBu 色板并反转方向 (direction = -1)
    cols <- RColorBrewer::brewer.pal(11, "RdYlBu")
    cols <- rev(cols)

    # 2. 创建连续映射函数，与 scale_fill_distiller() 保持一致
    pal <- scales::gradient_n_pal(
      cols,
      values = scales::rescale(seq(0, 1, length.out = 11))
    )

    # 3. 返回对应颜色（十六进制字符串）
    pal(x)
  }

  # 检查 tiers 是否为多个值的情况
  if (length(tiers) == 1) {
    strategy1 <- data.frame()

    for (i in c(0, 1)) {
      if (!tiers[1] %in% names(data_risk)) {
        # 修改为检查列名
        stop(paste("Column", tiers[1], "not found in data_risk"))
      }
      df <- data_risk %>%
        filter(!!sym(tiers[1]) == i) %>% # 动态引用列名
        select(!!sym(reference)) %>%
        mutate(
          !!sym(reference) := factor(!!sym(reference), levels = c(0, 1))
        ) %>% # 强制保留 0 和 1 的水平
        table() %>%
        prop.table() %>%
        as.data.frame() %>%
        mutate(!!sym(tiers[1]) := i)
      strategy1 <- bind_rows(strategy1, df)
    }

    strategy1 = strategy1 %>% filter(!!sym(reference) == 1) # 确保 reference 是有效列

    risk_df1 = strategy1 %>%
      mutate(
        "One Tier Risk" = Freq,
        Combo = if_else(
          !!sym(tiers[1]) == 0,
          paste(tier_labels[1], "(-)"),
          paste(tier_labels[1], "(+)")
        )
      )

    p1 = risk_df1 %>%
      pivot_longer(
        cols = c("One Tier Risk"),
        names_to = "Methods",
        values_to = "Values"
      ) %>%
      ggplot(aes(x = Methods, y = Combo, fill = Values)) +
      geom_tile(color = "white") + # 绘制方块
      scale_fill_distiller(
        palette = "RdYlBu", # 设置色板
        direction = -1, # 反转颜色方向：高值为红，低值为蓝
        name = "Positive %",
        limits = c(0, 1)
      ) +
      geom_text(aes(label = scales::percent(round(Values, 4))), size = 4) + # 在格子中显示数值
      theme_minimal() + # 使用简洁主题
      labs(
        title = title,
        x = "",
        y = ""
      ) +
      theme(axis.text.x = element_text(face = "bold", hjust = 0.8))

    return(
      risk_tile = list(strategy = strategy1, risk_df = risk_df1, plot = p1)
    ) # 返回绘图结果
  }

  if (length(tiers) == 2) {
    # Tier 1

    strategy1 <- data.frame()

    for (i in c(0, 1)) {
      if (!tiers[1] %in% names(data_risk)) {
        # 修改为检查列名
        stop(paste("Column", tiers[1], "not found in data_risk"))
      }
      df <- data_risk %>%
        filter(!!sym(tiers[1]) == i) %>% # 动态引用列名
        select(!!sym(reference)) %>%
        mutate(
          !!sym(reference) := factor(!!sym(reference), levels = c(0, 1))
        ) %>% # 强制保留 0 和 1 的水平
        table() %>%
        prop.table() %>%
        as.data.frame() %>%
        mutate(!!sym(tiers[1]) := i)
      strategy1 <- bind_rows(strategy1, df)
    }

    strategy1 = strategy1 %>% filter(!!sym(reference) == 1) # 确保 reference 是有效列

    risk_df1 = strategy1 %>%
      mutate(
        "One Tier Risk" = Freq,
        Combo = if_else(
          !!sym(tiers[1]) == 0,
          paste(tier_labels[1], "(-)"),
          paste(tier_labels[1], "(+)")
        )
      )

    # Tier 2

    strategy2 <- data.frame()

    for (i in c(0, 1)) {
      for (j in c(0, 1)) {
        if (!tiers[1] %in% names(data_risk)) {
          # 修改为检查列名
          stop(paste("Column", tiers[1], "not found in data_risk"))
        } else if (!tiers[2] %in% names(data_risk)) {
          stop(paste("Column", tiers[2], "not found in data_risk"))
        }

        df <- data_risk %>%
          filter(!!sym(tiers[1]) == i & !!sym(tiers[2]) == j) %>% # 动态引用列名
          select(!!sym(reference)) %>%
          mutate(
            !!sym(reference) := factor(!!sym(reference), levels = c(0, 1))
          ) %>% # 强制保留 0 和 1 的水平
          table() %>%
          prop.table() %>%
          as.data.frame() %>%
          mutate(!!sym(tiers[1]) := i, !!sym(tiers[2]) := j)
        strategy2 <- bind_rows(strategy2, df)
      }
    }

    strategy2 = strategy2 %>% filter(!!sym(reference) == 1)

    risk_df2 = strategy2 %>%
      # filter(!!sym(reference) == 1) %>%
      mutate(
        "One Tier Risk" = if_else(
          !!sym(tiers[1]) == 0,
          round(strategy1$Freq[1]),
          round(strategy1$Freq[2])
        ),
        Combo = case_when(
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 0 ~
            paste0(tier_labels[1], "(-)", " → ", tier_labels[2], "(-)"),
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 1 ~
            paste0(tier_labels[1], "(-)", " → ", tier_labels[2], "(+)"),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 0 ~
            paste0(tier_labels[1], "(+)", " → ", tier_labels[2], "(-)"),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 1 ~
            paste0(tier_labels[1], "(+)", " → ", tier_labels[2], "(+)"),
        ),
        "Two Tiers Risk" = Freq
      ) %>%
      select(
        !!sym(tiers[1]),
        !!sym(tiers[2]),
        Combo,
        `One Tier Risk`,
        `Two Tiers Risk`
      )

    p2 = risk_df2 %>%
      pivot_longer(
        cols = c("One Tier Risk", "Two Tiers Risk", ),
        names_to = "Methods",
        values_to = "Values"
      ) %>%
      ggplot(aes(x = Methods, y = Combo, fill = Values)) +
      geom_tile(color = "white") + # 绘制方块
      # scale_fill_gradient2(low = "blue",mid = "yellow",high = "red") +  # 颜色渐变，红色表示比例高
      scale_fill_distiller(
        palette = "RdYlBu", # 设置色板
        direction = -1, # 反转颜色方向：高值为红，低值为蓝
        name = "Positive %",
        limits = c(0, 1)
      ) +
      geom_text(aes(label = scales::percent(round(Values, 4))), size = 4) + # 在格子中显示数值
      theme_minimal() + # 使用简洁主题
      labs(
        title = title,
        x = "",
        y = "",
        # fill = "Positive %"
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
        label = paste0(round(strategy1$Freq[1], 4) * 100, "%"),
        size = 4
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
        label = paste0(round(strategy1$Freq[2], 4) * 100, "%"),
        size = 4
      )

    return(
      risk_tile = list(strategy = strategy2, risk_df = risk_df2, plot = p2)
    )
  }

  if (length(tiers) == 3) {
    strategy1 <- data.frame()

    for (i in c(0, 1)) {
      if (!tiers[1] %in% names(data_risk)) {
        # 修改为检查列名
        stop(paste("Column", tiers[1], "not found in data_risk"))
      } else if (!tiers[2] %in% names(data_risk)) {
        stop(paste("Column", tiers[2], "not found in data_risk"))
      } else if (!tiers[3] %in% names(data_risk)) {
        stop(paste("Column", tiers[3], "not found in data_risk"))
      }

      df <- data_risk %>%
        filter(!!sym(tiers[1]) == i) %>% # 动态引用列名
        select(!!sym(reference)) %>%
        mutate(
          !!sym(reference) := factor(!!sym(reference), levels = c(0, 1))
        ) %>% # 强制保留 0 和 1 的水平
        table() %>%
        prop.table() %>%
        as.data.frame() %>%
        mutate(!!sym(tiers[1]) := i)
      strategy1 <- bind_rows(strategy1, df)
    }

    strategy1 = strategy1 %>% filter(!!sym(reference) == 1) # 确保 reference 是有效列

    risk_df1 = strategy1 %>%
      mutate(
        "One Tier Risk" = Freq,
        Combo = if_else(
          !!sym(tiers[1]) == 0,
          paste(tier_labels[1], "(-)"),
          paste(tier_labels[1], "(+)")
        )
      )

    # Tier 2

    strategy2 <- data.frame()

    for (i in c(0, 1)) {
      for (j in c(0, 1)) {
        if (!tiers[1] %in% names(data_risk)) {
          # 修改为检查列名
          stop(paste("Column", tiers[1], "not found in data_risk"))
        } else if (!tiers[2] %in% names(data_risk)) {
          stop(paste("Column", tiers[2], "not found in data_risk"))
        }

        df <- data_risk %>%
          filter(!!sym(tiers[1]) == i & !!sym(tiers[2]) == j) %>% # 动态引用列名
          select(!!sym(reference)) %>%
          mutate(
            !!sym(reference) := factor(!!sym(reference), levels = c(0, 1))
          ) %>% # 强制保留 0 和 1 的水平
          table() %>%
          prop.table() %>%
          as.data.frame() %>%
          mutate(!!sym(tiers[1]) := i, !!sym(tiers[2]) := j)
        strategy2 <- bind_rows(strategy2, df)
      }
    }

    strategy2 = strategy2 %>% filter(!!sym(reference) == 1)

    risk_df2 = strategy2 %>%
      # filter(!!sym(reference) == 1) %>%
      mutate(
        "One Tier Risk" = if_else(
          !!sym(tiers[1]) == 0,
          round(strategy1$Freq[1]),
          round(strategy1$Freq[2])
        ),
        Combo = case_when(
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 0 ~
            paste0(tier_labels[1], "(-)", " → ", tier_labels[2], "(-)"),
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 1 ~
            paste0(tier_labels[1], "(-)", " → ", tier_labels[2], "(+)"),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 0 ~
            paste0(tier_labels[1], "(+)", " → ", tier_labels[2], "(-)"),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 1 ~
            paste0(tier_labels[1], "(+)", " → ", tier_labels[2], "(+)"),
        ),
        "Two Tiers Risk" = Freq
      ) %>%
      select(
        !!sym(tiers[1]),
        !!sym(tiers[2]),
        Combo,
        `One Tier Risk`,
        `Two Tiers Risk`
      )

    # Tier 3
    strategy3 <- data.frame()

    for (i in c(0, 1)) {
      for (j in c(0, 1)) {
        for (k in c(0, 1)) {
          df <- data_risk %>%
            filter(
              !!sym(tiers[1]) == i & !!sym(tiers[2]) == j & !!sym(tiers[3]) == k
            ) %>% # 动态引用列名
            select(!!sym(reference)) %>%
            mutate(
              !!sym(reference) := factor(!!sym(reference), levels = c(0, 1))
            ) %>% # 强制保留 0 和 1 的水平
            table() %>%
            prop.table() %>%
            as.data.frame() %>%
            mutate(
              !!sym(tiers[1]) := i,
              !!sym(tiers[2]) := j,
              !!sym(tiers[3]) := k
            )
          strategy3 <- bind_rows(strategy3, df)
        }
      }
    }

    strategy3 = strategy3 %>% filter(!!sym(reference) == 1)

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
            paste0(
              tier_labels[1],
              "(-)",
              " → ",
              tier_labels[2],
              "(-)",
              " → ",
              tier_labels[3],
              "(-)"
            ),
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 0 & !!sym(tiers[3]) == 1 ~
            paste0(
              tier_labels[1],
              "(-)",
              " → ",
              tier_labels[2],
              "(-)",
              " → ",
              tier_labels[3],
              "(+)"
            ),
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 1 & !!sym(tiers[3]) == 0 ~
            paste0(
              tier_labels[1],
              "(-)",
              " → ",
              tier_labels[2],
              "(+)",
              " → ",
              tier_labels[3],
              "(-)"
            ),
          !!sym(tiers[1]) == 0 & !!sym(tiers[2]) == 1 & !!sym(tiers[3]) == 1 ~
            paste0(
              tier_labels[1],
              "(-)",
              " → ",
              tier_labels[2],
              "(+)",
              " → ",
              tier_labels[3],
              "(+)"
            ),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 0 & !!sym(tiers[3]) == 0 ~
            paste0(
              tier_labels[1],
              "(+)",
              " → ",
              tier_labels[2],
              "(-)",
              " → ",
              tier_labels[3],
              "(-)"
            ),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 0 & !!sym(tiers[3]) == 1 ~
            paste0(
              tier_labels[1],
              "(+)",
              " → ",
              tier_labels[2],
              "(-)",
              " → ",
              tier_labels[3],
              "(+)"
            ),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 1 & !!sym(tiers[3]) == 0 ~
            paste0(
              tier_labels[1],
              "(+)",
              " → ",
              tier_labels[2],
              "(+)",
              " → ",
              tier_labels[3],
              "(-)"
            ),
          !!sym(tiers[1]) == 1 & !!sym(tiers[2]) == 1 & !!sym(tiers[3]) == 1 ~
            paste0(
              tier_labels[1],
              "(+)",
              " → ",
              tier_labels[2],
              "(+)",
              " → ",
              tier_labels[3],
              "(+)"
            ),
        ),
        "Three Tiers Risk" = Freq
      ) %>%
      select(
        !!sym(tiers[1]),
        !!sym(tiers[2]),
        !!sym(tiers[3]),
        Combo,
        `One Tier Risk`,
        `Two Tiers Risk`,
        `Three Tiers Risk`
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
        )
      ) %>%
      ggplot(aes(x = Methods, y = Combo, fill = Values)) +
      geom_tile(color = "white") + # 绘制方块
      # scale_fill_gradient2(low = "blue",mid = "yellow",high = "red") +  # 颜色渐变，红色表示比例高
      scale_fill_distiller(
        palette = "RdYlBu", # 设置色板
        direction = -1, # 反转颜色方向：高值为红，低值为蓝
        name = "Positive %",
        limits = c(0, 1)
      ) +
      geom_text(aes(label = scales::percent(round(Values, 4))), size = 4) + # 在格子中显示数值
      theme_minimal() + # 使用简洁主题
      labs(
        title = title,
        x = "",
        y = "",
        # fill = "Positive %"
      ) +
      theme(axis.text.x = element_text(face = "bold", hjust = 0.8)) +
      # tier 1
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
        label = paste0(round(strategy1$Freq[1], 4) * 100, "%"),
        size = 4
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
        label = paste0(round(strategy1$Freq[2], 4) * 100, "%"),
        size = 4
      ) +
      # tier 2
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
        label = paste0(round(strategy2$Freq[1], 4) * 100, "%"),
        size = 4
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
        label = paste0(round(strategy2$Freq[2], 4) * 100, "%"),
        size = 4
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
        label = paste0(round(strategy2$Freq[3], 4) * 100, "%"),
        size = 4
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
        label = paste0(round(strategy2$Freq[4], 4) * 100, "%"),
        size = 4
      )

    return(
      risk_tile = list(strategy = strategy3, risk_df = risk_df3, plot = p3)
    )
  }
}
