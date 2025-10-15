map_roc <- function(
    vars_to_calc_roc,
    response,
    data_roc,
    best_method = "c",
    best_policy = "random",
    label = "No label",
    plot = TRUE,
    color = "Diagnosis",
    legend_labels = NULL,
    title = "ROC Curve",
    help = FALSE) {
  # 1. 必要的包 -------------------------------------------------------------------------------------


  library(pROC)
  library(tidyverse)


  # 2. 帮助说明 -------------------------------------------------------------------------------------


  if (isTRUE(help)) {
    cat(
      "==========================================================================================\n",
      "这是通过同时传入多个 predictors 变量而进行批量 roc test 的函数,\n",
      "你需要传入一个包含 predictors 的向量, 一个 response 参数, 一个准备好的 data_roc 数据。\n",
      "这个函数返回一个list, 包含一个 roc_list: 包含所有的 roc 模型结果；还包含一个包含灵敏特异性和AUC的 dataframe.\n",
      "当然也可以通过 best_method 参数设置提取灵敏特异度时采用 youden(y) 方法，还是最近策略 'c', 默认为 'c'.\n",
      "最后，这个函数通过 plot = TRUE or FALSE 控制是否要画图，画出的图可以继续使用 ggplot 系统进行编辑。\n",
      "\n",
      "vars_to_calc_roc: 接收一个向量，如 c('variable1','variable2','variable3')\n",
      "response: 字符型，roc 计算的 response\n",
      "data_roc: 一个已经处理好的数据, 二分类变量都出列为 0,1.\n",
      "best_method: 计算灵敏特异度所用的方案, 可以是 (youden) 或 (c), 默认为 (c)\n",
      'best_policy: 当出现多个 best 的 threshold, 如何确定用哪一个。取值 "stop", "omit", "random", 默认为 "random" \n',
      "help: 当为 TRUE, 显示这一堆说明。\n",
      "plot: 默认为 TRUE, 决定是否要画个 ROC 图.\n",
      "color: 如果要画图, legend 的标题是什么, 默认为 Diagnosis.\n",
      "title: 如果要画图, title 是什么, 某认为 ROC Curve\n"
    )
    return("==========================================================================================\n")
  }


  # 3. 功能区 --------------------------------------------------------------------------------------

  ## 定义一个空列表用于接收 roc_list

  roc_list <- c()


  ## for 循环，遍历需要进行 roc test 的变量，并以 reponse 参数为 response。
  ## 没计算完毕，都将本次计算的 roc 对象存入 roc_list

  for (i in vars_to_calc_roc) {
    temp_roc <- roc(data_roc[, response], data_roc[, i], auc = T, ci = T)
    # print(temp_roc)
    roc_list[[i]] <- temp_roc
  }

  ## 得到 roc_list 后，批量计算每个 roc 对象对应的灵敏特异性和AUC

  se_sp <- roc_list |>
    map_df(
      ~ ci.coords(
        .x,
        x = "best",
        best.method = best_method,
        best.policy = best_policy,
        input = "threshold",
        ret = c("threshold", "specificity", "sensitivity", "npv", "ppv")
      )
    ) %>%
    mutate(
      "Threshold" = threshold[, 2],
      "Specificity (95% CI)" = sprintf("%.3f (%.3f, %.3f)", specificity[, 2], specificity[, 1], specificity[, 3]),
      "Sensitivity (95% CI)" = sprintf("%.3f (%.3f, %.3f)", sensitivity[, 2], sensitivity[, 1], sensitivity[, 3]),
      "Negative Predictive Value (95% CI)" = sprintf("%.3f (%.3f, %.3f)", npv[, 2], npv[, 1], npv[, 3]),
      "Positive Predictive Value (95% CI)" = sprintf("%.3f (%.3f, %.3f)", ppv[, 2], ppv[, 1], ppv[, 3])
    ) %>%
    select(c("Threshold", "Specificity (95% CI)", "Sensitivity (95% CI)", "Negative Predictive Value (95% CI)", "Positive Predictive Value (95% CI)"))


  auc_ci <- roc_list |>
    map_df(~ ci(.x)) |>
    t() |>
    as.data.frame() |>
    round(3) |>
    mutate(
      "AUC (95% CI)" = paste0(V2, " (", V1, ", ", V3, ")")
    ) |>
    select(dplyr::last_col())

  auc_ci_se_sp <- cbind(auc_ci, se_sp) |>
    rownames_to_column() |>
    mutate(
      Threshold = if_else(Threshold != 0.5, Threshold, NA)
    )

  auc_ci_se_sp$Group <- rep(label, length(vars_to_calc_roc))
  colnames(auc_ci_se_sp) <- c("Methods", "AUC (95% CI)", "Threshold", "Specificity (95% CI)", "Sensitivity (95% CI)", "Negative Predictive Value (95% CI)", "Positive Predictive Value (95% CI)", "Label")

  ## 画出 ROC 曲线图
  if (isTRUE(plot)) {
    p_roc <- ggroc(roc_list, linewidth = 0.9, legacy.axes = TRUE) +
      labs(
        x = "1 - Specificity",
        y = "Sensitivity",
        color = color,
        title = title
      ) +
      theme_minimal() +
      geom_abline(slope = 1, linetype = "dashed") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1L)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
      ggsci::scale_color_lancet(labels = if (is.null(legend_labels)) vars_to_calc_roc else legend_labels) +
      theme(
        plot.title.position = "panel",
        plot.title = element_text(face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.3),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
  } else {
    p_roc <- c()
  }



  # 4. 返回区 --------------------------------------------------------------------------------------


  returns <- list()
  returns[["roc_list"]] <- roc_list
  returns[["auc_ci_se_sp"]] <- auc_ci_se_sp
  returns[["roc_curve"]] <- p_roc

  return(returns)
}
