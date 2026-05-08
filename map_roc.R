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
    extra_metrics = c(),
    help = FALSE) {
  # 1. 参数校验 -------------------------------------------------------------------------------------

  valid_metrics <- c("npv", "ppv", "accuracy", "plr", "nlr")
  if (!all(extra_metrics %in% valid_metrics)) {
    invalid <- setdiff(extra_metrics, valid_metrics)
    stop("extra_metrics 包含无效值: ", paste(invalid, collapse = ", "),
         "\n可选值: ", paste(valid_metrics, collapse = ", "))
  }

  # 2. 必要的包 -------------------------------------------------------------------------------------

  if (!requireNamespace("pROC", quietly = TRUE)) stop("请安装 pROC 包")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("请安装 ggplot2 包")


  # 3. 帮助说明 -------------------------------------------------------------------------------------


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
      "plot: 默认为 TRUE, 决定是否要画个 ROC 图。\n",
      "color: 如果要画图, legend 的标题是什么, 默认为 Diagnosis。\n",
      "title: 如果要画图, title 是什么, 默认为 ROC Curve。\n",
      "extra_metrics: 字符向量，指定要额外输出的指标，可选值: 'npv', 'ppv', 'accuracy', 'plr', 'nlr'。\n",
      "              可任意组合，如 c('npv', 'ppv', 'accuracy')。默认为 c() 即不输出额外指标。\n",
      "\n",
      "auc_ci_se_sp 表格默认包含: Methods, AUC (95% CI), Threshold, Specificity (95% CI), Sensitivity (95% CI), Label\n",
      "可选列: NPV, PPV, Accuracy, Positive Likelihood Ratio, Negative Likelihood Ratio (均含 95% CI)\n"
    )
    return("==========================================================================================\n")
  }


  # 4. 功能区 --------------------------------------------------------------------------------------

  ## 定义一个空列表用于接收 roc_list

  roc_list <- purrr::set_names(
    purrr::map(vars_to_calc_roc, ~ pROC::roc(data_roc[, response], data_roc[, .x], auc = TRUE, ci = TRUE)),
    vars_to_calc_roc
  )

  ## 得到 roc_list 后，批量计算每个 roc 对象对应的灵敏特异性和AUC

  # 动态构建 ci.coords 的 ret 参数
  best_opts <- list(best.method = best_method, best.policy = best_policy)

  base_ret <- c("threshold", "specificity", "sensitivity")
  if ("npv" %in% extra_metrics) base_ret <- c(base_ret, "npv")
  if ("ppv" %in% extra_metrics) base_ret <- c(base_ret, "ppv")

  ci_result <- purrr::map_df(
    roc_list,
    ~ do.call(pROC::ci.coords, c(list(.x, x = "best", input = "threshold", ret = base_ret), best_opts))
  )

  # 构建基础结果列（始终输出）
  result_df <- data.frame(
    Threshold = ci_result$threshold[, 2],
    `Specificity (95% CI)` = sprintf(
      "%.3f (%.3f, %.3f)",
      ci_result$specificity[, 2],
      ci_result$specificity[, 1],
      ci_result$specificity[, 3]
    ),
    `Sensitivity (95% CI)` = sprintf(
      "%.3f (%.3f, %.3f)",
      ci_result$sensitivity[, 2],
      ci_result$sensitivity[, 1],
      ci_result$sensitivity[, 3]
    ),
    check.names = FALSE
  )

  # 可选列：NPV
  if ("npv" %in% extra_metrics) {
    result_df[["Negative Predictive Value (95% CI)"]] <- sprintf(
      "%.3f (%.3f, %.3f)",
      ci_result$npv[, 2],
      ci_result$npv[, 1],
      ci_result$npv[, 3]
    )
  }

  # 可选列：PPV
  if ("ppv" %in% extra_metrics) {
    result_df[["Positive Predictive Value (95% CI)"]] <- sprintf(
      "%.3f (%.3f, %.3f)",
      ci_result$ppv[, 2],
      ci_result$ppv[, 1],
      ci_result$ppv[, 3]
    )
  }

  # 可选列：Accuracy, PLR, NLR（从混淆矩阵计算）
  if ("accuracy" %in% extra_metrics || "plr" %in% extra_metrics || "nlr" %in% extra_metrics) {
    extra_metrics_df <- purrr::map_dfr(roc_list, function(roc_obj) {
      co <- as.data.frame(
        do.call(pROC::coords, c(list(roc_obj, x = "best", ret = c("tp", "tn", "fp", "fn")), best_opts))
      )
      tp <- co[["tp"]][1]
      tn <- co[["tn"]][1]
      fp <- co[["fp"]][1]
      fn <- co[["fn"]][1]
      n_pos <- tp + fn
      n_neg <- tn + fp
      n_total <- n_pos + n_neg
      se <- tp / n_pos
      sp <- tn / n_neg

      row <- list()

      if ("accuracy" %in% extra_metrics) {
        acc <- (tp + tn) / n_total
        acc_ci <- binom.test(tp + tn, n_total)$conf.int
        row[["Accuracy (95% CI)"]] <- sprintf(
          "%.3f (%.3f, %.3f)",
          acc, acc_ci[1], acc_ci[2]
        )
      }

      if ("plr" %in% extra_metrics) {
        plr <- se / (1 - sp)
        if (tp > 0 && fp > 0 && sp < 1) {
          var_log_plr <- 1 / tp - 1 / n_pos + 1 / fp - 1 / n_neg
          plr_ci <- exp(
            log(plr) + c(-1, 1) * qnorm(0.975) * sqrt(max(0, var_log_plr))
          )
        } else {
          plr_ci <- c(NA_real_, NA_real_)
        }
        row[["Positive Likelihood Ratio (95% CI)"]] <- sprintf(
          "%.3f (%.3f, %.3f)",
          plr, plr_ci[1], plr_ci[2]
        )
      }

      if ("nlr" %in% extra_metrics) {
        nlr <- (1 - se) / sp
        if (fn > 0 && tn > 0 && se < 1) {
          var_log_nlr <- 1 / fn - 1 / n_pos + 1 / tn - 1 / n_neg
          nlr_ci <- exp(
            log(nlr) + c(-1, 1) * qnorm(0.975) * sqrt(max(0, var_log_nlr))
          )
        } else {
          nlr_ci <- c(NA_real_, NA_real_)
        }
        row[["Negative Likelihood Ratio (95% CI)"]] <- sprintf(
          "%.3f (%.3f, %.3f)",
          nlr, nlr_ci[1], nlr_ci[2]
        )
      }

      tibble::as_tibble(row)
    })

    result_df <- cbind(result_df, extra_metrics_df)
  }

  ## AUC CI
  auc_ci <- purrr::map_df(roc_list, ~ pROC::ci(.x)) |>
    t() |>
    as.data.frame() |>
    round(3) |>
    dplyr::mutate(
      "AUC (95% CI)" = paste0(V2, " (", V1, ", ", V3, ")")
    ) |>
    dplyr::select(dplyr::last_col())

  ## 合并最终结果
  auc_ci_se_sp <- cbind(auc_ci, result_df) |>
    tibble::rownames_to_column(var = "Methods") |>
    dplyr::mutate(
      Threshold = dplyr::if_else(abs(Threshold - 0.5) > 1e-10, Threshold, NA_real_)
    )

  auc_ci_se_sp$Label <- rep(label, length(vars_to_calc_roc))

  if (isTRUE(plot)) {
    p_roc <- pROC::ggroc(roc_list, linewidth = 0.9, legacy.axes = TRUE) +
      ggplot2::labs(
        x = "1 - Specificity",
        y = "Sensitivity",
        color = color,
        title = title
      ) +
      ggplot2::theme_minimal() +
      ggplot2::geom_abline(slope = 1, linetype = "dashed") +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1L)) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
      ggsci::scale_color_lancet(labels = if (is.null(legend_labels)) vars_to_calc_roc else legend_labels) +
      ggplot2::theme(
        plot.title.position = "panel",
        plot.title = ggplot2::element_text(face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.3),
        legend.title = ggplot2::element_text(face = "bold"),
        legend.text = ggplot2::element_text(face = "bold"),
        axis.title = ggplot2::element_text(face = "bold")
      )
  } else {
    p_roc <- NULL
  }



  # 5. 返回区 --------------------------------------------------------------------------------------


  returns <- list()
  returns[["roc_list"]] <- roc_list
  returns[["auc_ci_se_sp"]] <- auc_ci_se_sp
  returns[["roc_curve"]] <- p_roc

  return(returns)
}
