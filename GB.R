# 加载需要的库
library(lme4)
library(parallel)
library(dplyr)
library(logistf)
library(data.table)
library(future)
library(future.apply)

# 增加倾向得分截断防止极端值
trim_propensity <- function(ps, trim_level = 0.01) {
  ps <- pmax(ps, trim_level)
  ps <- pmin(ps, 1 - trim_level)
  ps
}

fit_weighted_lmm <- function(data, indices, treated_idx, control_idx) {
  # 根据引导样本的索引抽取数据
  bootstrap_data <- data[indices, ]
  
  # 使用加权最小二乘法拟合模型
  fit <- try({
    lmer(
      O3_8h_max_boxcox ~ max_tem_35 + dew_tem_scaled + slp_scaled + 
        wd_scaled + wsr_scaled + sctc_scaled + lpd_scaled + (1 | site_id),
      data = bootstrap_data,
      weights = bootstrap_data$weight,  # 使用传递的权重
      control = lmerControl(
        optimizer = "nloptwrap",
        calc.derivs = FALSE,  # 禁用导数计算以加快速度
        optCtrl = list(maxfun = 500)  # 限制最大迭代次数
      )
    )
  }, silent = TRUE)
  
  if (inherits(fit, "try-error")) {
    return(NA)
  }
  return(fixef(fit)["max_tem_35"])
}

run_future_bootstrap <- function(n_bootstrap, data, strata, weights) {
  # 预计算治疗组和控制组的索引
  treated_idx <- which(data$max_tem_35 == 1)
  control_idx <- which(data$max_tem_35 == 0)
  
  # 使用 data.table 优化数据结构
  data <- as.data.table(data)
  
  # 设置并行环境
  plan(multisession, workers = max(detectCores() - 2, 1))  # 使用多核心并行
  
  # 并行执行 Bootstrap
  bootstrap_results <- future_lapply(1:n_bootstrap, function(i) {
    # 随机抽样索引（带 strata 和 weights）
    indices <- sample(1:nrow(data), size = nrow(data), replace = TRUE, prob = weights)
    # 调用加权模型拟合函数
    fit_weighted_lmm(data, indices, treated_idx, control_idx)
  })
  
  # 提取有效结果
  estimates <- unlist(bootstrap_results)
  convergence_rate <- mean(!is.na(estimates))
  
  list(
    estimates = estimates[!is.na(estimates)],  
    convergence_rate = convergence_rate
  )
}

set.seed(123)

# 计算倾向得分（带截断）
model_ps_firth <- logistf(
  max_tem_35 ~ latitude + observation_field_meter + month + tem_scaled + 
    dew_tem_scaled + slp_scaled + wd_scaled + wsr_scaled + sctc_scaled + lpd_scaled,
  data = new_df
)
new_df$propensity_score <- trim_propensity(predict(model_ps_firth, type = "response"))

# 定义 strata 和 weights
strata <- new_df$max_tem_35
weights <- ifelse(
  new_df$max_tem_35 == 1,
  (1 / new_df$propensity_score) / sum(1 / new_df$propensity_score),
  (1 / (1 - new_df$propensity_score)) / sum(1 / (1 - new_df$propensity_score))
)

# 运行 Bootstrap（测试时用100次，正式运行1000次）
bootstrap_results <- run_future_bootstrap(1000, new_df, strata, weights)

# 结果分析
valid_estimates <- bootstrap_results$estimates
cat("优化后的Bootstrap结果：\n",
    "有效迭代次数:", length(valid_estimates), "\n",
    "模型收敛率:", bootstrap_results$convergence_rate, "\n",
    "平均效应:", mean(valid_estimates), "\n",
    "标准误:", sd(valid_estimates), "\n",
    "95%置信区间:", quantile(valid_estimates, c(0.025, 0.975)))
