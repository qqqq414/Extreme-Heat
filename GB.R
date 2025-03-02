library(lme4)
library(parallel)
library(dplyr)
library(logistf)

# 增加倾向得分截断防止极端值
trim_propensity <- function(ps, trim_level = 0.01) {
  ps <- pmax(ps, trim_level)
  ps <- pmin(ps, 1 - trim_level)
  ps
}

# 改进的Bootstrap函数
bootstrap_ipw_gb_optimized <- function(data, treated_idx, control_idx) {
  # 重新计算归一化权重
  e_treated <- data$propensity_score[treated_idx]
  e_control <- data$propensity_score[control_idx]
  
  # 计算抽样概率
  p_treated <- (1/e_treated)/sum(1/e_treated)
  p_control <- (1/(1 - e_control))/sum(1/(1 - e_control))

# 校验概率和为1
  if(abs(sum(p_treated) - 1) > 1e-6 || abs(sum(p_control) - 1) > 1e-6) {
    warning("Probability normalization failed")
    return(list(estimate = NA, convergence = FALSE))
  }

  # 获取原始样本量
  nt <- length(treated_idx)
  nc <- length(control_idx)
  
  # 高效抽样
  resampled_treated <- sample(treated_idx, replace = TRUE, prob = p_treated)
  resampled_control <- sample(control_idx, replace = TRUE, prob = p_control)
  
  # 创建重采样数据集
  resampled_data <- bind_rows(
    data[resampled_treated, ],
    data[resampled_control, ]
  )
  # 获取重采样后样本量
  nt_resampled <- sum(resampled_data$max_tem_35 == 1)
  nc_resampled <- sum(resampled_data$max_tem_35 == 0)
  
  # 计算权重 1/nt*pi (sampling p)
  resampled_data <- resampled_data %>%
    mutate(
      weight = ifelse(
        max_tem_35 == 1,
        (1/nt) * 1/(propensity_score * p_treated),
        (1/nc) * 1/((1 - propensity_score) * p_control)
      ),
      weight = pmin(weight, quantile(weight, 0.99, na.rm = TRUE))
    )
  
  # 使用更鲁棒的模型拟合
  fit <- try({
    lmer(
      O3_8h_max_boxcox ~ max_tem_35 + dew_tem_scaled + slp_scaled + 
        wd_scaled + wsr_scaled + sctc_scaled + lpd_scaled + (1 | site_id),
      data = resampled_data,
      weights = weight,
      control = lmerControl(
        optimizer = "bobyqa",  
        calc.derivs = FALSE,
        optCtrl = list(maxfun = 1e5)  # 增加迭代次数
      )
    )
  }, silent = TRUE)
  
  # 返回结果或诊断信息
  if(inherits(fit, "try-error")) {
    warning(paste("Model failed:", attr(fit, "condition")$message))
    return(list(estimate = NA, convergence = FALSE))
  }
  list(estimate = fixef(fit)["max_tem_35"], convergence = TRUE)
}

# 并行执行优化
run_optimized_bootstrap <- function(n_bootstrap, data) {
  # 预计算索引
  treated_idx <- which(data$max_tem_35 == 1)
  control_idx <- which(data$max_tem_35 == 0)
  
  # 设置并行集群
  cl <- makeCluster(detectCores() - 1)
  clusterExport(cl, c("bootstrap_ipw_gb_optimized", "treated_idx", "control_idx"),
                envir = environment())
  clusterEvalQ(cl, {
    library(lme4)
    library(dplyr)
  })
  
  # 执行并行计算
  results <- parLapply(cl, 1:n_bootstrap, function(i) {
    set.seed(123 + i)  # 确保可重复性
    bootstrap_ipw_gb_optimized(data, treated_idx, control_idx)
  })
  
  stopCluster(cl)
  
  # 提取结果
  estimates <- sapply(results, function(x) x$estimate)
  convergence_rate <- mean(sapply(results, function(x) x$convergence))
  
  list(
    estimates = estimates[!is.na(estimates)],
    convergence_rate = convergence_rate
  )
}

# 主程序执行
set.seed(123)
output_dir <- "/Users/swifty/Desktop"

# 计算倾向得分（带截断）
model_ps_firth <- logistf(
  max_tem_35 ~ latitude + observation_field_meter + month + tem_scaled + 
    dew_tem_scaled + slp_scaled + wd_scaled + wsr_scaled + sctc_scaled + lpd_scaled,
  data = new_df
)
new_df$propensity_score <- trim_propensity(predict(model_ps_firth, type = "response"))

# 运行Bootstrap（测试时用100次，正式运行1000次）
bootstrap_results <- run_optimized_bootstrap(1000, new_df)

# 结果分析
valid_estimates <- bootstrap_results$estimates
cat("优化后的Bootstrap结果：\n",
    "有效迭代次数:", length(valid_estimates), "\n",
    "模型收敛率:", bootstrap_results$convergence_rate, "\n",
    "平均效应:", mean(valid_estimates), "\n",
    "标准误:", sd(valid_estimates), "\n",
    "95%置信区间:", quantile(valid_estimates, c(0.025, 0.975)))

# 保存结果
saveRDS(bootstrap_results, file.path(output_dir, "optimized_results.rds"))

library(ggplot2)

# 假设你的数据
bootstrap_estimates <- bootstrap_results$estimates # 请根据实际情况调整
ipw_estimate <- 2.246 # 假设这是IPW的估计结果

# 计算Bootstrap的密度
bootstrap_density <- density(bootstrap_estimates)
ipw_density <- density(rep(ipw_estimate, length(bootstrap_estimates))) # IPW的密度是一个单一的估计值

# 计算Bootstrap的均值
bootstrap_mean <- mean(bootstrap_estimates)
bootstrap_df <- data.frame(x = bootstrap_density$x, y = bootstrap_density$y, method = "Bootstrap")
ipw_df <- data.frame(x = ipw_density$x, y = ipw_density$y, method = "IPW")

# 绘制折线图和区域图
ggplot() +
  # 绘制Bootstrap分布的面积
  geom_area(data = bootstrap_df, aes(x = x, y = y, fill = "Bootstrap"), alpha = 0.5) +
  # 绘制IPW分布的面积
  geom_area(data = ipw_df, aes(x = x, y = y, fill = "IPW"), alpha = 0.5) +
  geom_line(data = bootstrap_df, aes(x = x, y = y, color = "Bootstrap"), size = 1) +
  geom_line(data = ipw_df, aes(x = x, y = y, color = "IPW"), size = 1) +
  geom_vline(aes(xintercept = ipw_estimate), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = bootstrap_mean), color = "blue", linetype = "dashed", size = 1) +
 
  labs(title = "Comparison of IPW and Generalized Bootstrap Distributions", 
       x = "Estimated Effect of Extreme Heat on Ozone Concentration", 
       y = "Density") +
  scale_fill_manual(values = c("Bootstrap" = "blue", "IPW" = "red")) +
  scale_color_manual(values = c("Bootstrap" = "blue", "IPW" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.7)))  
