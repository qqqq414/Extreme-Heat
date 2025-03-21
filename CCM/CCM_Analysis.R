# 清除历史变量
rm(list=ls())

# 设置全局随机种子
set.seed(123)

# 加载数据和所需库
load("/Users/swifty/Desktop/new_df.RData")
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(lme4)  # 用于混合效应模型

# 创建一个空列表，用于存储每个省份的统计量
province_stats_list <- list()
num_cities_positive <- list()  # 用于存储正向因果关系的省份

# 选择多个省份（例如河北，其他省份可以以类似方式添加）
target_provinces <- c(
  "辽宁", "浙江", "河北", "新疆", "北京", "广西", "江苏", "江西", "福建", "安徽", 
  "内蒙古", "山西", "天津", "河南", "四川", "湖北", "湖南", "广东", "山东", "云南", 
  "海南", "青海", "贵州", "宁夏", "吉林", "黑龙江", "陕西", "甘肃", "西藏"
)

# 逐个处理每个省份
for (target_province in target_provinces) {
  print(paste("Processing province:", target_province))
  
  # 筛选出该省下的所有站点
  matching_sites <- new_df %>%
    filter(grepl(target_province, province))  
  
  # 提取该省（市）下所有匹配站点的数据
  city_data <- new_df %>%
    filter(site_id %in% matching_sites$site_id)
  
  # 获取该城市的秋季数据（9月到11月）
  season_data_city <- city_data %>%
    filter(month(date) %in% c(5, 6, 7, 8, 9))  # 选择秋季数据
  
  # 检查站点数量是否大于1，避免混合效应模型因样本过少失败
  if (n_distinct(season_data_city$site_id) > 1) {
    # 使用混合效应模型进行建模
    model <- lmer(
      O3_8h_max_boxcox ~ max_tem_35 + dew_tem_scaled + slp_scaled + 
        wd_scaled + wsr_scaled + sctc_scaled + lpd_scaled + (1 | site_id),
      data = season_data_city
    )
    
    # 提取残差
    residuals <- residuals(model)
    
    # 将回归残差作为新的目标变量，进行CCM分析
    ccm_data <- season_data_city %>%
      select(ozone = O3_8h_max, 
             temperature_binary = max_tem_35,
             tem_scaled, dew_tem_scaled, slp_scaled,
             wd_scaled, wsr_scaled, sctc_scaled, lpd_scaled) %>%
      mutate(across(everything(), as.numeric)) %>%
      na.omit() %>%
      as.data.frame()
    
    ccm_data$residual_ozone <- residuals
  } else {
    # 对于只有一个站点的数据，使用固定效应模型（lm）
    model <- lm(
      O3_8h_max_boxcox ~ max_tem_35 + dew_tem_scaled + slp_scaled + 
        wd_scaled + wsr_scaled + sctc_scaled + lpd_scaled,
      data = season_data_city
    )
    
    # 提取残差
    residuals <- residuals(model)
    
    # 将回归残差作为新的目标变量，进行CCM分析
    ccm_data <- season_data_city %>%
      select(ozone = O3_8h_max, 
             temperature_binary = max_tem_35,
             tem_scaled, dew_tem_scaled, slp_scaled,
             wd_scaled, wsr_scaled, sctc_scaled, lpd_scaled) %>%
      mutate(across(everything(), as.numeric)) %>%
      na.omit() %>%
      as.data.frame()
    
    ccm_data$residual_ozone <- residuals
  }
  
  # 确保 LibSize 列在第一列
  ccm_data$LibSize <- rep(seq(10, 100, by = 10), length.out = nrow(ccm_data))
  ccm_data <- ccm_data %>%
    select(LibSize, everything())  # 将 LibSize 放在第一列
  
  
  
  # 列名验证
  required_cols <- c("residual_ozone", "temperature_binary", "LibSize")
  if (!all(required_cols %in% colnames(ccm_data))) {
    stop(paste("Missing columns:", 
               paste(setdiff(required_cols, colnames(ccm_data)), collapse = ", ")))
  }
  
  # 定义运行 CCM 分析的函数
  run_ccm_season <- function(ccm_data) {
    tryCatch({
      # 高温 -> 臭氧
      ccm_temp_to_ozone <- rEDM::CCM(
        dataFrame = ccm_data,
        E = 3,
        Tp = 0,
        columns = "temperature_binary",  
        target = "residual_ozone",  # 使用回归残差
        libSizes = "10 100 10",
        sample = 100,
        showPlot = FALSE
      )
      
      # 臭氧 -> 高温
      ccm_ozone_to_temp <- rEDM::CCM(
        dataFrame = ccm_data,
        E = 3,
        Tp = 0,
        columns = "residual_ozone", 
        target = "temperature_binary",  
        libSizes = "10 100 10",
        sample = 100,
        showPlot = FALSE
      )
      
      # 返回 ccm_results 和 ccm_data
      return(list(
        temp_to_ozone = ccm_temp_to_ozone, 
        ozone_to_temp = ccm_ozone_to_temp,
        ccm_data = ccm_data  # 返回 ccm_data
      ))
      
    }, error = function(e) {
      message("CCM Error: ", e$message)
      return(NULL)
    })
  } 
  
  # 执行 CCM 分析
  ccm_results <- run_ccm_season(ccm_data)
  
  # 计算预测技能的统计量
  calculate_stats <- function(ccm_result) {
    # 提取预测技能数据
    ccm_col <- grep(":", names(ccm_result), value = TRUE)[1]
    plot_data <- data.frame(
      LibSize = ccm_result$LibSize,
      rho = ccm_result[[ccm_col]]
    )
    
    # 计算统计量
    mean_rho <- mean(plot_data$rho, na.rm = TRUE)
    max_rho <- max(plot_data$rho, na.rm = TRUE)
    sd_rho <- sd(plot_data$rho, na.rm = TRUE)  # 计算标准差
    
    # 存储为对应季节的列表
    result <- list(
      mean = mean_rho, 
      max = max_rho, 
      sd = sd_rho  # 添加标准差
    )
    
    return(result)
  }
  
  # 将每个省份的结果存储在列表中
  province_stats_list[[paste0(target_province, "_temp_to_ozone")]] <- calculate_stats(ccm_results$temp_to_ozone)
  province_stats_list[[paste0(target_province, "_ozone_to_temp")]] <- calculate_stats(ccm_results$ozone_to_temp)
}

# 查看最终的省份统计列表和正向因果的省份列表
print(province_stats_list)


# 初始化统计结果列表
final_stats <- list()

# 初始化均值、最大值、标准差的存储
mean_values_temp_to_ozone <- numeric(0)
max_values_temp_to_ozone <- numeric(0)
sd_values_temp_to_ozone <- numeric(0)

mean_values_ozone_to_temp <- numeric(0)
max_values_ozone_to_temp <- numeric(0)
sd_values_ozone_to_temp <- numeric(0)

# 遍历省份统计列表并计算各省的mean、max和sd
for (province in names(province_stats_list)) {
  # 获取 temp_to_ozone 和 ozone_to_temp 对应的统计量
  if (grepl("temp_to_ozone", province)) {
    mean_values_temp_to_ozone <- c(mean_values_temp_to_ozone, province_stats_list[[province]]$mean)
    max_values_temp_to_ozone <- c(max_values_temp_to_ozone, province_stats_list[[province]]$max)
    sd_values_temp_to_ozone <- c(sd_values_temp_to_ozone, province_stats_list[[province]]$sd)
  }
  
  if (grepl("ozone_to_temp", province)) {
    mean_values_ozone_to_temp <- c(mean_values_ozone_to_temp, province_stats_list[[province]]$mean)
    max_values_ozone_to_temp <- c(max_values_ozone_to_temp, province_stats_list[[province]]$max)
    sd_values_ozone_to_temp <- c(sd_values_ozone_to_temp, province_stats_list[[province]]$sd)
  }
}

# 计算所有省份的mean、max、sd的均值
final_stats$mean_temp_to_ozone <- mean(mean_values_temp_to_ozone)
final_stats$max_temp_to_ozone <- mean(max_values_temp_to_ozone)
final_stats$sd_temp_to_ozone <- mean(sd_values_temp_to_ozone)

final_stats$mean_ozone_to_temp <- mean(mean_values_ozone_to_temp)
final_stats$max_ozone_to_temp <- mean(max_values_ozone_to_temp)
final_stats$sd_ozone_to_temp <- mean(sd_values_ozone_to_temp)

# 输出最终的统计结果
print(final_stats)

# 计算每个省份的正向因果关系
for (province in target_provinces) {
  # 获取当前省份的统计量
  temp_to_ozone_mean <- province_stats_list[[paste0(province, "_temp_to_ozone")]]$mean
  ozone_to_temp_mean <- province_stats_list[[paste0(province, "_ozone_to_temp")]]$mean
  
  # 如果 temp_to_ozone 的均值大于 ozone_to_temp 的均值，认为是正向因果关系
  if (temp_to_ozone_mean > ozone_to_temp_mean) {
    num_cities_positive[[province]] <- 1  # 正向因果关系
  } else {
    num_cities_positive[[province]] <- 0  # 反向因果或无因果
  }
}

# 输出正向因果关系的省份列表
print(num_cities_positive)