library(rEDM)
library(lme4)
library(ggplot2)
library(dplyr)
library(gridExtra)  
library(cowplot)    

# 清除历史变量
rm(list=ls())

# 设置全局随机种子
set.seed(123)

# 加载数据和所需库
load("/Users/swifty/Desktop/new_df.RData")

# 选择需要分析的多个省份
target_provinces <- c("辽宁", "河北","北京", "江西", "广西", "福建", "内蒙古", 
                      "天津", "四川", "湖北", "湖南", "山东")

# 存储图像的列表
plots <- list()

# 设置组合图形的布局 (3行4列)
par(mfrow = c(3, 4))

# 创建省份中英文对照表
province_translation <- c(
  "辽宁" = "Liaoning",
  "河北" = "Hebei",
  "北京" = "Beijing",
  "江西" = "Jiangxi",
  "广西" = "Guangxi",
  "福建" = "Fujian",
  "内蒙古" = "Inner Mongolia",
  "天津" = "Tianjin",
  "四川" = "Sichuan",
  "湖北" = "Hubei",
  "湖南" = "Hunan",
  "山东" = "Shandong"
)

# 循环遍历每个省份进行分析
for (i in 1:length(target_provinces)) {
  target_province <- target_provinces[i]
  print(paste("Processing province:", target_province))
  
  # 筛选出该省下的所有站点
  matching_sites <- new_df %>%
    filter(grepl(target_province, province))
  
  # 提取该省（市）下所有匹配站点的数据
  city_data <- new_df %>%
    filter(site_id %in% matching_sites$site_id)
  
  # 获取该城市的夏季数据（6月到8月）
  season_data_city <- city_data %>%
    filter(month(date) %in% c(6, 7, 8))  # 选择夏季数据
  
  # 使用混合效应模型进行建模
  if (n_distinct(season_data_city$site_id) > 1) {
    model <- lmer(
      O3_8h_max_boxcox ~ max_tem_35 + dew_tem_scaled + slp_scaled + 
        wd_scaled + wsr_scaled + sctc_scaled + lpd_scaled + (1 | site_id),
      data = season_data_city
    )
  } else {
    model <- lm(
      O3_8h_max_boxcox ~ max_tem_35 + dew_tem_scaled + slp_scaled + 
        wd_scaled + wsr_scaled + sctc_scaled + lpd_scaled,
      data = season_data_city
    )
  }
  
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
  
  # 确保 LibSize 列在第一列
  ccm_data$LibSize <- rep(seq(10, 100, by = 10), length.out = nrow(ccm_data))
  ccm_data <- ccm_data %>%
    select(LibSize, everything())  # 将 LibSize 放在第一列
  
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
        sample = 300,
        showPlot = FALSE
      )
      
      # 臭氧 -> 高温
      ccm_ozone_to_temp <- rEDM::CCM(
        dataFrame = ccm_data,
        E = 3,
        Tp = 0,
        columns = "residual_ozone",  # 使用回归残差
        target = "temperature_binary",  
        libSizes = "10 100 10",
        sample = 300,
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
    ccm_col <- grep(":", names(ccm_result), value = TRUE)[1]
    plot_data <- data.frame(
      LibSize = ccm_result$LibSize,
      rho = ccm_result[[ccm_col]]
    )
    
    mean_rho <- mean(plot_data$rho, na.rm = TRUE)
    max_rho <- max(plot_data$rho, na.rm = TRUE)
    sd_rho <- sd(plot_data$rho, na.rm = TRUE)
    
    return(c(mean = mean_rho, max = max_rho, sd = sd_rho))
  }
  
  # 计算并获取统计结果
  temp_to_ozone_stats <- calculate_stats(ccm_results$temp_to_ozone)
  ozone_to_temp_stats <- calculate_stats(ccm_results$ozone_to_temp)
  
  # 打印结果
  print(paste(target_province, "Statistics for Temperature -> Ozone:"))
  print(temp_to_ozone_stats)
  
  print(paste(target_province, "Statistics for Ozone -> Temperature:"))
  print(ozone_to_temp_stats)
  
  # 绘制因果关系图
  data_temp_to_ozone <- data.frame(
    LibSize = ccm_results$temp_to_ozone$LibSize,
    CausalRelation = ccm_results$temp_to_ozone$`temperature_binary:residual_ozone`
  )
  
  data_ozone_to_temp <- data.frame(
    LibSize = ccm_results$ozone_to_temp$LibSize,
    CausalRelation = ccm_results$ozone_to_temp$`residual_ozone:temperature_binary`
  )
  
  # 创建图形
  plot <- ggplot() +
    geom_line(data = data_temp_to_ozone, aes(x = LibSize, y = CausalRelation, color = "Temperature -> Ozone")) +
    geom_point(data = data_temp_to_ozone, aes(x = LibSize, y = CausalRelation)) +
    geom_line(data = data_ozone_to_temp, aes(x = LibSize, y = CausalRelation, color = "Ozone -> Temperature")) +
    geom_point(data = data_ozone_to_temp, aes(x = LibSize, y = CausalRelation)) +
    labs(
      x = "Library Size (LibSize)", 
      y = "Causal Relationship",
      title = province_translation[target_province]  # 使用英文省份名
    ) +
    scale_color_manual(values = c("Temperature -> Ozone" = "blue", "Ozone -> Temperature" = "red")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "none"  # 去掉每个图形的图例
    )
  
  # 将图像添加到列表中
  plots[[i]] <- plot
}

# 使用grid.arrange()将所有图像显示为 3x4 的组图
grid.arrange(grobs = plots, nrow = 3, ncol = 4)