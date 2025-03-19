# 加载必要的包
library(dplyr)
library(RTransferEntropy)  # 转移熵计算

# 初始化结果存储列表
te_results <- list()
signif_stars <- c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1) # 显著性标记

# 参数设置
target_provinces <- c("辽宁", "浙江", "河北", "新疆", "北京", "广西", "江苏", "江西", 
                      "福建", "安徽", "内蒙古", "山西", "天津", "河南", "四川", "湖北", 
                      "湖南", "广东", "山东", "云南", "海南", "青海", "贵州", "宁夏", 
                      "吉林", "黑龙江", "陕西", "甘肃", "西藏"
)

# 主分析循环
for (target_province in target_provinces) {
  tryCatch({
    cat("\n=== 正在处理省份:", target_province, "===\n")
    
    # 数据预处理
    province_sites <- new_df %>%
      filter(grepl(paste0("^", target_province), province)) %>%
      select(predicted_max_tem_35, O3_8h_max_boxcox) %>%
      na.omit()  # 去除缺失值
    
    # 提取温度和臭氧数据
    temp <- province_sites$predicted_max_tem_35
    ozone <- province_sites$O3_8h_max_boxcox
    
    # 计算转移熵
    te_result <- transfer_entropy(temp, ozone, lx = 2, ly = 1, nboot = 100)
    
    # 将结果存入列表
    te_results[[target_province]] <- te_result
    cat("\n转移熵计算完成:", target_province, "\n")
    
  }, error = function(e) {
    cat("\n!!! 处理省份", target_province, "时出错: ", e$message, "\n")
  })
}

# 按省份输出转移熵分析结果
for (target_province in target_provinces) {
  te_result <- te_results[[target_province]]
  
  # 提取从温度到臭氧和从臭氧到温度的转移熵系数
  te_temp_to_ozone <- te_result$coef[1, "te"]
  te_ozone_to_temp <- te_result$coef[2, "te"]
  
  # 提取p值
  p_temp_to_ozone <- te_result$coef[1, "p-value"]
  p_ozone_to_temp <- te_result$coef[2, "p-value"]
  
  # 输出结果
  cat("\n=== 正在处理省份:", target_province, "===\n")
  cat("从温度到臭氧的转移熵:", te_temp_to_ozone, "p值:", p_temp_to_ozone, "\n")
  cat("从臭氧到温度的转移熵:", te_ozone_to_temp, "p值:", p_ozone_to_temp, "\n")
  
  # 判断转移熵大小
  if (te_temp_to_ozone > te_ozone_to_temp) {
    print(paste(target_province, "温度先于臭氧"))
  } else if (te_temp_to_ozone < te_ozone_to_temp) {
    print(paste(target_province, "臭氧先于温度"))
  } else {
    print(paste(target_province, "温度和臭氧影响相当"))
  }
  
  # 显著性检查
  if (p_temp_to_ozone < 0.001) {
    print("从温度到臭氧的影响显著 (p < 0.001)")
  } else if (p_temp_to_ozone < 0.01) {
    print("从温度到臭氧的影响显著 (p < 0.01)")
  } else if (p_temp_to_ozone < 0.05) {
    print("从温度到臭氧的影响显著 (p < 0.05)")
  } else {
    print("从温度到臭氧的影响不显著")
  }
  
  if (p_ozone_to_temp < 0.001) {
    print("从臭氧到温度的影响显著 (p < 0.001)")
  } else if (p_ozone_to_temp < 0.01) {
    print("从臭氧到温度的影响显著 (p < 0.01)")
  } else if (p_ozone_to_temp < 0.05) {
    print("从臭氧到温度的影响显著 (p < 0.05)")
  } else {
    print("从臭氧到温度的影响不显著")
  }
}


# 创建一个空的数据框来存储结果
result_df <- data.frame(
  Province = character(),
  TE_Temp_to_Ozone = numeric(),
  P_Temp_to_Ozone = character(),
  TE_Ozone_to_Temp = numeric(),
  P_Ozone_to_Temp = character(),
  Dominant_Direction = character(),
  stringsAsFactors = FALSE
)

# 手动存储每个省份的结果，使用英文名称
result_df <- rbind(result_df, data.frame(
  Province = "Liaoning",
  TE_Temp_to_Ozone = 0.01130649,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.004852319,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Zhejiang",
  TE_Temp_to_Ozone = 0.02086674,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.005229586,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Hebei",
  TE_Temp_to_Ozone = 0.01327406,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.005909546,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Xinjiang",
  TE_Temp_to_Ozone = 0.01979067,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.008576813,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Beijing",
  TE_Temp_to_Ozone = 0.01031306,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.008503927,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Guangxi",
  TE_Temp_to_Ozone = 0.009747755,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.006231624,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Jiangsu",
  TE_Temp_to_Ozone = 0.01470588,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.006120615,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Jiangxi",
  TE_Temp_to_Ozone = 0.01011489,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.007545881,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Fujian",
  TE_Temp_to_Ozone = 0.01119572,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.00540621,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Anhui",
  TE_Temp_to_Ozone = 0.01657674,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.01153604,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Inner Mongolia",
  TE_Temp_to_Ozone = 0.01701312,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.006472292,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Shanxi",
  TE_Temp_to_Ozone = 0.04545306,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.01002086,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Tianjin",
  TE_Temp_to_Ozone = 0.0182848,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.006434189,
  P_Ozone_to_Temp = "0.02 *",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Henan",
  TE_Temp_to_Ozone = 0.01471364,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.01346229,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Sichuan",
  TE_Temp_to_Ozone = 0.01888677,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.01540293,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Hubei",
  TE_Temp_to_Ozone = 0.01241405,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.005219146,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Hunan",
  TE_Temp_to_Ozone = 0.01429968,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.00681831,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Guangdong",
  TE_Temp_to_Ozone = 0.01106916,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.004739848,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Shandong",
  TE_Temp_to_Ozone = 0.02083901,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.00934926,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Yunnan",
  TE_Temp_to_Ozone = 0.004906123,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.003584182,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Hainan",
  TE_Temp_to_Ozone = 0.007363963,
  P_Temp_to_Ozone = "0.21 ns",
  TE_Ozone_to_Temp = 0.003010856,
  P_Ozone_to_Temp = "0.61 ns",
  Dominant_Direction = "Temperature precedes Ozone"
))
result_df <- rbind(result_df, data.frame(
  Province = "Qinghai",
  TE_Temp_to_Ozone = 0.0153264,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.004724038,
  P_Ozone_to_Temp = "0.16 ns",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Guizhou",
  TE_Temp_to_Ozone = 0.01642284,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.00716359,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Ningxia",
  TE_Temp_to_Ozone = 0.02313151,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.01292441,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Jilin",
  TE_Temp_to_Ozone = 0.01520015,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.007630436,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Heilongjiang",
  TE_Temp_to_Ozone = 0.006557175,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.003657998,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Shaanxi",
  TE_Temp_to_Ozone = 0.02374663,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.009806407,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

result_df <- rbind(result_df, data.frame(
  Province = "Gansu",
  TE_Temp_to_Ozone = 0.01605758,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.008848391,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))


library(ggplot2)
library(reshape2)
result_long <- reshape2::melt(result_df, id.vars = "Province", 
                              measure.vars = c("TE_Temp_to_Ozone", "TE_Ozone_to_Temp"),
                              variable.name = "Transfer_Entropy_Type", 
                              value.name = "Transfer_Entropy_Value")

# 创建条形图
ggplot(result_long, aes(x = Province, y = Transfer_Entropy_Value, fill = Transfer_Entropy_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Transfer Entropy between Temperature and Ozone",
       x = "Province",
       y = "Transfer Entropy Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("TE_Temp_to_Ozone" = "blue", "TE_Ozone_to_Temp" = "red"),
                    name = "Transfer Entropy Type",
                    labels = c("Temperature to Ozone", "Ozone to Temperature"))
result_df <- rbind(result_df, data.frame(
  Province = "Tibet",
  TE_Temp_to_Ozone = 0.01764601,
  P_Temp_to_Ozone = "0.00 ***",
  TE_Ozone_to_Temp = 0.005402113,
  P_Ozone_to_Temp = "0.00 ***",
  Dominant_Direction = "Temperature precedes Ozone"
))

# 打印结果数据框
print(result_df)
