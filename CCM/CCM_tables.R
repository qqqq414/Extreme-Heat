# 示例数据
seasonal_stats_temp_to_ozone <- data.frame(
  Season = c("Spring", "Summer", "Fall", "Winter"),
  mean = c(-0.018, 0.022, -0.022, -0.033),  # 手动输入每个季节的mean
  max = c(-0.009, 0.034, -0.015, -0.030),   # 手动输入每个季节的max
  sd = c(0.006, 0.010, 0.004, 0.002)     # 手动输入每个季节的标准差
)

# 使用kable呈现表格
library(knitr)
kable(seasonal_stats_temp_to_ozone, caption = "Seasonal CCM Statistics of Temperature to Ozone", row.names = FALSE)


# 示例数据
seasonal_stats_ozone_to_temp <- data.frame(
  Season = c("Spring", "Summer", "Fall", "Winter"),
  mean = c(0.005, 0.023, 0.004, 0.000),  # 手动输入每个季节的mean
  max = c(0.009, 0.032, 0.007, 0.000),   # 手动输入每个季节的max
  sd = c(0.003, 0.007, 0.002, 0.000)     # 手动输入每个季节的标准差
)

# 使用kable呈现表格
library(knitr)
kable(seasonal_stats_ozone_to_temp, caption = "Seasonal CCM Statistics of Ozone to Temperature", row.names = FALSE)

#---------------------------------------------------------------------------------------------------------------------------------------

fall_province_stats_list <- read.csv("/Users/swifty/Desktop/winter_province_stats_list.csv")

# 创建两个空的列表，用于存储统计结果
temp_to_ozone_stats <- list("positive" = 0, "non_positive" = 0)
ozone_to_temp_stats <- list("positive" = 0, "non_positive" = 0)

# 按Province分类进行统计
for (i in 1:nrow(fall_province_stats_list)) {
  # 获取当前行的数据
  current_row <- fall_province_stats_list[i, ]
  
  # 判断类别并统计mean的值
  if (grepl("temp_to_ozone", current_row$Province)) {
    # temp_to_ozone类别
    if (current_row$mean > 0) {
      temp_to_ozone_stats$positive <- temp_to_ozone_stats$positive + 1
    } else {
      temp_to_ozone_stats$non_positive <- temp_to_ozone_stats$non_positive + 1
    }
  } else if (grepl("ozone_to_temp", current_row$Province)) {
    # ozone_to_temp类别
    if (current_row$mean > 0) {
      ozone_to_temp_stats$positive <- ozone_to_temp_stats$positive + 1
    } else {
      ozone_to_temp_stats$non_positive <- ozone_to_temp_stats$non_positive + 1
    }
  }
}

# 输出结果
cat("Number of temp_to_ozone with positive mean:", temp_to_ozone_stats$positive, "\n")
cat("Number of temp_to_ozone with non-positive mean:", temp_to_ozone_stats$non_positive, "\n")
cat("Number of ozone_to_temp with positive mean:", ozone_to_temp_stats$positive, "\n")
cat("Number of ozone_to_temp with non-positive mean:", ozone_to_temp_stats$non_positive, "\n")


# 创建数据框 Positive_cities_temp
Positive_cities_temp <- data.frame(
  Season = c("Spring", "Summer", "Fall", "Winter"),
  Positive = c(4, 22, 5, 0),  # 手动输入每个季节的正相关城市数量
  Non_positive = c(25, 7, 24, 29)  # 手动输入每个季节的负相关城市数量
)

# 确保数据框已经正确创建
print(Positive_cities_temp)

# 使用kable呈现表格
library(knitr)
kable(Positive_cities_temp, caption = "The number of cities where extreme heat were significantly positively 
      (negatively) correlated with ozone", row.names = FALSE)
# 创建数据框 Positive_cities_ozone
Positive_cities_ozone <- data.frame(
  Season = c("Spring", "Summer", "Fall", "Winter"),
  Positive = c(17, 24, 11, 0),  # 手动输入每个季节的正相关城市数量
  Non_positive = c(12, 5, 18, 29)   # 手动输入每个季节的负相关城市数量
)

# 确保数据框已经正确创建
print(Positive_cities_ozone)

# 使用kable呈现表格
library(knitr)
kable(Positive_cities_ozone, caption = "The number of cities where ozone were significantly positively
      (negatively) correlated with extreme heat", row.names = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------------

# 创建一个空的数据框来存储统计结果
province_stats <- data.frame(
  Province = character(),
  Mean_Temperature_to_Ozone = numeric(),
  Max_Temperature_to_Ozone = numeric(),
  Sd_Temperature_to_Ozone = numeric(),
  Mean_Ozone_to_Temperature = numeric(),
  Max_Ozone_to_Temperature = numeric(),
  Sd_Ozone_to_Temperature = numeric(),
  stringsAsFactors = FALSE
)

# 输入每个省份的统计数据
province_stats <- rbind(province_stats, data.frame(
  Province = "辽宁",
  Mean_Temperature_to_Ozone = 0.05069200,
  Max_Temperature_to_Ozone = 0.08286974,
  Sd_Temperature_to_Ozone = 0.02616773,
  Mean_Ozone_to_Temperature = 0.007761302,
  Max_Ozone_to_Temperature = 0.012119829,
  Sd_Ozone_to_Temperature = 0.002577673
))

province_stats <- rbind(province_stats, data.frame(
  Province = "河北",
  Mean_Temperature_to_Ozone = 0.03771294,
  Max_Temperature_to_Ozone = 0.05351584,
  Sd_Temperature_to_Ozone = 0.01275910,
  Mean_Ozone_to_Temperature = 0.017216225,
  Max_Ozone_to_Temperature = 0.024474214,
  Sd_Ozone_to_Temperature = 0.005179338
))

province_stats <- rbind(province_stats, data.frame(
  Province = "北京",
  Mean_Temperature_to_Ozone = 0.06232500,
  Max_Temperature_to_Ozone = 0.07400072,
  Sd_Temperature_to_Ozone = 0.01119629,
  Mean_Ozone_to_Temperature = 0.016765272,
  Max_Ozone_to_Temperature = 0.023984157,
  Sd_Ozone_to_Temperature = 0.004911533
))

province_stats <- rbind(province_stats, data.frame(
  Province = "江西",
  Mean_Temperature_to_Ozone = 0.05192631,
  Max_Temperature_to_Ozone = 0.06288456,
  Sd_Temperature_to_Ozone = 0.01212659,
  Mean_Ozone_to_Temperature = 0.028773967,
  Max_Ozone_to_Temperature = 0.035667421,
  Sd_Ozone_to_Temperature = 0.005846538
))

province_stats <- rbind(province_stats, data.frame(
  Province = "广西",
  Mean_Temperature_to_Ozone = 0.06144219,
  Max_Temperature_to_Ozone = 0.07100242,
  Sd_Temperature_to_Ozone = 0.01268728,
  Mean_Ozone_to_Temperature = 0.054212728,
  Max_Ozone_to_Temperature = 0.062127658,
  Sd_Ozone_to_Temperature = 0.008502154
))

province_stats <- rbind(province_stats, data.frame(
  Province = "福建",
  Mean_Temperature_to_Ozone = 0.07949604,
  Max_Temperature_to_Ozone = 0.09839215,
  Sd_Temperature_to_Ozone = 0.01725107,
  Mean_Ozone_to_Temperature = 0.009147883,
  Max_Ozone_to_Temperature = 0.011342232,
  Sd_Ozone_to_Temperature = 0.002164468
))

province_stats <- rbind(province_stats, data.frame(
  Province = "内蒙古",
  Mean_Temperature_to_Ozone = 0.03206647,
  Max_Temperature_to_Ozone = 0.05560814,
  Sd_Temperature_to_Ozone = 0.01751969,
  Mean_Ozone_to_Temperature = 0.027259404,
  Max_Ozone_to_Temperature = 0.037524308,
  Sd_Ozone_to_Temperature = 0.009593404
))

province_stats <- rbind(province_stats, data.frame(
  Province = "天津",
  Mean_Temperature_to_Ozone = 0.05037675,
  Max_Temperature_to_Ozone = 0.05997682,
  Sd_Temperature_to_Ozone = 0.00837420,
  Mean_Ozone_to_Temperature = 0.028350599,
  Max_Ozone_to_Temperature = 0.033929842,
  Sd_Ozone_to_Temperature = 0.005062742
))

province_stats <- rbind(province_stats, data.frame(
  Province = "四川",
  Mean_Temperature_to_Ozone = 0.06141273,
  Max_Temperature_to_Ozone = 0.08125506,
  Sd_Temperature_to_Ozone = 0.01653424,
  Mean_Ozone_to_Temperature = 0.022799542,
  Max_Ozone_to_Temperature = 0.030377984,
  Sd_Ozone_to_Temperature = 0.006077998
))

province_stats <- rbind(province_stats, data.frame(
  Province = "湖北",
  Mean_Temperature_to_Ozone = 0.050087601,
  Max_Temperature_to_Ozone = 0.061346773,
  Sd_Temperature_to_Ozone = 0.008090783,
  Mean_Ozone_to_Temperature = 0.04839760,
  Max_Ozone_to_Temperature = 0.06264582,
  Sd_Ozone_to_Temperature = 0.01175969
))

province_stats <- rbind(province_stats, data.frame(
  Province = "湖南",
  Mean_Temperature_to_Ozone = 0.018457778,
  Max_Temperature_to_Ozone = 0.026344481,
  Sd_Temperature_to_Ozone = 0.005790236,
  Mean_Ozone_to_Temperature = 0.015425552,
  Max_Ozone_to_Temperature = 0.020731613,
  Sd_Ozone_to_Temperature = 0.004405203
))

province_stats <- rbind(province_stats, data.frame(
  Province = "山东",
  Mean_Temperature_to_Ozone = 0.05102808,
  Max_Temperature_to_Ozone = 0.06931315,
  Sd_Temperature_to_Ozone = 0.01404903,
  Mean_Ozone_to_Temperature = 0.035239952,
  Max_Ozone_to_Temperature = 0.043137064,
  Sd_Ozone_to_Temperature = 0.007549886
))

# 四舍五入数据框中的所有数值列到小数点后三位
province_stats_df_rounded <- province_stats
province_stats_df_rounded[, 2:7] <- round(province_stats_df_rounded[, 2:7], 3)

# 将四舍五入后的数据保存为 CSV 文件
write.csv(province_stats_df_rounded, "/Users/swifty/Desktop/rounded_province_stats.csv", row.names = FALSE)