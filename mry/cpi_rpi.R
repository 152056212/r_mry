# -------------------------
# Author : mry
# Date : 2020-05-31
# Modification : v0.1
# -------------------------
# 检测tidyverse环境
if(require("tidyverse")){
  print("成功载入包")
} else {
  print("不存在这个包，正在尝试安装")
  install.packages("tidyverse")
  if(require("tidyverse")){
    print("成功安装并载入包")
  } else {
    stop("安装失败")
  }
}

# 数据导入
# # cpi_rpi.csv为分地区居民消费价格及商品零售价格分类指数月度数据源文件
ori_cpi_rpi = read_csv("cpi_rpi.csv")





