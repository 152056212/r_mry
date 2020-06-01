# -------------------------
# Author : mry
# Date : 2020-05-31
# Modification : v0.1
# Note : 
## Staper [统计期间] -
## Fresgn [数据频率] - M=月度，Q=季度
## Datasgn [数据标识] - A=当期数，B=累计数
## Provsgn [省份标识] - 见附录三《行政区划分编码》
## Provnm [省份名称] -
## Epim0301 [居民消费价格指数] - 上年同期=100
## Epim0302 [居民消费价格指数-城镇] - 上年同期=100
## Epim0303 [居民消费价格指数-农村] - 上年同期=100
## Epim0304 [商品零售价格指数] - 上年同期=100
## Epim0305 [商品零售价格指数-城镇] - 上年同期=100
## Epim0306 [商品零售价格指数-农村] - 上年同期=100
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
ori_cpi_rpi <- read_csv("cpi_rpi.csv")

# ---------------------------------------------------------------------------
# typeof(str_count("2018-01", "^2019"))
# rm(ocr_2019_tj_now_ab_wider)
# ori_cpi_rpi$Staper[2]
# str_count(ori_cpi_rpi$Staper[2], "^2019") == 0
# ---------------------------------------------------------------------------

# ------------------------------------整理数据start---------------------------------------
# 根据正则表达式筛选2019年范围内的数据
ocr_2019 <- ori_cpi_rpi %>% rename( Rdate = `Staper`) %>% dplyr::filter(  str_count(Rdate, "19$") != 0 )
# 查询2019年天津市的各项指数Epim0301/2/3/4/5/6
ocr_2019_tj <- ocr_2019 %>% dplyr::filter( str_count(Provnm, "^天津") != 0 )
# 查询2019年天津市居民消费价格指数的当期数
ocr_2019_tj_now <- ocr_2019_tj %>% dplyr::filter( Datasgn %in% c("A" ) != 0 ) %>% select( "Rdate","Provnm","Epim0301" ) 
# 格式化日期--值引用str_replace(item, "Dec", "12")
idxDate <- 0
for( item in ocr_2019_tj_now$Rdate ){
  idxDate <- idxDate + 1
  switch (str_sub(item, 1, 3),
    Jan = ocr_2019_tj_now$Rdate[idxDate] <-"01-2019",
    Feb = ocr_2019_tj_now$Rdate[idxDate] <-"02-2019",
    Mar = ocr_2019_tj_now$Rdate[idxDate] <-"03-2019",
    Apr = ocr_2019_tj_now$Rdate[idxDate] <-"04-2019",
    May = ocr_2019_tj_now$Rdate[idxDate] <-"05-2019",
    Jun = ocr_2019_tj_now$Rdate[idxDate] <-"06-2019",
    Jul = ocr_2019_tj_now$Rdate[idxDate] <-"07-2019",
    Aug = ocr_2019_tj_now$Rdate[idxDate] <-"08-2019",
    Sep = ocr_2019_tj_now$Rdate[idxDate] <-"09-2019",
    Oct = ocr_2019_tj_now$Rdate[idxDate] <-"10-2019",
    Nov = ocr_2019_tj_now$Rdate[idxDate] <-"11-2019",
    Dec = ocr_2019_tj_now$Rdate[idxDate] <-"12-2019",
  )
}

# 以数据标识为表头旋转表格(长变宽)
ocr_2019_tj_now_ab <- ocr_2019_tj %>% select( "Rdate","Datasgn","Provnm","Epim0301" ) 
ocr_2019_tj_now_ab_wider = pivot_wider(ocr_2019_tj_now_ab, names_from = "Datasgn", values_from = "Epim0301" )
# ------------------------------------整理数据end-----------------------------------------




