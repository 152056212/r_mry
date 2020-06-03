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
if(require("tidyverse") && require("ggplot2") && require("ggthemes") && require("shiny") && require("devtools")){
  print("成功载入包")
} else {
  print("不存在这个包，正在尝试安装")
  install.packages("tidyverse")
  install.packages("ggplot2")
  install.packages("ggthemes")
  install.packages("shiny")
  install.packages("devtools")
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
# rm(cpi_rpi_China)
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


# ------------------------------------数据可视化start-------------------------------------
## 2019年全国及各省市居民消费指数和零售价格指数分布直方图---------------------------------
# 以数据标识为表头旋转表格
cpi_rpi_2019_a <-  ocr_2019 %>% dplyr::filter( Datasgn %in% c("A" ) != 0 ) %>% rename( CPI = `Epim0301`, RPI = `Epim0304`) %>% select( "Rdate","Datasgn","Provnm","CPI", "RPI" )  %>%  pivot_longer( cols = c(`CPI`, `RPI`), names_to = "指数类型", values_to = "value")

ggplot(cpi_rpi_2019_a) +
  # 几何对象函数
  geom_col( mapping = aes(x = `Provnm`, y = value, fill = `指数类型`)) + 
  # 标度
  labs(x = NULL, y = "指数") + 
  # 主题
  theme_solarized(base_family = "cnfont", base_size = 10) + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "red", size = 40),
    plot.subtitle = element_text(hjust = 0.5, color = "red", size = 30),
    plot.caption = element_text(family = "enfont", size = 25),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.1, 0.8)
  )

# ----------------------------------------------------------------------------------------
## 2009-2020年中国居民消费指数和零售价格指数折线图----------------------------------------
# install_github("jeevanyue/echarter")
# library(echarter)
cpi_rpi_China  <- ori_cpi_rpi %>% rename( Rdate = `Staper`) %>% dplyr::filter(  str_count(Provnm, "中国$") != 0, str_count(Rdate, "19$|20$") != 0 ) %>% dplyr::filter(Datasgn %in% c("A" ) != 0 ) %>% rename( CPI = `Epim0301`, RPI = `Epim0304`) %>% select( "Rdate","CPI", "RPI" )
barnline<-list(
  title = list(text = '2019年中国CPI和RPI'),
  tooltip = list(),
  legend = list(data=c('CPI','RPI')),
  xAxis= list(
    type= 'category',
    data= cpi_rpi_China$Rdate,
    splitLine = list(show=FALSE) # 删掉竖线
  ),
  yAxis= list(
    list(
      type= 'value',
      name= 'CPI',
      splitLine=list(
        show=FALSE # 删掉横线
      )
    ),
    list(
      type= 'value',
      name= 'RPI',
      splitLine=list(
        show=TRUE # 删掉横线
      )
    
    )
  ),
  series = list(
    list(
      type = 'bar',
      name = 'CPI',
      data = cpi_rpi_China$CPI
    ),
    list(
      name = 'RPI',
      type = 'line',
      data = cpi_rpi_China$RPI
    )
      
  )
  
)

echart(barnline)

## 2019年全国及各省市居民消费指数和零售价格指数分布直方图---------------------------------
cpi_rpi_rose  <- ori_cpi_rpi %>% rename( Rdate = `Staper`) %>% 
  dplyr::filter(  str_count(Provnm, "中国$") == 0, str_count(Rdate, "Dec-19$") != 0, Datasgn %in% c("A" ) != 0 ) %>% 
  rename( CPI = `Epim0301`, RPI = `Epim0304`) %>% 
  select( "Rdate","Provnm","CPI" )
top_30 <- cpi_rpi_rose %>% arrange(CPI)
rose <- top_30 %>% 
  mutate(
    id = row_number(),
    height = log(CPI, 2), 
    label = case_when(
      id <= 15 ~ str_c(Provnm," ", CPI, ""),
      TRUE ~str_c(Provnm,"\n", CPI, "")
    )
  )

left_half <- rose %>% dplyr::filter(id <= 15)

right_half <- rose %>% dplyr::filter(id > 15)

p3 <- ggplot(data = rose, mapping = aes(x = id, y = height, label = label)) +
  geom_col(aes(fill = id), width = 1, size = 0) +
  geom_col(
    mapping = aes(y = 2),
    fill = "white",
    width = 1,
    alpha = 0.2,
    size = 0
  ) +
  geom_col(
    mapping = aes(y = 1),
    fill = "white",
    width = 1,
    alpha = 0.2,
    size = 0
  ) +  
  scale_y_continuous(limits = c(-2, 13)) +
  coord_polar() +
  theme_void() +
  geom_text(
    data = left_half,
    size = 3,
    nudge_y = 2,
    angle = 95 - 180 * c(1:15) / 15,
    fontface = "bold"
  ) +
  geom_text(
    data = right_half,
    mapping = aes(size = factor(id)),
    nudge_y = -1.1,
    color = "white",
    fontface = "bold"
  ) +
  scale_size_manual(values = c(rep(2, 7),rep(3, 5),rep(3.5, 3)))

## 2019年全国及各省市居民消费指数和零售价格指数分布直方图---------------------------------
cpi_rpi_China_3d  <- ori_cpi_rpi %>% rename( Rdate = `Staper`) %>% dplyr::filter(  str_count(Provnm, "中国$") == 0 ) %>% dplyr::filter(Datasgn %in% c("A" ) != 0 ) %>% rename( CPI = `Epim0301`, RPI = `Epim0304`) %>% select( "Rdate", "Provnm", "CPI", "RPI" )
sizeValue <- '57%';
symbolSize <- 2.5;
idx3D <- 1
formatList <- list(list( "Rdate","Provnm","CPI"))
while(idx3D != 4124){
  idx3D <- idx3D + 1
  formatList[[idx3D]] <- list(cpi_rpi_China_3d$Rdate[idx3D],
                              cpi_rpi_China_3d$Provnm[idx3D],
                              cpi_rpi_China_3d$CPI[idx3D])
}

barnline3d <-list(
  tooltip = list(),
  grid3D = {
    width = '50%'
  },
  xAxis3D = {},
  yAxis3D = {},
  zAxis3D = {},
  grid = list(
    list(left = '50%', width = '20%', bottom = sizeValue)
  ),
  xAxis = list(
    list(type="value",gridIndex = 0,name="Rdate", axisLabel = list(rotate = 50, interval = 0))
  ),
  yAxis = list(
    list(type="value",gridIndex = 0,name="CPI", axisLabel = list(rotate = 50, interval = 0))
  ),
  dataset = list(
    dimensions = list(
      "Rdate","Provnm",list(name="CPI",type="ordinal")
    ),
    data = formatList
  ),
  series = list(
    list(
      type = "scatter3D",
      symbolSize = 3,
      encode = list(
        x = "Rdate",
        y = "Provnm",
        z = "CPI",
        tooltip = list(0,1,2)
      )
    )
  )
)
echart(barnline3d)

# ------------------------------------数据可视化end---------------------------------------

