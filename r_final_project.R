library(readxl)
library(tidyverse)
library(repr)

# read files--------------------------------------------------------------------
# 有哪幾個年份檔案的要讀進來 (*1)
tourist__files <- c("tourist_18", "tourist_19", "tourist_20")

# 讀檔案進來 (*1)
for (i in tourist__files) {
  assign(i, readxl::read_excel(
    paste0(
      "20",
      strsplit(i, split = "")[[1]][9],
      strsplit(i, split = "")[[1]][10],
      "_tourist.xlsx"
    ),
    skip = 1
  ))
}

# 設定字體(不過好像沒什麼用QQ) (*1)
theme <-
  theme_get()
theme$text$family <- "STHeiti"
theme_set(theme)

# Pre-processing----------------------------------------------------------------
# Change names and add years and bind the 3 files to create "tourist" (*1)
revised_names <- c("Type", "Spots", "City", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total", "Counting_method")

names(tourist_18) <- revised_names
tourist_18$year <- 2018

names(tourist_19) <- revised_names
tourist_19$year <- 2019

names(tourist_20) <- revised_names
tourist_20$year <- 2020

tourist <- bind_rows(tourist_18, tourist_19, tourist_20) %>%
  select(Spots, City, year, Jan:Total)

# 移除缺失值的列 (*1)
tourist <- na.omit(tourist)
# 有些國家公園因為沒有登記人數資料就會因為NA被移除

# tourist$Spots # 這邊景點名稱還是混亂的
# 把Spots name中間的多餘空白清理乾淨(mutate出一行) (*1)
spots_zh <- c()
spots_en <- c()
for (spot in tourist$Spots) {
  spots_zh <- c(spots_zh, str_split(spot, "\r\n| +")[[1]][1])
}
for (spot in tourist$Spots) {
  spots_en <- c(spots_en,
                trimws(
                  paste(str_split(spot, "\r\n| +")[[1]][-1], collapse = " "),
                  which = "both",
                  whitespace = "[ \t\r\n]"
                ))
}
tourist <- tourist %>%
  mutate(Spots_zh = spots_zh,
         Spots_en = spots_en) %>%
  unite(spots, c("Spots_zh", "Spots_en"), sep = " ")
# 最後一個column為清理過後的景點名稱("spots") (*1)

# 把City名稱只保留中文部分(直接取代City), 並整理欄位排序 (*1)
cities <- c()
for (city in tourist$City) {
  cities <- c(cities, str_split(city, "\r\n| +")[[1]][1])
}
tourist$City <- cities
tourist <- tourist %>%
  select(City, spots, year:Total)
# 有跨縣市景點的項目取第一個縣市為代表, 保留中文部分方便之後分類 (*1)

unique(tourist$City) # 應該包含全台所有縣市 (*1)

# Data analysis-----------------------------------------------------------------
# 看哪一年的哪一縣市遊客量最多(排序) (*1)
tourist %>% 
  group_by(City, year) %>%
  summarise(num_spots = n(),
            total_tourist = sum(Total)) %>%
  arrange(desc(total_tourist))

# plots
# 對特定地點的觀光人數對月份作圖, 並以年份做facet_wrap (包成function) (*1)
show_plot <- function(place) {
  data <- tourist %>% filter(str_detect(spots, place)) %>%
    pivot_longer(cols = Jan:Dec,
                 names_to = "month",
                 values_to = "people") %>%
    select(!Total)
  cat(paste0("「", place, "」", "的搜尋結果:"), unique(data$spots), sep = "\n")
  data$month <-
    factor(as.character(data$month), levels = unique(as.character(data$month)))
  options(repr.plot.width= 20, repr.plot.height=7*length(unique(data$spots)))
  ggplot(data) +
    geom_bar(aes(month, people), stat = "identity", fill = "cornflowerblue") +
    facet_wrap(vars(spots, year), ncol = length(tourist__files)) +
    ggtitle(paste0("「", place, "」", "的搜尋結果"))
}

sample(tourist$spots, size = 30) # 可重複執行, 從裡面挑選想分析的景點

# Some examples
# show_plot("武陵農場")
# show_plot("國立臺灣美術館")
# show_plot("台北植物園")
# show_plot("日月潭") #有些風景區的名字會改變, 所以我用str_detect(), 只要給予足夠辨識資訊就能全部抓出
# show_plot("六福村")
# show_plot("野柳海洋")
# show_plot("國立科學工藝博物館")
show_plot("101")
# 從下圖我們看到, 基本上各個景點的2018跟2019都蠻相近的, 但在2020,2~4月人潮明顯降低(因為COVID-19), 但到了暑假大家又報復性出遊, 人潮飆高.
show_plot("動物園")
show_plot("國立臺灣.*博物館") # 也可用regex語法 (*1)
show_plot("遊樂園") # 遊樂園能明顯觀察出2020三四月人少,暑假人多的趨勢 (*1)
show_plot("Taiwan.+Center") # 也可用英文搜尋 (*1)

# sort_growth_rate function=====================================================
sort_growth_rate <-
  function(focused_months = 1:12, # 選擇月份
           tourists_threshold = 50000, # 設定選擇月份內總觀光人次的門檻
           which_year_to_look_up = 2019) { # 想觀察第n年到n+1年間的成長率
    
    # 將觀光景點以名字粗分
    new_tourist <- tourist %>% arrange(spots) 
    spot_id <- c()
    temp <- c()
    id <- 1
    
    # 同一景點在不同年份可能有不同名字, 因此創建一新變項spot_id來對真正的景點編號
    for (i in 1:(length(new_tourist$spots) - 1)) {
      spot_id <- c(spot_id, id)
      if (new_tourist$year[i] + 1 != new_tourist$year[i + 1] | # 利用相同景點年份相連的特性
          new_tourist$City[i] != new_tourist$City[i + 1]) { # 利用相同景點縣市相同的特性
        id <- id + 1
        temp <- c(temp, NA) # 明年的數據還沒出來, 因此無法計算成長率
      }
      else {
        temp <- c(temp, 1)
      }
    }
    
    spot_id <- c(spot_id, id)
    temp <- c(temp, NA)
    new_tourist$spot_id <- spot_id # 以spot_id對景點編號
    
    # 將表格的月份collapse成一新變數month, 方便稍後統計
    new_tourist <-
      new_tourist %>% select(City, spot_id, spots, year:Total) %>%
      pivot_longer(cols = Jan:Dec,
                   names_to = "month",
                   values_to = "people") %>%
      select(!Total)
    
    mon <-  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    # 留下指定月份的資料, 計算總觀光人次
    selected_new_tourist <- new_tourist %>%
      filter(month %in% mon[focused_months]) %>%
      group_by(spot_id, year) %>%
      summarise(total = sum(people))
    
    # 計算觀光人次年增量
    annual_growth <- c(diff(selected_new_tourist$total), NA) * temp
    
    selected_new_tourist$annual_growth <- annual_growth
    
    # 計算觀光人次年增率
    selected_new_tourist <- selected_new_tourist %>%
      mutate(annual_growth_rate = annual_growth / total)
    
    # 選定要觀察的年份, 設定總人次門檻, 並依照觀光人次年增率由高至低排列
    focused_new_tourist <- selected_new_tourist %>%
      filter(year == which_year_to_look_up, total >= tourists_threshold) %>%
      arrange(desc(annual_growth_rate))
    
    # 回原本dataframe抓spot_id對應的景點名稱
    get_name <- function(id) {
      return(paste0(unique(new_tourist$spots[new_tourist$spot_id == id]), collapse = " / "))
    }
    
    # 回原本dataframe抓spot_id對應景點所在縣市
    get_city <- function(id) {
      return(paste0(unique(new_tourist$City[new_tourist$spot_id == id]), collapse = " / "))
    }
    
    # 把觀光人次年增率以百分比形式呈現
    percent <- function(x, digits = 2, format = "f") {
      paste0(formatC(100 * x, format = format, digits = digits), "%")
    }
    
    # 最後整理dataframe並回傳
    focused_new_tourist <- focused_new_tourist %>%
      mutate(City = get_city(spot_id),
             spot_name = get_name(spot_id),
             this_year_total = total,
             next_year_total = total + annual_growth,
             growth_rate = percent(annual_growth_rate)) %>%
      ungroup() %>%
      select(City:next_year_total,
             annual_growth, 
             growth_rate) %>%
      drop_na()
    
    return(focused_new_tourist)
  }

sort_growth_rate(
  focused_months = 1:12, # 想看全年的成長率
  tourists_threshold = 1000000, # 指定全年觀光人次總和在一百萬以上的景點(避免一些奇怪的無名小景點跑出來)
  which_year_to_look_up = 2019 # 想看2019到2020的成長率
) %>% head(10)








# Growth Rate Top 10------------------------------------------------------------
sort_growth_rate(
  focused_months = 1:12, # 想看全年的成長率
  tourists_threshold = 1000000, # 指定全年觀光人次總和在一百萬以上的景點(避免一些奇怪的無名小景點跑出來)
  which_year_to_look_up = 2019 # 想看2019到2020的成長率
) %>% head(10)

sort_growth_rate(
  focused_months = 6:8, # 想看6~8月成長率
  tourists_threshold = 300000, # 指定6~8月觀光人次總和在三十萬以上的景點
  which_year_to_look_up = 2018 # 想看2018到2019的成長率
) %>% head(10)

sort_growth_rate(
  focused_months = 1:4, # 想看1~4月成長率
  tourists_threshold = 250000, # 指定1~4月觀光人次總和在二十五萬以上的景點
  which_year_to_look_up = 2019 # 想看2019到2020的成長率
) %>% head(10)

# 看108下學期 vs 107下學期, 總觀光人次總和(107下)在19萬以上的景點年增率排名
sort_growth_rate(3:6, 190000, 2019) %>% head(10)

# July-August
sort_growth_rate(7:8, 300000, 2019) %>% head(10)

# check_names function----------------------------------------------------------
# 檢查名字有沒有改變或混淆
check_names <- function() {
  new_tourist <- tourist %>% arrange(spots)
  spot_id <- c()
  temp <- c()
  id <- 1
  for (i in 1:(length(new_tourist$spots) - 1)) {
    spot_id <- c(spot_id, id)
    if (new_tourist$year[i] + 1 != new_tourist$year[i + 1] |
        new_tourist$City[i] != new_tourist$City[i + 1]) {
      id <- id + 1
      temp <- c(temp, NA) 
    } else {
      temp <- c(temp, 1)
    }
  }
  
  spot_id <- c(spot_id, id)
  temp <- c(temp, NA)
  new_tourist$spot_id <- spot_id 
  new_tourist <-
    new_tourist %>% select(City, spot_id, spots, year)
  
  spot_name <- c()
  spot_count <- c()
  dif_name <- c()
  cnt <- 1
  name <- new_tourist$spots[1]
  for (i in 2:length(new_tourist$spot_id)) {
    if (new_tourist$spot_id[i] == new_tourist$spot_id[i - 1]) {
      cnt <- cnt + 1
      name <- c(name, new_tourist$spots[i])
    } else{
      spot_count <- c(spot_count, cnt)
      cnt <- 1
      dif_name <- c(dif_name, length(unique(name)))
      spot_name <-
        c(spot_name, paste0(unique(name), collapse = " / "))
      name <- new_tourist$spots[i]
    }
  }
  return (tibble(
    name = spot_name,
    count = spot_count,
    n_name = dif_name
  ))
}

check_names() %>% filter(count == 3, n_name == 2)
# check_names() %>% filter(count == 1)
# 行政瑕疵: 一地多名

# 

















