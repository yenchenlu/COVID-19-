library(readxl)
library(tidyverse)
library(repr)

# system("gdown --id 13kAhQTWmP1yiIvVKqcUv5nPZ_jUqdtOr")
# system("gdown --id 171iXYT6gZZ2JKVsn8iVHPMpvR0jx1sUS")
# system("gdown --id 1Z4NSWjwI6MQJS_i8F8kbVjIznDYu-vme")

# 設定路徑
# setwd("~/Documents/R_group1_final_project")

# 有哪幾個年份的要讀進來
tourist__files <- c("tourist_18", "tourist_19", "tourist_20")


# 設定字體
theme <-
    theme_get()
theme$text$family <- "PingFangTC-Thin"
theme_set(theme)

# 讀檔案進來
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


# Change names and add years and bind the 3 files to create "tourist"
revised_names <- c("Type","Spots","City","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Total","Counting_method")
names(tourist_18) <- revised_names
tourist_18$year <- 2018
names(tourist_19) <- revised_names
tourist_19$year <- 2019
names(tourist_20) <- revised_names
tourist_20$year <- 2020
tourist <- bind_rows(tourist_18, tourist_19, tourist_20) %>%
    select(Spots, City, year, Jan:Total)
# 若新加檔案進來, 只要新增code到以上為止


# 移除缺失值的列
tourist <- na.omit(tourist)


# 把景點名稱中間的多餘空白清理乾淨(mutate出一行)
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


# 把City名稱只保留中文部分(直接取代City), 並整理欄位排序
cities <- c()
for (city in tourist$City) {
    cities <- c(cities, str_split(city, "\r\n| +")[[1]][1])
}
tourist$City <- cities
tourist <- tourist %>%
    select(City, spots, year:Total)


# 看各縣市的總遊客量
tourist %>%
    group_by(City, year) %>%
    summarise(num_spots = n(),
              total_tourist = sum(Total)) %>%
    arrange(desc(total_tourist))


# 對特定地點的觀光人數對月份作圖, 並以年份做facet_wrap (包成function)
show_plot <- function(place, save = TRUE, width = 10) {
    data <- tourist %>% filter(str_detect(spots, place)) %>%
        pivot_longer(cols = Jan:Dec,
                     names_to = "month",
                     values_to = "people") %>%
        select(!Total)
    cat(paste0("「", place, "」", "的搜尋結果:"), unique(data$spots), sep = "\n")
    data$month <-
        factor(as.character(data$month), levels = unique(as.character(data$month)))
    # options(repr.plot.width= 20, repr.plot.height=7*length(unique(data$spots)))
    p <- ggplot(data) +
        geom_bar(aes(month, people), stat = "identity", fill = "cornflowerblue") +
        facet_wrap(vars(spots, year), ncol = length(tourist__files)) +
        ggtitle(paste0("「", place, "」", "的搜尋結果"))
    print(p)
    if (save) {
        ggsave(
            "plot.png",
            width = width,
            height = sum(c(3.5 * length(
                unique(data$spots)
            ), 0.5)),
            dpi = 320,
            device = "png"
        )
    }
}


# sample(tourist$spots, size = 30)
# options(repr.plot.width= 20, repr.plot.height=9)
show_plot("國立臺灣.*博物館", save = T, width = 12) #可自行調整width(有些地名太長塞不下)
ggsave(
    "showplot.png",
    width = 10,
    height = 7,
    dpi = 320,
    device = "png"
)






# 看年成長率
sort_growth_rate <-
    function(focused_months = 1:12,             # 選擇月份
             tourists_threshold = 50000,        # 設定選擇月份內總觀光人次的門檻
             which_year_to_look_up = 2019) {    # 想觀察第n年到n+1年間的成長率
        
        # 將觀光景點以名字粗分
        new_tourist <- tourist %>% arrange(spots)
        spot_id <- c()
        temp <- c()
        id <- 1
        
        # 同一景點在不同年份可能有不同名字, 因此創建一新變項spot_id來對真正的景點編號
        for (i in 1:(length(new_tourist$spots) - 1)) {
            spot_id <- c(spot_id, id)
            if (new_tourist$year[i] + 1 != new_tourist$year[i + 1] |
                # 利用相同景點年份相連的特性
                new_tourist$City[i] != new_tourist$City[i + 1]) {
                # 利用相同景點縣市相同的特性
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
        
        mon <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        
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
        
        # 回原本dataframe抓spot_id對應的景點名稱所在縣市
        get_city <- function(id) {
            return(paste0(unique(new_tourist$City[new_tourist$spot_id == id]), collapse = " / "))
        }
        
        # 把觀光人次年增率以百分比形式呈現
        percent <- function(x,
                            digits = 2,
                            format = "f") {
            paste0(formatC(100 * x, format = format, digits = digits), "%")
        }
        
        # 最後整理dataframe並回傳
        focused_new_tourist <- focused_new_tourist %>%
            mutate(
                City = get_city(spot_id),
                spot_name = get_name(spot_id),
                this_year_total = total,
                next_year_total = total + annual_growth,
                growth_rate = percent(annual_growth_rate)
            ) %>%
            ungroup() %>%
            select(City:next_year_total,
                   annual_growth,
                   growth_rate) %>%
            drop_na()
        
        return(focused_new_tourist)
    }

table1 <- sort_growth_rate(
    focused_months = 1:12,
    tourists_threshold = 1000000,
    which_year_to_look_up = 2019
)

# 檢查名字有沒有改變或混淆
check_names <- function() {
    new_tourist <- tourist %>% arrange(spots)
    spot_id <- c()
    temp <- c()
    id <- 1
    for (i in 1:(length(new_tourist$spots) - 1)) {
        spot_id <- c(spot_id, id)
        if (new_tourist$year[i] + 1 != new_tourist$year[i + 1] |
            # 利用相同景點年份相連的特性
            new_tourist$City[i] != new_tourist$City[i + 1]) {
            # 利用相同景點縣市相同的特性
            id <- id + 1
            temp <- c(temp, NA) # 明年的數據還沒出來, 因此無法計算成長率
        } else {
            temp <- c(temp, 1)
        }
    }
    
    spot_id <- c(spot_id, id)
    temp <- c(temp, NA)
    new_tourist$spot_id <- spot_id # 以spot_id對景點編號
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

(check_names() %>% filter(n_name == 2))$name





