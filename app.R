library(shiny)
library(shinyWidgets)
library(gapminder) # data
library(DT)
library(shinythemes)
library(ggplot2)
library(rsconnect)
source("system_setup.R")
data <- source("final_project.R", local = TRUE)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  h1("COVID-19-對台灣國旅的影響"),
  # Create a container for tab panels
  tabsetPanel(
      tabPanel(
      title = "介紹",
      h3("COVID-19除了影響日常生活，也影響國人旅遊型態"),
      h3("在本報告中，我們將蒐集各觀光景點在疫情前後的造訪人次資料、Google評論"),
      h3("以上述資料找出各觀光景點成長幅度，並以情緒分析找出影響成長幅度多寡的原因"),
      h3("\n"),
      h5("組員：莊頌恩、盧妍蓁、陳郁婕、朱嘉棟")
    ),
      
    # Create a "Plot" tab
    tabPanel(
      title = "各景點每月遊歷人數（2018 - 2020）",
      searchInput("place",
                  label = "輸入一個觀光景點(包含片面字詞 / regex)",
                  placeholder = "例: 日月潭, 墾丁, 遊樂園, Zoo",
                  btnSearch = icon("search"),
                  btnReset = icon("remove"),
                  width = "450px"
                  ),
      plotOutput("showplot")
      ), 
    # Create "Table" tab
    tabPanel(
      title = "各景點人數成長率",
      selectInput("period", "請選擇時間區段", choices = c("全年度", "暑假（7-8月)", "春假（3-4月)")),
      selectInput("year", "請選擇年份", choices = c("2018-2019", "2019-2020")),
      DT::dataTableOutput("growth")
    ),
    tabPanel(
      title = "成長率前十大景點 Google評論情緒分析",
      selectInput("site", "請選擇景點", choices = c("北港朝天宮", "虎頭山風景特定區", 
                                               "日月潭風景區", "新竹漁港", 
                                               "鹿野高臺", "六福村主題遊樂園", 
                                               "溪頭自然教育園區", "國立海洋科技博物館", 
                                               "國立科學工藝博物館")),
      plotOutput("sentimentplot")
    ),
    tabPanel(
      title = "結論 / 資料來源",
      h4("在疫情爆發年（2020），台灣實施嚴格邊境管制，以致國際觀光客無法自由往來，使國內3-4月"), 
      h4("各觀光景點造訪人次大幅流失。然而隨國內疫情平穩及政府推出的安心旅遊補助政策，國內各大"), 
      h4("景點除了恢復觀光熱潮，更相較前兩年呈現大幅的觀光成長。其中室外景點的成長幅度又高於室"),
      h4("內景點。")
    )
  )
)

server <- function(input, output) {
  output$showplot <- renderPlot(
    height = reactive(get_height(place = input$place)*400 + 50),
    width = 1500,
    {
      req(input$place)
      show_plot(place = input$place)
    }
  )
  
  growth_data <-  reactive({if (input$period == "全年度") {
    if (input$year == "2018-2019") {
      sort_growth_rate(1:12, 1000000, 2018)
    } else {sort_growth_rate(1:12, 1000000, 2019)}
  } else if (input$period == "暑假（7-8月)" ) {
    if (input$year == "2018-2019") {
      sort_growth_rate(7:8, 300000, 2018)
    } else {sort_growth_rate(7:8, 300000, 2019)}
  } else {
    if (input$period == "春假（3-4月)") {
      if (input$year == "2018-2019") {
        sort_growth_rate(3:4, 150000, 2018)
      } else {sort_growth_rate(3:4, 150000, 2019)}
    }
  }})
  output$growth <- DT::renderDataTable({
    data <- growth_data()
    data
  })
  
  output$sentimentplot <- renderPlot(
    height = 700,
    width = 1300,
    {
      sentiment_plot(input$site)
      })
}

shinyApp(ui, server)
