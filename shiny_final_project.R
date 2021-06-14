library(shiny)
library(gapminder) # data
library(DT)
library(shinythemes)
library(ggplot2)
data <- source("r_final_project.R")

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
      textInput("place", "輸入觀光景點"),
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
      title = "成長率前十大景點 Google評論情緒分析"
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
  output$showplot <- renderPlot({
    show_plot(input$place)
  })
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
}

shinyApp(ui, server)
