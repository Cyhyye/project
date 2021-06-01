# Load packages 
library(shiny)
library(ggmap)
library(tidyverse)
library(magrittr)
library(ghibli)

# Load data
load("data/shiny_data.RData")

# 지도시각화 함수
get_map = function(data, fill.var){
  ggplot(data) + 
    geom_polygon(data = data,
                 aes(x=long, y=lat, group=group, fill = get(fill.var)), 
                 color = 'black', 
                 alpha = 0.5) +
    theme(plot.title = element_text(face = 'bold', size = 20),
          legend.position = c(0.15, 0.8),
          legend.background = element_blank()) + 
    scale_fill_gradient(low = "lightyellow", 
                        high = "orange", 
                        space = "Lab", 
                        guide = "colourbar", 
                        name = fill.var)
}

# barplot 함수
get_topbar = function(data, arrange_var, n_top){
  data %>% arrange(desc(get(arrange_var))) %>% 
    slice(1:n_top) %>% 
    ggplot() +
    geom_bar(aes(reorder(행정동, -get(arrange_var)),y = get(arrange_var), 
                 fill = reorder(행정동, get(arrange_var))), stat = 'identity') +
    ylab(arrange_var) +
    xlab('행정동') +
    theme_bw() +
    theme(legend.position = 'none') 
}


# User interface
ui <- fluidPage(
  titlePanel("서울시 주차장 현황"),
  
  sidebarLayout(
    sidebarPanel(
      img(src = "106048_99642_3724.jpg", height = 100, width = 200),
      helpText("서울시 주차장 분포"),
      selectInput("parking",
                  label = "운영 분류 선택",
                  choices = c("공영", "민영", "합계"),
                  selected = "합계"),
      checkboxInput("park_bar_plot",
                    label = "상위 행정동 표시", value = FALSE),
      conditionalPanel(condition = "input.park_bar_plot == true",
                       sliderInput('parking_n_top', '행정동 개수', value = 13, min = 5, max = 20)
      )
    ),
    mainPanel(plotOutput("park_map"))
  ),
  fluidRow(
    column(12, plotOutput("park_bar_plot"))
  ),
  
  hr(),
  
  titlePanel("서울시 인구 분포"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("서울시 인구 정보별 분포"),
      selectInput("population",
                  label = "인구정보 선택",
                  choices = c("주거인구", "직장인구", "생활인구"),
                  selected = "주거인구"),
      conditionalPanel(condition = "input.population == '생활인구'",
                       selectInput('holiday', '휴일 여부', c('평일', '휴일'), selected = '평일'),
                       selectInput('time', '시간대', c('생활시간', '비생활시간'), selected = '생활시간')
      ),
      checkboxInput("pop_bar_plot",
                    label = "상위 행정동 표시", value = FALSE),
      conditionalPanel(condition = "input.pop_bar_plot == true",
                       sliderInput('pop_n_top', '행정동 개수', value = 13, min = 5, max = 20)
      )
    ),
    mainPanel(plotOutput("pop_map"))
  ),
  
  fluidRow(
    column(12, plotOutput("pop_bar_plot"))
  )
  
  
)

# Server logic
server <- function(input, output){
  
  populationInput <- reactive({
    if(input$population != '생활인구'){
      return(input$population)
    } else {
      return(paste(input$holiday, input$time, sep = '_'))
    }
  })
  
  
  output$pop_map <- renderPlot({
    get_map(pop_bound_dat, populationInput())
  })
  
  output$pop_bar_plot <- renderPlot({
    if(input$pop_bar_plot){
      get_topbar(pop_dat, populationInput(), input$pop_n_top) +
        scale_fill_manual(values = ghibli_palette('PonyoMedium', n = input$pop_n_top, type = 'continuous'))
    }
  })
  
  output$park_map <- renderPlot({
    get_map(parking_bound_dat, input$parking)
  })
  
  output$park_bar_plot <- renderPlot({
    if(input$park_bar_plot){
      get_topbar(parking_dat, input$parking, input$parking_n_top) +
        scale_fill_manual(values = ghibli_palette('PonyoMedium', n = input$parking_n_top, type = 'continuous'))
    }
  })
}

# Run app
shinyApp(ui, server)
