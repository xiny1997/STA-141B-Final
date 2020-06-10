
library(shiny)
library(nycflights13)
library(ggplot2)
library(tidyverse)
library(httr)
library(tidyverse)
library(jsonlite)
library(scales)
library(shinyWidgets)
library(ggthemes)
library(plotly)
country <- GET("https://api.covid19api.com/summary")
json <- content(country, as = "text")
country <- fromJSON(json)
country <- country %>%
  .$Countries %>% arrange(Country)
###ui

ui <- fluidPage(
  setBackgroundColor(color = c("#F7FBFF", "#2171B5"),
                     gradient = "radial",
                     direction = c("top", "left")),
  
  mainPanel(tabsetPanel(
    tabPanel("COVID-19 Worldwide Situation", 
             h4("Current time: ",textOutput("currentTime", container = span)),
             h4("Global Situation"),tabsetPanel(
               tabPanel("Daily Information",plotOutput("pie1"),tableOutput("global_summary1")),
               tabPanel("Overall Information",plotOutput("pie2"),tableOutput("global_summary2")))
    ),
    
    tabPanel("Cases by Country & Date",tableOutput("monthly"),fluid = TRUE, 
             sidebarLayout(sidebarPanel(fluid = TRUE,
                                        selectInput("country", "Country",c("-",country$Country)),
                                        dateRangeInput("date", "Date",
                                                       start = NULL, end = NULL, min = "2020/03/05", max = Sys.Date() -1 ,format = "yyyy/mm/dd",
                                                       separator = " to ")
             ),position = "right",
             mainPanel(h4("Cases Over Time"),tabsetPanel(id = "tabs",
                                                         tabPanel("Confirmed Information", value = "confirmed",plotOutput("scatterplot1"),tableOutput("country_summary1")),
                                                         tabPanel("Deaths Information",value = "deaths",plotOutput("scatterplot2"),tableOutput("country_summary2")),
                                                         tabPanel("Recovered Information",value = "recovered",plotOutput("scatterplot3"),tableOutput("country_summary3"))),
                       textOutput("Country_Summary"),tableOutput("countrysum"),plotOutput("pie3")
             )))
    
  )))


###Server
server <- function(input, output, session) {
  
  output$currentTime <- renderText({
    
    invalidateLater(1000, session)
    
    format(Sys.time())
  })
  
  summary <- reactive({
    GET("https://api.covid19api.com/summary") %>%
      content(., as = "text") %>%
      fromJSON()
  })
  
  
  country_code <- reactive({
    code <- "-"
    if (input$country != "-") {
      code <- country$Slug[which(country$Country == input$country)]
    }
    code
  })
  
  covid <- reactive({
    req(input$country != "-")
    GET(str_glue("https://api.covid19api.com/dayone/country/{input_country}/status/{input_status}",
                 input_country = country_code(),
                 input_status = as.character(input$tabs))) %>% 
      content(., as = "text") %>%
      fromJSON() %>% 
      mutate(Month_Date = substr(Date,6,10))})
  
  
  covid_time <- reactive({
    req(input$country != "-")
    GET(str_glue("https://api.covid19api.com/total/country/{input_country}/status/{input_status}?from={date1}T00:00:00Z&to={date2}T00:00:00Z",
                 input_country = country_code(),
                 input_status = as.character(input$tabs),
                 date1 = input$date[1],
                 date2 = input$date[2])) %>%
      content(., as = "text") %>%
      fromJSON()%>% 
      mutate(Month_Date = substr(Date,6,10))
  }) 
  
  country_summary <- reactive({
    summary()$Countries %>%
      filter(Country == input$country) %>%
      select(Country, NewConfirmed ,NewDeaths,NewRecovered,TotalConfirmed,TotalDeaths,TotalRecovered) %>%
      mutate(TotalCases = TotalConfirmed - TotalDeaths - TotalRecovered,
             DeathRate = percent(TotalDeaths/TotalConfirmed))
  })
  
  
  global_summary <- reactive({
    tibble(NewConfirmed = summary()$Global$NewConfirmed,
           NewDeaths = summary()$Global$NewDeaths,
           NewRecovered = summary()$Global$NewRecovered,
           TotalConfirmed = summary()$Global$TotalConfirmed,
           TotalDeaths = summary()$Global$TotalDeaths,
           TotalRecovered = summary()$Global$TotalRecovered,
    ) %>% 
      mutate(TotalCases = TotalConfirmed - TotalDeaths - TotalRecovered,
             DeathRate = percent(TotalDeaths/TotalConfirmed))
  })
  
  
  output$scatterplot1 <- renderPlot({
    if (is.null(input$date)) {ggplot(data = covid(), aes(x = as.factor(Month_Date), y = Cases)) + 
        geom_point(colour = "orange") +geom_line(aes(group =1),colour = "orange")+xlab("Date") + geom_rangeframe() + 
        theme_tufte()} 
    else{ggplot(data = covid_time(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point(colour = "orange") +geom_line(aes(group =1),colour = "orange") + geom_rangeframe() + 
        theme_tufte()  +xlab("Date") }
  })
  
  output$scatterplot2 <- renderPlot({
    if (is.null(input$date)) {ggplot(data = covid(), aes(x = as.factor(Month_Date), y = Cases)) + 
        geom_point() +geom_line(aes(group =1))+xlab("Date") + geom_rangeframe() + 
        theme_tufte()} 
    else{ggplot(data = covid_time(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point() +geom_line(aes(group =1))+ geom_rangeframe() + 
        theme_tufte()  +xlab("Date") }
  })
  
  output$scatterplot3 <- renderPlot({
    if (is.null(input$date) == TRUE) {ggplot(data = covid(), aes(x = as.factor(Month_Date), y = Cases)) + 
        geom_point(colour = "green") +geom_line(aes(group =1),colour = "green") +xlab("Date") +   ggtitle(paste("Total", input$tabs,"in",input$country,"in 2020"))+ geom_rangeframe() + 
        theme_tufte()} 
    else{ggplot(data = covid_time(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point(colour = "green") +geom_line(aes(group =1),colour = "green") + geom_rangeframe() + 
        theme_tufte()  +xlab("Date") }
  })
  
  
  
  output$global_summary1 <- renderTable({
    df1<- global_summary() %>%
      select(NewConfirmed,NewDeaths,NewRecovered)
    df1 %>%
      set_names(c('Daily Confirmed','Daily Death','Daily Recovered'))
  })
  
  output$global_summary2 <- renderTable({
    df2 <- global_summary() %>%
      select(TotalConfirmed,TotalCases,TotalDeaths,TotalRecovered,DeathRate)
    df2 %>%
      set_names(c('Total Confirmed','Total Cases','Total Deaths','Total Recovered','Total DeathRate'))
  })
  
  output$pie1 <- renderPlot({df <- data.frame(
    x <- c(global_summary()$NewConfirmed,global_summary()$NewDeaths,
           global_summary()$NewRecovered),
    labels <- c("Daily Confirmed","Daily Death","Daily Recovered"))
  piepercent<- round(100*x/sum(x), 1)
  pie(df$x,labels = piepercent, main = "Daily COV-ID19 Pie Chart",col = c("orange", "black", "green"))
  legend("topright", c("Daily Confirmed","Daily Death","Daily Recovered"), cex = 0.8,
         fill = c("orange", "black", "green"))
  })
  
  output$pie2 <- renderPlot({df <-
    data.frame(x <- c(global_summary()$TotalCases,global_summary()$TotalDeaths,
                      global_summary()$TotalRecovered),
               labels <- c("Total Cases","Total Death","Total Recovered"))
    piepercent<- round(100*x/sum(x), 1)
    pie(df$x,labels = piepercent, main = "Total COV-ID19 Pie Chart",col = c("orange", "black", "green"))
    legend("topright", c("Total Cases","Total Death","Total Recovered"), cex = 0.8,
           fill = c("orange", "black", "green"))
  })
  
  output$pie3 <- renderPlot({
    req(input$country != "-")
    df <- data.frame(x <- c(country_summary()$TotalCases,country_summary()$TotalDeaths,
                      country_summary()$TotalRecovered),
               labels <- c("Total Confirmed","Total Death","Total Recovered"))
    piepercent<- round(100*x/sum(x), 1)
    pie(df$x,labels = piepercent, main = paste("Total COV-ID19 Cases in",input$country," Pie Chart"),col = c("orange", "black", "green"))
    legend("topright", c("Total Cases","Total Death","Total Recovered"), cex = 0.8,
           fill = c("orange", "black", "green"))
  })
  
  output$countrysum <- renderTable({
    req(input$country != "-")
   df3<-  country_summary() %>%
      select(NewConfirmed ,NewDeaths,NewRecovered,TotalConfirmed,TotalCases,TotalDeaths,TotalRecovered,DeathRate)
   df3 %>%
      set_names(c('Daily Confirmed','Daily Death','Daily Recovered','Total Confirmed','Total Cases','Total Deaths','Total Recovered','Total Death Rate'))
  })
  
  output$Country_Summary <- renderText({
    req(input$country != "-")
    paste("Summary Statistics in ",input$country)
  })
  
}


shinyApp(ui = ui, server = server)
