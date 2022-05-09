library(fpp3)
library(plotly)
library(ggplot2)
library(seasonal)
library(dplyr)
library(shinydashboard)
library(quantmod)
library(shiny)
library(shinyWidgets)

#Data path
file_path <- "multiTimeline.csv"

g_trends <- read.csv(file_path, skip = 2)

#Name columns
names(g_trends) <- c("Month","Interest")


g_trends$Month <- yearmonth(g_trends$Month)
g_trends$Interest <- as.numeric(ifelse(g_trends$Interest == "<1", 0, g_trends$Interest))


g_trends <- tsibble(g_trends)
autoplot(g_trends)  +
    geom_vline(xintercept = as.numeric(as.Date("2017-12-21")),linetype = 4, colour = "red") +
    geom_vline(xintercept = as.numeric(as.Date("2017-9-26")),linetype = 4, colour = "red") +
    geom_vline(xintercept = as.numeric(as.Date("2020-3-10")),linetype = 4, colour = "red") +
    ggtitle("Interest in Battle Royales Over Time") -> NormalPlot

g_trends %>% gg_season(Interest, labels = "both") + labs(y = "% Interest", title = 'Seasonal Plot') -> SeasonalPlot

g_trends %>% ACF(Interest, lag_max = 20) %>% autoplot() + labs(title = "Autocorrelation Plot") -> AutocorrelationPlot

lambda <- g_trends %>% features(Interest, features = guerrero) %>% pull(lambda_guerrero)
g_trends %>% autoplot(box_cox(Interest, lambda)) + labs(y = "", title = "Transformed Interest/Decomposition Plot") -> DecompPlot


#Final

autoplot(Interest)

TSLM(Interest~trend())


#fit <- g_trends %>% select(Interest) %>% filter(Quarter > yearmonth("2018 Jan")) autoplot(fit) %>%
#fit_trends <- g_trends %>% model(TSLM(Interest~ trend() + season()))
#report(fit_trends)
#(g_trends %>% autoplot(Interest) + 
#    autolayer(g_trends,Interest,colour = "red")) %>% ggplotly()
#fit

predict <- g_trends %>% model(MEAN(Interest)) 
predict_fc <- predict %>% forecast(h = 15)
predict_fc %>% autoplot(g_trends, level = NULL) + labs(y = "% Interest", title = 'Mean Forecast') -> MeanForecast

predict2 <- g_trends %>% model(NAIVE(Interest)) 
predict2_fc <- predict2 %>% forecast(h = 15)
predict2_fc %>% autoplot(g_trends, level = NULL) + labs(y = "% Interest", title = 'NaiveForecast') -> NaiveForecast

predict3 <- g_trends %>% model(SNAIVE(Interest~lag("year")))
predict_fc3 <- predict3 %>% forecast(h = 15)
predict_fc3 %>% autoplot(g_trends, level = NULL) + labs(y = "% Interest", title = 'Seasonal Naive Forecast') -> SeasonalNaiveForecast

predict4 <- g_trends %>% model(RW(Interest~drift()))
predict_fc4 <- predict4 %>% forecast(h = 15)
predict_fc4 %>% autoplot(g_trends, level = NULL) + labs(y = "% Interest", title = 'Drift Forecast') -> DriftForecast

predict5 <- g_trends %>% model(ETS(Interest~error("A") + trend("A") + season("N")))
predict_fc5 <- predict5 %>% forecast(h = 15)
predict_fc5 %>% autoplot(g_trends, level = NULL) + labs(y = "% Interest", title = 'Holts Forecast') -> HoltsForecast

predict6 <- g_trends %>% model(ETS(Interest~error("A") + trend("A") + season("A")))
predict_fc6 <- predict6 %>% forecast(h = 15)
predict_fc6 %>% autoplot(g_trends, level = NULL) + labs(y = "% Interest", title = 'Holts/Winters Forecast') -> HoltsWintersForecast


predict7 <- g_trends %>% model(ARIMA(Interest~pdq(2,1,0)))
predict_fc7 <- predict7 %>% forecast(h = 15)
predict_fc7 %>% autoplot(g_trends, level = NULL) + labs(y = "% Interest", title = 'ARIMA Forecast') -> ManualARIMAForecast



NormalPlot
SeasonalPlot
AutocorrelationPlot
DecompPlot
MeanForecast
NaiveForecast
SeasonalNaiveForecast
DriftForecast
HoltsForecast
HoltsWintersForecast
ManualARIMAForecast





ui <- dashboardPage(skin = 'black',
                    dashboardHeader(title = "Hunter Governale BAS 475 Final project", titleWidth = 500),
                    dashboardSidebar(width = 250,
                                     sidebarMenu(
                                         menuItem("How to Use", tabname = "feature1", 
                                                  icon = icon("th")),
                                         menuItem("Interest Over Time", tabname = "feature2", 
                                                  icon = icon("dashboard")),
                                         menuItem("Seasonal Interest", tabname = "feature3", 
                                                  icon = icon("dashboard")),
                                         menuItem("Autocorrelated Interest", tabname = "feature4", 
                                                  icon = icon("dashboard")),
                                         menuItem("Decomposition Plot", tabname = "feature5", 
                                                  icon = icon("dashboard")),
                                         menuItem("Mean Forecast", tabname = "feature6", 
                                                  icon = icon("dashboard")),
                                         menuItem("Naive Forecast", tabname = "feature7", 
                                                  icon = icon("dashboard")),
                                         menuItem("Seasonal Naive Forecast", tabname = "feature8", 
                                                  icon = icon("dashboard")),
                                         menuItem("Drift Forecast", tabname = "feature9", 
                                                  icon = icon("dashboard")),
                                         menuItem("Holts Forecast", tabname = "feature10", 
                                                  icon = icon("dashboard")),
                                         menuItem("Holts and Winters Forecast", tabname = "feature11", 
                                                  icon = icon("dashboard")),
                                         menuItem("ARIMA Forecast", tabname = "feature12", 
                                                  icon = icon("dashboard")))
                    ),
                    
dashboardBody(
    tabItems(
    #first tab
    tabItem(tabname = "feature1",
        fluidPage(
            titlePanel("How to Use the Following ShinyApp"),
                mainPanel(
                    h4("This ShinyApp will allow you to look at various time series of Battle Royale interest"),
                    h4("- After navigating to the 'Interest Over Time' tab you will be presented with a time series showing the percent interest of battle royales since Jan 2004"),
                    h4("- After navigating to the 'Seasonal interest' tab you will be presented a seasonal plot of battle royale interest"),
                    h4("- After navigating to the 'Autocorrelated interest' tab you will be presented an autocorrelated plot of battle royale interest"),
                    h4("- After navigating to the 'Decomposition interest' tab you will be presented a decomposition plot plot of battle royale interest"),
                    h4("- After navigating to the 'Mean Forecast' tab you will be presented a forecast of battle royale interest using the mean model"),
                    h4("- After navigating to the 'Naive Forecast' tab you will be presented a forecast of battle royale interest using the Naive model"),
                    h4("- After navigating to the 'Seasonal Naive Forecast' tab you will be presented a forecast of battle royale interest using the Seasonal Naive model"),
                    h4("- After navigating to the 'Drift Forecast' tab you will be presented a forecast of battle royale interest using the Drift model"),
                    h4("- After navigating to the 'Holts Forecast' tab you will be presented a forecast of battle royale interest using the Holts model"),
                    h4("- After navigating to the 'Holts and Winters Forecast' tab you will be presented a forecast of battle royale interest using the Holts and Winters model"),
                    h4("- After navigating to the 'ARIMA Forecast' tab you will be presented a forecast of battle royale interest using the ARIMA model")
                                        )
                                    )
                            ),
                            #second tab
                            tabItem(tabname = "feature2",
                                    fluidPage(
                                        box(title = "Interest in Battle Royales Over Time", width = 12, solidHeader = TRUE, status = "primary",
                                            plotlyOutput("NormalPlot", height = 250)))),
                            #third tab
                            tabItem(tabname = "feature3",
                                    fluidPage(
                                        box(title = "Seasonal Interest in Battle Royales", width = 12, solidHeader = TRUE, status = "primary",
                                            plotlyOutput("SeasonalPlot", height = 250)))),
                            #fourth tab
                            tabItem(tabname = "feature4",
                                    fluidPage(
                                        box(title = "Autocorrelated Interest in Battle Royales", width = 12, solidHeader = TRUE, status = "primary",
                                            plotlyOutput("AutocorrelationPlot", height = 250)))),
                            #fifth tab
                            tabItem(tabname = "feature5",
                                    fluidPage(
                                        box(title = "Transformed Plot of Battle Royale Interest", width = 12, solidHeader = TRUE, status = "primary",
                                            plotlyOutput("DecompPlot", height = 250)))))),
                            #sixth tab
                            tabItem(tabname = "feature6",
                                fluidPage(
                                    box(title = "Forecasted Interest using Mean Model", width = 12, solidHeader = TRUE, status = "primary",
                                        plotlyOutput("MeanForecast", height = 250)))),
                            #seventh tab
                            tabItem(tabname = "feature7",
                                fluidPage(
                                    box(title = "Forecasted Interest using Naive Model", width = 12, solidHeader = TRUE, status = "primary",
                                        plotlyOutput("NaiveForecast", height = 250)))),
                            #eight tab
                            tabItem(tabname = "feature8",
                                fluidPage(
                                    box(title = "Forecasted Interest using Seasonal Naive Model", width = 12, solidHeader = TRUE, status = "primary",
                                        plotlyOutput("SeasonalNaiveForecast", height = 250)))),
                            #ninth tab
                            tabItem(tabname = "feature9",
                                fluidPage(
                                    box(title = "Forecasted Interest using Drift Model", width = 12, solidHeader = TRUE, status = "primary",
                                        plotlyOutput("DriftForecast", height = 250)))),
                            #tenth tab
                            tabItem(tabname = "feature10",
                                fluidPage(
                                    box(title = "Forecasted Interest using Holts Model", width = 12, solidHeader = TRUE, status = "primary",
                                        plotlyOutput("HoltsForecast", height = 250)))),
                            #eleventh tab
                            tabItem(tabname = "feature11",
                                fluidPage(
                                    box(title = "Forecasted Interest using Holts and Winters Model", width = 12, solidHeader = TRUE, status = "primary",
                                        plotlyOutput("HoltsWintersForecast", height = 250)))),
                            #twelth tab
                            tabItem(tabname = "feature12",
                                fluidPage(
                                    box(title = "Forecasted Interest using ARIMA Model", width = 12, solidHeader = TRUE, status = "primary",
                                        plotlyOutput("DriftForecast", height = 250))))
                        
                        
)



server <- function(input, output, session) {
    output$g_trends <- renderPrint({input$Interest})
    output$NormalPlot <- renderPlotly(ggplotly(NormalPlot))
    output$SeasonalPlot <- renderPlot(ggplotly(SeasonalPlot))
    output$AutocorrelationPlot <- renderPlotly(ggplotly(AutocorrelationPlot))
    output$DecompPlot <- renderPlotly(ggplotly(DecompPlot))
    output$MeanForecast <- renderPlotly(ggplotly(MeanForecast))
    output$NaiveForecast <- renderPlotly(ggplotly(NaiveForecast))
    output$SeasonalNaiveForecast <- renderPlotly(ggplotly(SeasonalNaiveForecast))
    output$DriftForecast <- renderPlotly(ggplotly(DriftForecast))
    output$DriftForecast <- renderPlotly(ggplotly(HoltsForecast))
    output$DriftForecast <- renderPlotly(ggplotly(HoltsWintersForecast))
    output$DriftForecast <- renderPlotly(ggplotly(ARIMAForecast))
}




shinyApp(ui = ui, server = server)
