#Shiny app to look at making webpage
#To open excel in R
library(openxlsx)
# To plot using ggplot2
library(ggplot2)
#To plot side by side or on top of each other
library(gridExtra)
#To use date_break functinoallity
library(scales)
library(lubridate)
library(rkt)
library(EnvStats)
library(zoo)
library(dplyr)
library(Kendall)
library(boot)
library(magrittr)
library(ggpubr)
library(ggthemes)
library(plotly)
library(psych)
library(shiny)

#path of spreadsheet
dbPath1 <- "TMDL_Stream_2010-2017_take2.xlsx"

# load the workbook

wb <- loadWorkbook(dbPath1)

#load the worksheets
SARU_Excel <-  read.xlsx(dbPath1, "SARU", detectDates = T)
LS_1_Excel <-  read.xlsx(dbPath1, "LS_1", detectDates = T)
JO_1_Excel <-  read.xlsx(dbPath1, "JO_1", detectDates = T)
PM_1_Excel <-  read.xlsx(dbPath1, "PM_1", detectDates = T)
NCLD_Excel <-  read.xlsx(dbPath1, "NCLD", detectDates = T)
HC_1_Excel <-  read.xlsx(dbPath1, "HC_1", detectDates = T)
HC_2_Excel <-  read.xlsx(dbPath1, "HC_2", detectDates = T)
NC_1_Excel <-  read.xlsx(dbPath1, "NC_1", detectDates = T)
WC_1_Excel <-  read.xlsx(dbPath1, "WC_1", detectDates = T)

# Combine Worksheets
AllCountsAmbient <- rbind(SARU_Excel, LS_1_Excel, JO_1_Excel, PM_1_Excel,
                           NCLD_Excel, HC_1_Excel,HC_2_Excel, NC_1_Excel, WC_1_Excel)
#Creat Year and Month Vectors

AllCountsAmbient$Month <- month(AllCountsAmbient$Date, label = TRUE, abbr = TRUE)

#get objects in correct form
AllCountsAmbient$pH <- as.numeric(AllCountsAmbient$pH)
AllCountsAmbient$Turbidity <- as.numeric(AllCountsAmbient$Turbidity)

AllCountsAmbient$Temp.Type <- ifelse(AllCountsAmbient$TEMP < 16, "below", "above")
AllCountsAmbient$DO.Type <- ifelse(AllCountsAmbient$DO > 9.5, "below", "above")
AllCountsAmbient$Turbidity.Type <- ifelse(AllCountsAmbient$Turbidity < 4, "below", "above")

f <- list(
  family = "helvetica",
  size = 18,
  color = "Black"
)
y <- list(
  title = "Summer Dissolved Oxygen (mg/L)",
  titlefont = f
)
x <- list(
  title = "Temperature (Degrees Celcius)",
  titlefont = f
)

plot_ly(subset(AllCountsAmbient, Season %in% "Summer"), y = ~DO, color = ~Site, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y)

plot_ly(AllCountsAmbient, y = ~DO, x = ~TEMP, color = ~Site, type = "scatter", jitter = 0.3) %>% 
  layout(yaxis = y) %>%
  layout(xaxis = x)



plot_ly(AllCountsAmbient, y = ~Turbidity, x = ~Site, color = ~Precipitation, type = "scatter", jitter = 0.3) %>% 
  layout(yaxis = y) %>%
  layout(xaxis = x)
dev.off()
#to add dot plot geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .2, fill="red")

#plotly

pturb <- ggplot(AllCountsAmbient, aes(Site, Turbidity)) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, color = "cyan4") +
  geom_boxplot(fill = "gray94", width = 0.5, color = "cyan4")  +
  labs(title = "e. Turbidity (NTU)", subtitle=NULL, y=NULL, x=NULL) + scale_y_log10(breaks=c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 150)) +
  theme(axis.title.y = element_text(size=7), title = element_text(size = 15), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_few() + theme(axis.title.y = element_text(size=7), title = element_text(size = 8), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                      axis.text.y = element_text(size = 10), legend.title = element_text(size=18), legend.text = element_text(size=15)) + coord_flip()

pturb <- ggplotly(pturb)

pturb

#shiny

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)


#Shiny Example Temp
# Define UI
AllCountsAmbient2 <- AllCountsAmbient[, c("Date", "TEMP", "DO", "Site")]

ui <- fluidPage(
  fluidRow(
    column(width = 4,
           plotOutput("plot1", height = 300,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           )
    )
  ),
  fluidRow(
    column(width = 15,
           h4("Clicked Points"),
           verbatimTextOutput("click_info")
    ),
    column(width = 6,
           h4("Highlighted Points"),
           verbatimTextOutput("brush_info")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(AllCountsAmbient2, aes(Date, TEMP, group = "Site")) + geom_point() + geom_hline(yintercept = 16, color = "red")
  })
  
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(AllCountsAmbient2, input$plot1_click, addDist = TRUE)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(AllCountsAmbient2, input$plot1_brush)
  })
}

shinyApp(ui, server)

#summer boxplots





ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("event")
)

server <- function(input, output) {
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    plot_ly(subset(AllCountsAmbient, Season %in% "Summer"), y = ~DO, color = ~Site, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
      layout( yaxis = y)
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}

shinyApp(ui, server)
