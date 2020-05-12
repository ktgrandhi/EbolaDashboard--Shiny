if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, shiny, shinydashboard, dplyr, leaflet, plotly,DT, tmap, tibble)


ebolaData <- read.csv("ebola_2014_2016.csv")
head(ebolaData)
nrow(ebolaData)
summary(ebolaData)

ebolaData$Country <-gsub("United States of America","United States",ebolaData$Country)
task1 <- subset(ebolaData,select = c("Country", "Date","No..of.confirmed.cases","No..of.confirmed.deaths"))
names(task1)<- c("Country", "DateValue", "CasesConfirmed", "DeathsConfirmed")
nrow(task1)
summary(task1[!complete.cases(task1),])
subset1 <- na.omit(task1)
nrow(subset1)

subset1$DateValue<- as.Date(subset1$DateValue,"%m/%d/%Y")
dateStats <- aggregate(subset1[c("CasesConfirmed","DeathsConfirmed")], by=list(DateValue = subset1$DateValue), FUN=sum)
dateStats <- dateStats[order(dateStats$DateValue),]
head(dateStats)

plot1<- ggplotly(ggplot(dateStats,aes(DateValue))
            +geom_line(data = dateStats, aes(y = CasesConfirmed, colour = "Cases"))
            +geom_line(data = dateStats, aes(y = DeathsConfirmed,colour = "Deaths"))
            +ylab("Number of cases")
            +xlab("Date")
            +scale_x_date(date_breaks = "12 weeks",date_labels = "%m/%y")
    )
plot1 <- plot1 %>% layout(legend = list(orientation = "v", x = 0.7, y = 0))

subset2 <- subset1 %>% group_by(Country = subset1$Country) %>% summarise(TotalCases = sum(CasesConfirmed)) 
subset2 <- subset2[order(subset2$TotalCases, decreasing = TRUE), ]


country_grouped <- subset2 %>%
  group_split(Country) %>%
  setNames(subset2 %>% 
             group_keys(Country) %>% 
             pull(1))

country_grouped
data(World)

body <- dashboardBody(
    fluidRow(
        box(width = 4 ,
            height = '100%',
           # textOutput("totalCases"),
            dateRangeInput('dateRange',
                           label = 'Select Range:',
                           start = min(subset1$DateValue), end = max(subset1$DateValue),
                           format = "mm/dd/yyyy",separator = '-',
                           min = min(subset1$DateValue), max = max(subset1$DateValue),
            ),
            fluidRow(
                column(width = 4, h5("TotalCases")),
                column(width = 4, h5("Fatality Rate")),
                column(width = 4, h5("Recovery Rate")),

            ),
            fluidRow(
                column(width = 4, textOutput("totalCases")),
                column(width = 4, textOutput("recoveryRate")),
                column(width = 4, textOutput("fatalityRate")),

            ),
            plotlyOutput("plot1",width = "100%")
            
        ),
        box(title = "Map view of affected places", solidHeader = TRUE,
            width = 8, 
            leafletOutput(outputId= 'geo_map', width = '100%'),
            tableOutput("fatalityRateByCountry")
       
        ),
       
    ))


# Define UI for application that draws a histogram
ui <- dashboardPage(
        dashboardHeader(title = "Ebola..."),
        dashboardSidebar(disable = TRUE),
        body,
        tags$head(tags$style("#totalCases{color: red;
                                 font-size:130%;
                                 font-style: bold;
                             }",
                             "#fatalityRate{color: red;
                                font-size:130%;
                                font-style: bold;
                             }",
                             "#recoveryRate{color: red;
                                font-size:130%;
                                font-style: bold;
                             }"
                             
        )
        )
        
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    #FatalityRate
    output$fatalityRate  <- renderText({
        datesRange <- unlist(strsplit(as.character(input$dateRange),split = " "))
        filteredData <- subset1[subset1$DateValue >= datesRange[1] & subset1$DateValue <= datesRange[2],]
        sum(filteredData$DeathsConfirmed)/sum(filteredData$CasesConfirmed)
    })
    #RecoveryRate
    output$recoveryRate  <- renderText({
        datesRange <- unlist(strsplit(as.character(input$dateRange),split = " "))
        filteredData <- subset1[subset1$DateValue >= datesRange[1] & subset1$DateValue <= datesRange[2],]
        (sum(filteredData$CasesConfirmed)-sum(filteredData$DeathsConfirmed))/sum(filteredData$CasesConfirmed)
    })
    #Fatality Rate grouped by country
    output$fatalityRateByCountry <-renderTable({
        datesRange <- unlist(strsplit(as.character(input$dateRange),split = " "))
        filteredData <- subset1[subset1$DateValue >= datesRange[1] & subset1$DateValue <= datesRange[2],]
        filteredData2 <-subset(filteredData,select = c("Country", "CasesConfirmed", "DeathsConfirmed"))
        result <- filteredData2  %>% group_by(Country) %>%summarise(Cases=sum(CasesConfirmed),fatalityRate = sum(DeathsConfirmed)/sum(CasesConfirmed))
        result <- result[result$fatalityRate>0, ]
        result <- result[order(result$Cases,decreasing = TRUE),]
        rownames(result)<-result$Country
        result$Country <- NULL
        tResult <- as.data.frame(t(as.matrix(result)))
        row.names(tResult) <-c("Cases","FatalityRate")
        rownames_to_column(tResult, var="Country")
    })
    
    #Total confirmed cases
    output$totalCases<-renderText({
        datesRange <- unlist(strsplit(as.character(input$dateRange),split = " "))
        filteredData <- subset1[subset1$DateValue >= datesRange[1] & subset1$DateValue <= datesRange[2],]
        sum(filteredData$CasesConfirmed)
        })
    
    #Cases over Time plot
    output$plot1 <-renderPlotly({
        datesRange <- unlist(strsplit(as.character(input$dateRange),split = " "))
        filteredData <- dateStats[dateStats$DateValue >= datesRange[1] & dateStats$DateValue <= datesRange[2],]
        plot1<- ggplotly(ggplot(filteredData,aes(DateValue))
            +geom_line(data = filteredData, aes(y = CasesConfirmed, colour = "Cases"))
            +geom_line(data = filteredData, aes(y = DeathsConfirmed,colour = "Deaths"))
            +ylab("Number of cases")
            +xlab("Date")
            +scale_x_date(date_breaks = "12 weeks",date_labels = "%m/%y"))
        plot1 <- plot1 %>% layout(legend = list(orientation = "v", x = 0.7, y = 0))
    
        })
    
    
    #Map
    output$geo_map <- renderLeaflet({
    World$name <- as.character(World$name)
    mapData <- World %>% inner_join(subset2, by = c("name" = "Country"))
    mapData$stat <- paste0('Country:', mapData$name)
    tmap_leaflet(tm_shape(mapData) + 
                     tm_fill(col = 'TotalCases', palette = 'Blues', id = 'stat',
                             title = 'Total Cases', alpha = 0.7, n = 6) + 
                     tm_borders() )
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)

