#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#
library(shiny)
library(rsconnect)
library(ggplot2)
library(readxl)
library(dplyr)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(leaflet)
library(plotly)
library(shinydashboard)
library(htmlwidgets)
library(htmltools)
library(tidyr)
# Define UI for application that draws a histogram
ui <-
  dashboardPage(
    dashboardHeader(title = "Drugs in Victoria"),
    dashboardSidebar(sidebarMenu(
      menuItem("Overview and Demographic", tabName = "dashboard"),
      menuItem("Local Government Area", tabName = "dashboard2"),
      menuItem("Drug Types", tabName = "dashboard3")
    )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard2",
                column(width = 12,
                       box(status = "primary", solidHeader = TRUE,
                           title = "Drug Crime Rates by LGA",
                           column(width = 8, box(leafletOutput("lgamap", height="660px"),
                                                 div(HTML("<p>From the map above, we can see that
MELBOURNE, NOTHERN GRAMPIANS and ARARAT are the top three LGAs with the highest
drug crime rates.
 GOLDEN PLAINS, NILLUMBIK and MANNINGHAM are the three
LGAs with the lowest drug crime rates.</p>"), style = "text-align: center;margin: 10px"),
                                                 div(HTML("<p><b>Data References</b></p>
 <p>Crime Statistic Agency. (2022). Data Tables LGA Criminal
Incidents Year Ending December 2022. https://www.crimestatistics.vic.gov.au/crimestatistics/latest-victorian-crime-data/download-data</p>")),
                                                 solidHeader = TRUE, width = 12)),
                           div(HTML("<p>The line graph below shows the drug crime rate (per 100,000
population) for the selected LGAs compared to the average drug crime rate in Victoria, and
the Bar chart shows the Top 5 surburbs with the highest drug crime rate in the selected
LGA.</p>"), style = "text-align: center;margin: 10px"),
                           box(solidHeader = TRUE,
                               plotlyOutput("lgayear", height="240px"), width = 4),
                           box(solidHeader = TRUE,
                               plotlyOutput("top5LGA", height="240px"), width = 4),
                           box(solidHeader = TRUE,
                               selectInput("lgayear",
                                           label = "Local Government Area",
                                           choices = c("ALPINE", "ARARAT", "BALLARAT", "BANYULE", "BASS
COAST", "BAW BAW", "BAYSIDE", "BENALLA", "BOROONDARA", "BRIMBANK", "BULOKE",
                                                       "CAMPASPE", "CARDINIA", "CASEY", "CENTRAL GOLDFIELDS", "COLAC-OTWAY",
                                                       "CORANGAMITE", "DAREBIN", "EAST GIPPSLAND", "FRANKSTON", "GANNAWARRA",
                                                       "GLEN EIRA", "GLENELG", "GOLDEN PLAINS", "GREATER BENDIGO", "GREATER
DANDENONG", "GREATER GEELONG", "GREATER SHEPPARTON", "HEPBURN",
                                                       "HINDMARSH", "HOBSONS BAY", "HORSHAM", "HUME", "INDIGO", "KINGSTON", "KNOX",
                                                       "LATROBE", "LODDON", "MACEDON RANGES", "MANNINGHAM", "MANSFIELD",
                                                       "MARIBYRNONG", "MAROONDAH", "MELBOURNE", "MELTON", "MERRI-BEK",
                                                       "MILDURA", "MITCHELL", "MOIRA", "MONASH", "MOONEE VALLEY", "MOORABOOL",
                                                       "MORNINGTON PENINSULA", "MOUNT ALEXANDER", "MOYNE", "MURRINDINDI",
                                                       "NILLUMBIK", "NORTHERN GRAMPIANS", "PORT PHILLIP", "PYRENEES", "QUEENSCLIFFE",
                                                       "SOUTH GIPPSLAND", "SOUTHERN GRAMPIANS", "STONNINGTON", "STRATHBOGIE",
                                                       "SURF COAST", "SWAN HILL", "TOWONG", "WANGARATTA", "WARRNAMBOOL",
                                                       "WELLINGTON", "WEST WIMMERA", "WHITEHORSE", "WHITTLESEA", "WODONGA",
                                                       "WYNDHAM", "YARRA", "YARRA RANGES", "YARRIAMBIACK"),
                                           selected = NULL,
                                           multiple = FALSE,
                                           selectize = FALSE,
                                           size = 5), width = 4)
                           ,width = 12)
                ),
                
        ),
        tabItem(tabName = "dashboard",
                column(width = 8,
                       box( title = "Overview", status = "primary", solidHeader = TRUE,
                            box(solidHeader = TRUE,
                                tabsetPanel(
                                  tabPanel(title = "Drug Incidents Overall",
                                           div(style= "margin: 5px;"),
                                           plotlyOutput("vicyear2", height="260px")),
                                  tabPanel(title = "Drug Incidents by Offence Subdivision",
                                           div(style= "margin: 5px;"),
                                           plotlyOutput("vicyear", height="260px"))
                                ),div(HTML("<p>From the line graph above, we can see that the drug crime
rate is on an upward trend from 2013 and peaks in 2020 with a record 18,697 incidents
recorded in Victoria. The drug crime rate then began to decline in 2021. The trends are
similar for all offence types.</p>"), style = "text-align: center;margin: 10px"),
                                width = 12),
                            box(solidHeader = TRUE,
                                plotlyOutput("porpo", height="210px"),div(HTML("<p>The most
common drug offence in Victoria is Drug use and Possession, then followed by Drug
dealing and Trafficking.</p>"), style = "text-align: center"),
                                width = 6),
                            
                            box(solidHeader = TRUE,
                                plotlyOutput("top5loc",
                                             height="210px"),div(HTML("<p>Street/Lane/Footpath are the most common locations
where drug crimes occur, then followed by house.</p>"), style = "text-align: center"),
                                width = 6),
                            div(HTML("<p><b>Data References</b></p>
 <p>Crime Statistic Agency. (2022). Data Tables LGA Criminal
Incidents Year Ending December 2022. https://www.crimestatistics.vic.gov.au/crimestatistics/latest-victorian-crime-data/download-data</p>
 <p>Crime Statistic Agency. (2022). Data Tables Alleged Offender
Incidents Visualisation Year Ending December 2022.
https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crimedata/download-data</p>")),
                            width = 12)
                ),
                column(width = 4,
                       box(title = "Demographic", status = "primary",
                           box(solidHeader = TRUE, plotlyOutput("Genporpo",
                                                                height="250px"),div(HTML("<p>The rate of drug-related offenses committed by males is
over three times higher than that of Females.</p>"), style = "text-align: center"),width =
                                 12),
                           box(solidHeader = TRUE, plotlyOutput("age", height="360px"),width = 12),
                           div(HTML("<p>Among all age groups, people aged 20-24 has the highest drug crime rate
with 57,456 incidents recorded in Victoria. The distribution of incidents recorded vs age
group looks like a normal distribution, it was on an upward trend from Age 10-14, and it
peaked at 57,456 in 20-24 Age group, then began to decline.</p>"), style = "text-align:
center"), solidHeader = TRUE, width = 12)
                )
        ),
        tabItem(tabName = "dashboard3",
                column(width = 8,
                       box(title = "Drug Crime Rates by LGA and Drug Types", status = "primary",
                           solidHeader = TRUE,
                           leafletOutput("drugmap", height="495px"), box( solidHeader = TRUE,
                                                                          sidebarPanel(
                                                                            sliderInput("year", label = "Year", min = 2006, sep="",
                                                                                        max = 2016, value = 2016,
                                                                                        animate = animationOptions(interval = 500, loop = TRUE))
                                                                            , width = 6), box(selectInput("drugtype",
                                                                                                          label = "Drug Type",
                                                                                                          choices = c("Amphetamine", "Cannabis", "Ecstasy",
                                                                                                                      "Heroin", "Methamphetamine", "Other drug"),
                                                                                                          selected = NULL,
                                                                                                          multiple = FALSE,
                                                                                                          selectize = FALSE,
                                                                                                          size = 5), width = 6 , solidHeader = TRUE), width = 12
                           ), div(HTML("<p>The map above shows drug crime rates (per 100,000
population) by LGA and Drug Types over years.</p>"), style = "text-align: center"),
                           div(HTML("<p><b>Data References</b></p>
 <p>Crime Statistic Agency. (2022). Data tables: Rate and number of
offences by LGA and drug type. https://www.crimestatistics.vic.gov.au/research-andevaluation/publications/drug-and-alcohol-use-and-crime/what-drug-types-droveincreases</p>"))
                           ,width = 12)
                ),
                column(width = 4,
                       box(title = "Drug Types Overview", solidHeader = TRUE,
                           box(title = "Drug Types and Crime Rates Over the Years",
                               tabsetPanel(
                                 tabPanel(title = "Heatmap", plotlyOutput("heatmap", height="270px")),
                                 tabPanel(title = "Line chart", plotlyOutput("drugline", height="270px"))
                               ),div(HTML("<p>From the Heapmap and Linechart, we can see that
Cannabis, Methamphetamine, Ecstasy and Other Drug have an increasing trend from year
2006 to year 2016, whereas Amphetamine was on an upward trend from year 2006 to
2014, and it began to decline in 2015.</p>"), style = "text-align: center"), solidHeader =
                                 TRUE, width = 12),
                           box(title = "Proportions of Crime Rate for different Drug Types",
                               plotlyOutput("typepor", height="200px"), div(HTML("<p>The most
common drug type in Victoria is Cannabis (54.3%), then followed by Anphetamine (16.9%)
and Methamphetamine (10.3%). These 3 drug types take up more than 80% of total drug
crimes.</p>"), style = "text-align: center"),solidHeader = TRUE, width = 12),
                           status = "primary", width = 12),
                )
        )
      )
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$vicyear <- renderPlotly({
    
    plot_ly(data = filter(IncidentsVICsub, 'Offence Subdivision' == "C10 Drug dealing and
trafficking"), x = ~Year, y = ~'Incidents Recorded',
            type = "scatter", mode = "bars", name = "C10 Drug dealing and trafficking", marker
            = list(color = "#ea5545"), line = list(color = "#ea5545")) %>%
      add_trace(data = filter(IncidentsVICsub, 'Offence Subdivision' == "C20 Cultivate or
manufacture drugs"), x = ~Year, y = ~'Incidents Recorded',
                type = "scatter", mode = "bars", name = "C20 Cultivate or manufacture drugs",
                marker = list(color = "#b33dc6"), line = list(color = "#b33dc6")) %>%
      add_trace(data = filter(IncidentsVICsub, 'Offence Subdivision' == "C30 Drug use and
possession"), x = ~Year, y = ~'Incidents Recorded',
                type = "scatter", mode = "bars", name = "C30 Drug use and possession", marker
                = list(color = "#ef9b20"), line = list(color = '#ef9b20')) %>%
      add_trace(data = filter(IncidentsVICsub, 'Offence Subdivision' == "C90 Other drug
offences"), x = ~Year, y = ~'Incidents Recorded',
                type = "scatter", mode = "bars", name = "C90 Other drug offences", marker =
                  list(color = "#87bc45"), line = list(color = '#87bc45')) %>%
      layout(title = "Drug Incidents Recorded in Victoria Over Years by Offence Subdivision",
             yaxis = list(zeroline = FALSE, title = "Incidents Recorded"),
             xaxis = list(zeroline = FALSE, title = "Year", rangeslider = list(type = "date")))
    
  })
  
  output$vicyear2 <- renderPlotly({
    
    plot_ly(data = IncidentsVICall, x = ~Year, y = ~sum_incident,type = "scatter", mode =
              "bars", name = "VIC") %>%
      layout(title = "Drug incidents Recorded in Victoria Over Years",
             yaxis = list(zeroline = FALSE, title = "Incidents Recorded", range = c(0,20000)),
             xaxis = list(zeroline = FALSE, title = "Year", rangeslider = list(type = "date"))) %>%
      layout(annotations = a1)
    
  })
  
  output$top5loc <- renderPlotly({
    
    plot_ly(DemoVICloc5, x = ~'Location Group', y = ~'Incidents Recorded') %>%
      add_trace(type = "bar") %>%
      layout(
        title = "Top 5 Locations for Drug Offences",
        yaxis = list(zeroline = FALSE, title = "Incidents Recorded"),
        xaxis = list(zeroline = FALSE, title = "Location", categoryorder = "total descending",
                     tickangle=30, tickfont = list(size=9))) %>%
      layout(annotations = a2)
    
  })
  
  output$porpo <- renderPlotly({
    
    plot_ly(data = IncidentsPor, labels = ~'Offence Subdivision', values = ~'Incidents
            Recorded', sort = FALSE, type = 'pie',
            marker = list(colors = c("#ea5545", "#b33dc6", "#ef9b20", "#87bc45")), rotation = -
              90) %>%
      layout(title = 'Drug Incidents Recorded by Offence Subdivision',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$Genporpo <- renderPlotly({
    
    plot_ly(data = DemoVICGen, labels = ~Sex, values = ~'Alleged Offender Incidents', type
            = 'pie',
            marker = list(colors = c("#f46a9b","#27aeef", "#ede15b"))) %>%
      layout(title = 'Alleged Drug Offenders by Sex',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$age <- renderPlotly({
    
    plot_ly(DemoVICAge, x = ~'Age Group', y = ~People, type = 'bar', name = 'People',
            marker = list(color = "#ede15b")) %>%
      add_trace(y = ~Females, name = 'Females', marker = list(color = "#f46a9b")) %>%
      add_trace(y = ~Males, name = 'Males', marker = list(color = "#27aeef")) %>%
      layout(title = "Alleged Drug Offenders by Age Group and Sex",
             yaxis = list(title = 'Incidents Recorded'),
             xaxis = list(title = 'Age Group', tickangle=45),barmode = 'stack') %>%
      layout(annotations = a3)
    
  })
  
  output$lgamap <- renderLeaflet({
    leaflet(lga_allerged) %>%
      setView(lng = 144.94, lat = -37.84, zoom = 7) %>% addTiles() %>% addPolygons() %>%
      addPolygons(
        fillColor = ~pal(mean_incident),
        weight = 2,
        opacity = 1,
        color = ~pal(mean_incident),
        dashArray = "3",
        fillOpacity = 0.7) %>% addPolygons(
          fillColor = ~pal(mean_incident),
          stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
          color = ~pal(mean_incident),
          weight = 2,
          opacity = 1,
          dashArray = "3",
          highlight = highlightOptions(
            weight = 1,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
      addLegend(pal = pal,
                values = ~mean_incident,
                opacity = 1, title = "Rate",
                position = "bottomright") %>%
      addControl(title, position = "topright")
  })
  
  output$lgayear <- renderPlotly({
    
    plot_ly(data = filter(IncidentsLGAyear, lga_name == input$lgayear), x = ~Year, y = ~'Rate
            per 100,000 population',
            type = "scatter", mode = "bars", name = "LGA", marker = list(color = "#353535"),
            line = list(color = '#353535')) %>%
      add_trace(data = IncidentsVIC, x = ~Year, y = ~'Rate per 100,000 population', type =
                  "scatter", mode = "bars", name = "VIC",
                marker = list(color = "#f9c74f"), line = list(color = '#f9c74f')) %>%
      layout(title = paste(input$lgayear, "vs VIC average Drug Crime Rate"),
             yaxis = list(zeroline = FALSE, title = "Rate per 100k population", range = c(0,250)),
             xaxis = list(zeroline = FALSE, title = "Year", rangeslider = list(type = "date")))
  })
  
  output$top5LGA <- renderPlotly({
    
    plot_ly(filter(IncidentsOSLGA, lga_name == input$lgayear) %>%
              arrange(desc(sum_incident)) %>% head(5),
            x = ~'Suburb/Town Name' , y = ~sum_incident, marker = list(color =
                                                                         "#9e0059")) %>%
      
      add_trace(type = "bar") %>%
      
      layout(
        title = paste("Top 5 Drug Crime Rate Surburbs in", input$lgayear),
        yaxis = list(zeroline = FALSE, title = "Crime rate"),
        xaxis = list(zeroline = FALSE, title = "Suburb/Town Name", categoryorder = "total
descending", tickangle=45, tickfont = list(size=13)))})
  
  output$drugmap <- renderLeaflet({
    
    test <- drug_type[drug_type$year == input$year & drug_type$drug_type ==
                        input$drugtype,]
    drug_typemap <- sp::merge(lga_simp, test, by="lga_name")
    pal2 <- colorNumeric(
      "Reds",
      domain = c(-1:drug_typeMax[drug_typeMax$drug_type == input$drugtype,]$maxi+1)
    )
    labels2 <- sprintf(
      "<strong>%s</strong><br/>%g",
      drug_typemap$lga_name,
      drug_typemap$'rate of offences'
    ) %>% lapply(htmltools::HTML)
    leaflet(drug_typemap) %>%
      setView(lng = 144.3467, lat = -36.7948, zoom = 7) %>% addTiles() %>%
      addPolygons() %>% addPolygons(
        fillColor = ~pal2('rate of offences'),
        weight = 2,
        opacity = 1,
        color = ~pal2('rate of offences'),
        dashArray = "3",
        fillOpacity = 0.7) %>% addPolygons(
          fillColor = ~pal2('rate of offences'),
          stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
          color = ~pal2('rate of offences'),
          weight = 2,
          opacity = 1,
          dashArray = "3",
          highlight = highlightOptions(
            weight = 1,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels2,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
      addLegend(pal = pal2,
                values = ~c(0:drug_typeMax[drug_typeMax$drug_type ==
                                             input$drugtype,]$maxi+1 ),
                opacity = 1, title = "Rate",
                position = "bottomright")
    
  })
  
  output$heatmap <- renderPlotly({
    
    plot_ly(
      data = drug_typeHeat,
      x = ~year, y = ~drug_type,
      z = ~Incidents, type = "heatmap", colors = "Reds") %>%
      layout(
        yaxis = list(title = 'Drug Type'),
        xaxis = list(title = 'Year', tickangle=45))
    
  })
  
  output$drugline <- renderPlotly({
    
    plot_ly(data = filter(drug_typeHeat, drug_type == "Amphetamine"), x = ~year, y =
              ~Incidents, type = "scatter", mode = "bars",
            marker = list(color = c("#F9D030")), line = list(color = c("#F9D030")), name =
              "Amphetamine") %>%
      add_trace(data = filter(drug_typeHeat, drug_type == "Cannabis"), x = ~year, y =
                  ~Incidents, type = "scatter", mode = "bars",
                marker = list(color = c("#D773A2")), line = list(color = c("#D773A2")), name =
                  "Cannabis") %>%
      add_trace(data = filter(drug_typeHeat, drug_type == "Ecstasy"), x = ~year, y =
                  ~Incidents, type = "scatter", mode = "bars",
                marker = list(color = c("#07BB9C")), line = list(color = c("#07BB9C")), name =
                  "Ecstasy") %>%
      add_trace(data = filter(drug_typeHeat, drug_type == "Heroin"), x = ~year, y =
                  ~Incidents, type = "scatter", mode = "bars",
                marker = list(color = c("#5E376D")), line = list(color = c("#5E376D")), name =
                  "Heroin") %>%
      add_trace(data = filter(drug_typeHeat, drug_type == "Methamphetamine"), x = ~year,
                y = ~Incidents, type = "scatter", mode = "bars",
                marker = list(color = c("#B8EE30")), line = list(color = c("#B8EE30")), name =
                  "Methamphetamine") %>%
      add_trace(data = filter(drug_typeHeat, drug_type == "Other drug"), x = ~year, y =
                  ~Incidents, type = "scatter", mode = "bars",
                marker = list(color = c("#0000FF")), line = list(color = c("#0000FF")), name =
                  "Other drug") %>%
      layout(yaxis = list(zeroline = FALSE, title = "Incidents Recorded"),
             xaxis = list(zeroline = FALSE, title = "Year", rangeslider = list(type = "date")))
    
  })
  
  output$typepor <- renderPlotly({
    
    plot_ly(data = drug_typePor, labels = ~drug_type, values = ~'Incidents Recorded', type =
              'pie', sort = FALSE, rotation = -180,
            marker = list(colors = c("#F9D030","#D773A2",
                                     "#07BB9C","#5E376D","#B8EE30","#0000FF"))) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
}
#IncidentsLGA <-
read_excel("Data_Tables_LGA_Criminal_Incidents_Year_Ending_December_2022.xlsx",
           sheet = "Table 02")
#IncidentsVIC <- IncidentsLGA[IncidentsLGA$'Offence Division' == 'C Drug offences',]
#IncidentsVIC$'Offence Division' <- IncidentsVIC$'Offence Division' %>% as.factor()
#IncidentsVIC$Year <- IncidentsVIC$Year %>% as.factor()
#IncidentsVICsub <- IncidentsVIC %>% group_by(Year, 'Offence Subdivision') %>%
summarise('Incidents Recorded' = sum('Incidents Recorded', na.rm = TRUE))
#writexl::write_xlsx(IncidentsVICsub, "IncidentsVICsub.xlsx")
IncidentsVICsub <- read_excel("IncidentsVICsub.xlsx")
IncidentsVICall <- IncidentsVICsub %>% group_by(Year) %>% summarise(sum_incident =
                                                                      sum('Incidents Recorded', na.rm = TRUE))
IncidentsVICall$Year <- IncidentsVICall$Year %>% as.numeric()
a1 <- list(
  x = IncidentsVICall[which.max(IncidentsVICall$sum_incident), ]$Year,
  y = IncidentsVICall[which.max(IncidentsVICall$sum_incident), ]$sum_incident,
  text = paste("(Year: ",IncidentsVICall[which.max(IncidentsVICall$sum_incident), ]$Year, ",
Incidents: ",IncidentsVICall[which.max(IncidentsVICall$sum_incident), ]$sum_incident,
               ")" , sep = ""),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 50,
  ay = -20
)
# Pie chart
IncidentsPor <- IncidentsVICsub %>% group_by('Offence Subdivision') %>%
  summarise('Incidents Recorded' = sum('Incidents Recorded', na.rm = TRUE))
# Demographic
DemoVIC <-
  read_excel("Data_Tables_Alleged_Offender_Incidents_Visualisation_Year_Ending_Decemb
er_2022.xlsx", sheet = "Table 04")
DemoVIC <- DemoVIC[DemoVIC$'Offence Division' == 'C Drug offences',]
DemoVIC$Sex <- DemoVIC$Sex %>% as.factor()
DemoVIC$'Alleged Offender Incidents'
DemoVICGen <- DemoVIC %>% group_by(Sex) %>% summarise('Alleged Offender
                                                       Incidents' = sum('Alleged Offender Incidents', na.rm = TRUE))
fig <- plot_ly(data = DemoVICGen, labels = ~Sex, values = ~'Alleged Offender Incidents',
               type = 'pie') %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig
# Stacked bar chart
DemoVICAge <- DemoVIC %>% group_by('Age Group', Sex) %>% summarise('Alleged
                                                                    Offender Incidents' = sum('Alleged Offender Incidents', na.rm = TRUE))
DemoVICAge <- spread(DemoVICAge, key = "Sex", value = 'Alleged Offender Incidents')
DemoVIC$'Age Group' <- DemoVIC$'Age Group' %>% as.factor()
DemoVICAge <- DemoVICAge[!DemoVICAge$'Age Group' == 'Total',]
DemoVICAge <- DemoVICAge %>% mutate(., total = Males + Females + People)
a3 <- list(
  x = DemoVICAge[which.max(DemoVICAge$total), ]$'Age Group',
  y = DemoVICAge[which.max(DemoVICAge$total), ]$total,
  text = paste("(Year: ",DemoVICAge[which.max(DemoVICAge$total), ]$'Age Group', ",
Incidents: ",DemoVICAge[which.max(DemoVICAge$total), ]$total, ")" , sep = ""),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 50,
  ay = -20
)
fig <- plot_ly(DemoVICAge, x = ~'Age Group', y = ~People, type = 'bar', name = 'People',
               marker = list(color = "#ede15b")) %>%
  add_trace(y = ~Females, name = 'Females', marker = list(color = "#f46a9b")) %>%
  add_trace(y = ~Males, name = 'Males', marker = list(color = "#27aeef")) %>%
  layout(title = "Alleged Drug Offenders by Age Group and Sex",
         yaxis = list(title = 'Total Incidents'),
         xaxis = list(title = 'Age Group', tickangle=45),barmode = 'stack') %>%
  layout(annotations = a3)
fig
# Top 5 locations
#DemoVICloc <-
read_excel("Data_Tables_Criminal_Incidents_Visualisation_Year_Ending_December_2022.
xlsx", sheet = "Table 02")
#DemoVICloc <- DemoVICloc[DemoVICloc$'Offence Division' == 'C Drug offences',]
#DemoVICloc$'Location Group' <- DemoVICloc$'Location Group' %>% as.factor()
#DemoVICloc <- DemoVICloc %>% group_by('Location Group') %>% summarise('Incidents
Recorded' = sum('Incidents Recorded', na.rm = TRUE))
#DemoVICloc5 <- DemoVICloc %>% arrange(desc('Incidents Recorded')) %>% head(5)
#writexl::write_xlsx(DemoVICloc5, "DemoVICloc5.xlsx")
DemoVICloc5 <- read_excel("DemoVICloc5.xlsx")
a2 <- list(
  x = DemoVICloc5[which.max(DemoVICloc5$'Incidents Recorded'), ]$'Location Group',
  y = DemoVICloc5[which.max(DemoVICloc5$'Incidents Recorded'), ]$'Incidents Recorded',
  text = paste("(Location: ",DemoVICloc5[which.max(DemoVICloc5$'Incidents
                                                   Recorded'), ]$'Location Group', ", Incidents:
",DemoVICloc5[which.max(DemoVICloc5$'Incidents Recorded'), ]$'Incidents Recorded',
                                                   ")" , sep = ""),
                                         xref = "x",
                                         yref = "y",
                                         showarrow = TRUE,
                                         arrowhead = 7,
                                         ax = 90,
                                         ay = -20
  )
  fig <- plot_ly(DemoVICloc5, x = ~'Location Group', y = ~'Incidents Recorded') %>%
    add_trace(type = "bar") %>%
    layout(
      title = "Top 5 Locations for Drug Offences",
      yaxis = list(zeroline = FALSE, title = "Incidents Recorded"),
      xaxis = list(zeroline = FALSE, title = "Location", categoryorder = "total descending",
                   tickangle=30, tickfont = list(size=9))) %>%
    layout(annotations = a2)
  fig
  # Leaflet Map
  IncidentsLGA <-
    read_excel("Data_Tables_LGA_Criminal_Incidents_Year_Ending_December_2022.xlsx",
               sheet = "Table 02")
  colnames(IncidentsLGA)[4] <- 'lga_name'
  IncidentsLGA$lga_name <- IncidentsLGA$lga_name %>% as.factor()
  IncidentsLGA$'Offence Division' <- IncidentsLGA$'Offence Division' %>% as.factor()
  IncidentsLGA <- IncidentsLGA[IncidentsLGA$'Offence Division' == 'C Drug offences',]
  IncidentsLGAMean <- IncidentsLGA %>% group_by(lga_name) %>%
    summarise(mean_incident = mean('LGA Rate per 100,000 population', na.rm = TRUE))
  IncidentsLGAMean$lga_name <- toupper(IncidentsLGAMean$lga_name)
  IncidentsLGAMean$lga_name[IncidentsLGAMean$lga_name == 'COLAC-OTWAY'] <- 'COLAC
OTWAY'
  vic.lga.shp <- readShapeSpatial("vmlite_lga_cm.shp")
  lga_simp <- gSimplify(vic.lga.shp, tol = .001, topologyPreserve=TRUE)
  lga_simp <- SpatialPolygonsDataFrame(lga_simp, data=vic.lga.shp@data)
  lga_allerged <- sp::merge(lga_simp, IncidentsLGAMean, by="lga_name")
  # Line chart LGA and VIC comparison
  # I) LGA Line
  IncidentsLGAyear <- IncidentsLGA %>% group_by(Year,lga_name) %>% summarise('Rate
                                                                              per 100,000 population' = mean('LGA Rate per 100,000 population', na.rm = TRUE))
IncidentsLGAyear$lga_name <- toupper(IncidentsLGAyear$lga_name)
IncidentsLGAyear$lga_name <- IncidentsLGAyear$lga_name %>% as.factor()
IncidentsLGAyear$Year <- IncidentsLGAyear$Year %>% as.factor()
# II) VIC Line
IncidentsVIC <- IncidentsLGA %>% group_by(Year)%>% summarise('Rate per 100,000
                                                              population' = mean('LGA Rate per 100,000 population', na.rm = TRUE))
# Top Suburb
# "Data_Tables_LGA_Criminal_Incidents_Year_Ending_December_2022.xlsx" getting out of
memory, so pre-processing is required
#IncidentsOSLGA <-
read_excel("Data_Tables_LGA_Criminal_Incidents_Year_Ending_December_2022.xlsx",
           sheet = "Table 03")
#colnames(IncidentsOSLGA)[3] <- 'lga_name'
#IncidentsOSLGA$lga_name <- IncidentsOSLGA$lga_name %>% as.factor()
#IncidentsOSLGA <- IncidentsOSLGA %>% group_by(lga_name,'Suburb/Town Name') %>%
summarise(sum_incident = sum('Incidents Recorded', na.rm = TRUE))
#IncidentsOSLGA$lga_name <- toupper(IncidentsOSLGA$lga_name)
#writexl::write_xlsx(IncidentsOSLGA, "IncidentsOSLGA.xlsx")
IncidentsOSLGA <- read_excel("IncidentsOSLGA.xlsx")
# Drug Types
amphetamine <- read_excel("Drug type by LGA.xlsx", sheet = "Table 1")
colnames(amphetamine) <- c("lga_name",2006:2016)
amphetamine$lga_name <- amphetamine$lga_name %>% as.factor()
amphetamine_r <- amphetamine[89:168,]
amphetamine_i <- amphetamine[5:84,]
amphetamine_r <- amphetamine_r %>% gather(c(2:12), key = "year", value =
                                            "Amphetamine")
amphetamine_i <- amphetamine_i %>% gather(c(2:12), key = "year", value =
                                            "Amphetamine")
cannabis <- read_excel("Drug type by LGA.xlsx", sheet = "Table 2")
colnames(cannabis) <- c("lga_name",2006:2016)
cannabis$lga_name <- cannabis$lga_name %>% as.factor()
cannabis_r <- cannabis[89:168,]
cannabis_i <- cannabis[5:84,]
cannabis_r <- cannabis_r %>% gather(c(2:12), key = "year", value = "Cannabis")
cannabis_i <- cannabis_i %>% gather(c(2:12), key = "year", value = "Cannabis")
ecstasy <- read_excel("Drug type by LGA.xlsx", sheet = "Table 3")
colnames(ecstasy) <- c("lga_name",2006:2016)
ecstasy$lga_name <- ecstasy$lga_name %>% as.factor()
ecstasy_r <- ecstasy[89:168,]
ecstasy_i <- ecstasy[5:84,]
ecstasy_r <- ecstasy_r %>% gather(c(2:12), key = "year", value = "Ecstasy")
ecstasy_i <- ecstasy_i %>% gather(c(2:12), key = "year", value = "Ecstasy")
heroin <- read_excel("Drug type by LGA.xlsx", sheet = "Table 4")
colnames(heroin) <- c("lga_name",2006:2016)
heroin$lga_name <- heroin$lga_name %>% as.factor()
heroin_r <- heroin[89:168,]
heroin_i <- heroin[5:84,]
heroin_r <- heroin_r %>% gather(c(2:12), key = "year", value = "Heroin")
heroin_i <- heroin_i %>% gather(c(2:12), key = "year", value = "Heroin")
methamphetamine <- read_excel("Drug type by LGA.xlsx", sheet = "Table 5")
colnames(methamphetamine) <- c("lga_name",2006:2016)
methamphetamine$lga_name <- methamphetamine$lga_name %>% as.factor()
methamphetamine_r <- methamphetamine[89:168,]
methamphetamine_i <- methamphetamine[5:84,]
methamphetamine_r <- methamphetamine_r %>% gather(c(2:12), key = "year", value =
                                                    "Methamphetamine")
methamphetamine_i <- methamphetamine_i %>% gather(c(2:12), key = "year", value =
                                                    "Methamphetamine")
otherDrug <- read_excel("Drug type by LGA.xlsx", sheet = "Table 6")
colnames(otherDrug) <- c("lga_name",2006:2016)
otherDrug$lga_name <- otherDrug$lga_name %>% as.factor()
otherDrug_r <- otherDrug[89:168,]
otherDrug_i <- otherDrug[5:84,]
otherDrug_r <- otherDrug_r %>% gather(c(2:12), key = "year", value = "Other drug")
otherDrug_i <- otherDrug_i %>% gather(c(2:12), key = "year", value = "Other drug")
drug_type <- inner_join(amphetamine_r, cannabis_r, by = c("lga_name","year")) %>%
  inner_join(., ecstasy_r, by = c("lga_name","year")) %>%
  inner_join(., heroin_r, by = c("lga_name","year")) %>%
  inner_join(., methamphetamine_r, by = c("lga_name","year")) %>%
  inner_join(., otherDrug_r, by = c("lga_name","year"))
drug_type <- drug_type %>% gather(c(3:8), key = "drug_type", value = "rate of offences")
drug_type$lga_name <- toupper(drug_type$lga_name)
drug_type$lga_name[drug_type$lga_name == 'COLAC-OTWAY'] <- 'COLAC OTWAY'
test <- drug_type[drug_type$year == 2006 & drug_type$drug_type == 'Amphetamine',]
drug_typemap <- sp::merge(lga_simp, test, by="lga_name")
drug_typeMax <- drug_type %>% group_by(drug_type) %>% summarise(maxi = max('rate
                                                                            of offences', na.rm = TRUE))
# Heatmap
drug_type_i <- inner_join(amphetamine_i, cannabis_i, by = c("lga_name","year")) %>%
  inner_join(., ecstasy_i, by = c("lga_name","year")) %>%
  inner_join(., heroin_i, by = c("lga_name","year")) %>%
  inner_join(., methamphetamine_i, by = c("lga_name","year")) %>%
  inner_join(., otherDrug_i, by = c("lga_name","year"))
drug_type_i <- drug_type_i %>% gather(c(3:8), key = "drug_type", value = "Incidents
Recorded")
drug_typeHeat <- drug_type_i %>% group_by(year, drug_type) %>% summarise(Incidents =
                                                                           sum('Incidents Recorded'', na.rm = TRUE))
fig <- plot_ly(
  data = drug_typeHeat,
  x = ~year, y = ~drug_type,
  z = ~Incidents, type = "heatmap", colors = "Reds"
)
fig
# Line chart Drug
# Piechart drug
drug_typePor <- drug_type_i %>% group_by(drug_type) %>% summarise('Incidents
                                                                   Recorded' = sum('Incidents Recorded', na.rm = TRUE))
fig <- plot_ly(data = drug_typePor, labels = ~drug_type, values = ~'Incidents Recorded',
               type = 'pie') %>%
  layout(title = 'United States',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig
pal <- colorNumeric(
  "Reds",
  domain = lga_allerged$mean_incident
)
pal2 <- colorNumeric(
  "Reds",
  domain = c(0:max(drug_typeMax$maxi))
)
labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  lga_allerged$lga_name,
  lga_allerged$mean_incident
) %>% lapply(htmltools::HTML)
labels2 <- sprintf(
  "<strong>%s</strong><br/>%g",
  drug_typemap$lga_name,
  drug_typemap$'rate of offences'
) %>% lapply(htmltools::HTML)
title <- tags$div(
  HTML('<h5>Drug Crime Rate (per 100,000 population) by LGA in Victoria</h5>')
)
title2 <- tags$div(
  HTML('<h5>Drug Cime Rate by LGA</h5>')
)
# Run the application
shinyApp(ui = ui, server = server)