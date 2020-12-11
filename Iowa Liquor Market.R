---
title: "Iowa Liquor Market"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: fill
    css: css/styles-default2.css
    logo: img/craft spirits2.gif
    runtime: shiny_prerender
---

```{r context = "setup", include=FALSE}

library(flexdashboard)
library(bigrquery)
library(DBI)
library(googleAuthR)
library(ggplot2)
library(tidyverse)
library(plotly)
library(ggmap)
library(knitr)
library(leaflet)
library(DT)
library(highcharter)
library(shiny)
library(plotly)
library(rdrop2)
library(reshape2)
library(scales)
library(htmltools)
library(zoo)   # for the rolling average
library(lubridate)
library(tidyquant)
library(ggthemes)
library(dygraphs) 

#knitr::opts_chunk$set(echo = FALSE)

## establish the connection

# authenticate with Google cloud with your secret API key- opens browser prompt. It will be cached across R sessions locally
#bigrquery::bq_auth("62522979213-iu6juif1aqds5bsikqc4q904h48l3jhs.apps.googleusercontent.com")

# preferrably authenticate with the path to your service account token file. This is needed for automated use of Big Query.
#bigrquery::bq_auth("/json/service_account.json")

# establish the connection to the big query openaq data collection
con_sales <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "iowa_liquor_sales",
  billing = "xxxxxx"  
)

con_weather <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "noaa_gsod",
  billing = "xxxxxx"  
)


#con_CIVID <- dbConnect(
#  bigrquery::bigquery(),
#  project = "bigquery-public-data",
#  dataset = "covid19_usafacts",
#  billing = "neon-297716"  
#)

######## Data Tabels ##############

## First page data

marketSize<- dbGetQuery(con_sales,"select sum( volume_sold_liters ) as liters_demand ,
sum( pack * state_bottle_cost ) as gross_revenue,
count (distinct (vendor_number)) as vendor_count,
count (distinct( store_number )) as store_count,
extract(year from date) as year
from`bigquery-public-data.iowa_liquor_sales.sales`
group by year 
having gross_revenue is not null and liters_demand is not null and vendor_count is not null")

#write_csv(marketSize,"./data/marketSize.csv")

### map data

storeLocation<- dbGetQuery(con_sales,
"select sum( volume_sold_liters ) as liters_demand,
sum( pack * state_bottle_cost ) as gross_revenue,
count (distinct (vendor_number)) as vendor_count,
INITCAP((split( store_name , '/'))[safe_ordinal(1)]) as store,
store_location,
city,
extract(year from date) as year,
from`bigquery-public-data.iowa_liquor_sales.sales` 
group by store_location, year , store, city
having gross_revenue is not null and liters_demand is not null and store_location is not null" )


#write_csv(storeLocation,"./data/storeLocation.csv")


MA_GrossRev<- dbGetQuery(con_sales,"select distinct (cast(AVG(gross_revenue) OVER (partition by date)*100 as int64))as avg_gross_rev,
date
from
(select cast((pack * state_bottle_cost)*100 as int64)  as gross_revenue,
date
from `bigquery-public-data.iowa_liquor_sales.sales`
where date is not null)a
order by avg_gross_rev desc")

#write_csv(MA_GrossRev,"./data/MA_GrossRev.csv")

Product_category<- dbGetQuery(con_sales,"select distinct (INITCAP( category_name) )as category,
cast(sum( volume_sold_liters )*100 as int64) as liters_demand,
extract(year from date) as year
from `bigquery-public-data.iowa_liquor_sales.sales`
group by category_name, year
having  category_name is not null
order by liters_demand desc ")

# write_csv(Product_category,"./data/Product_category.csv")


storeLocation1<-storeLocation

storeLocation1<-
  storeLocation1%>%
  mutate(store_location = str_replace_all(store_location, "\\(|\\)", ""))%>%
  separate(store_location, c("delete","long","lat")," ")%>%
  select(-delete)%>%
  mutate(lat = as.double(lat),
         long = as.double(long))



```



Market Overview 2012 - 2020 {data-icon="fa-chart-line"} 
=======================================================================================

Column {.sidebar}
-----------------------------------------------------------------------

```{r}
numericInput('year', 'Plot total annual revenue on the map', 2019,
              min = 2012, max = 2020)

```

The map represents the store locations and their total annual revenue. 
The blue mark corresponds to the size of the revenue for the area. 
Use the dropdown button to choose the year for the total revenue you want to map.


Row 
--------------------------------------

### Average annual demand in liters of alcohol

```{r}

AverageVolumeSold <- sum(marketSize$liters_demand)/9
  

  #aggregate(liters_demand ~year, marketSize, mean)

valueBox(value= AverageVolumeSold %>% scales::comma(),
         icon = "fas fa-cocktail", 
         caption ="Average annual liquor sold 2012 - 2020",
         color ="DarkSlateBlue" ) 

```


### Gross Average Annual Revenue

```{r}


AverageAnnualRevenue <- sum(marketSize$gross_revenue)/9

#AverageAnnualRevenue <- aggregate(gross_revenue ~year, marketSize, mean)


valueBox(value = AverageAnnualRevenue %>% scales::dollar(scale = 1e-6, suffix="M", accuracy = 0.01),
         icon = "fa-usd",
         caption = "Average annul revenue 2012-2020", 
         color = 'DarkRed')
```


### Average Annual Number of Competitors 

```{r}
averageNCompetitors<-sum(marketSize$vendor_count)/9


#TotalNCompetitors <- aggregate(vendor_count ~year, marketSize, mean)

valueBox(value = averageNCompetitors %>% scales::number(),
         icon = "fas fa-user-alt",
         caption = "Average annual number of competitors 2012 - 2020", 
         color = 'DarkSlateBlue')

```

### Average Annual Number of Retailers 

```{r}

TotalNRetailers <- sum(marketSize$store_count)/9


#TotalNCompetitors <- aggregate(store_count ~year, marketSize, mean)

valueBox(value = TotalNRetailers,icon = "fas fa-store",caption = "Average annual number of retailers 2012 - 2020", color = 'DarkRed')

```


Row
---------------------------------------------------------------

### Geographic destribution of revenue {data-width=668}


```{r, context="server"}

renderLeaflet({
 storeLocation1%>%
    filter(year == input$year)%>%
  leaflet()%>%
#  leaflet(storeLocation1) %>%
 addTiles() %>%
  addCircles(lng = ~long, lat = ~lat, weight = 1,
    radius = ~sqrt(gross_revenue),label = ~htmlEscape(store)
  )
})

```


Row
-------------------------------------

### Total revenue and demand over time

```{r}


demand_revenue<- marketSize%>%
  select(year, liters_demand, gross_revenue )%>%
  arrange(year) %>%
  melt(id="year")

hchart(demand_revenue, "line", hcaes(x = year, y = value, group = variable))



```

### Competitors and retailers number over time

```{r}


demand_revenue<- marketSize%>%
  select(year, store_count,vendor_count )%>%
  arrange(year) %>%
  melt(id="year")

hchart(demand_revenue, "line", hcaes(x = year, y = value, group = variable))


```


Trend Analysis and Forecasting 2012 - 2020 {data-icon="fa-chart-line"} 
=======================================================================================

Column {.sidebar}
-----------------------------------------------------------------------

```{r}

numericInput('months_over', 'Number of months', 13,
             min = 1, max = 106)

```

Select the number of month over which to calculate the moving average. The higher is the number the smoother will be the line.

Row
-------------------------------------

### Moving Average

```{r context= "server"}


#movingAvgRevenue <- dbGetQuery(con_sales,
#"select 
#date,
#gross_revenue,
#cast(AVG(gross_revenue) OVER (order by date  ROWS 30 PRECEDING)*100 as int64) AS MA20,
#cast(AVG(gross_revenue) OVER (order by date  ROWS 365 PRECEDING)*100 as int64) AS MA100
#from
#(
#select 
#cast((pack * state_bottle_cost)*100 as int64)  as gross_revenue,
#INITCAP((split( store_name , '/'))[safe_ordinal(1)]) as store,
#date,
#from`bigquery-public-data.iowa_liquor_sales.sales` 
#where date > '2020-6-30')a" )

#write_csv(movingAvgRevenue,"./data/movingAvgRevenue.csv")


## static version

#grossrev_static <- MA_GrossRev %>%
#  select(date, grossrev = gross_revenue)%>%
#  mutate(grossrev_1 = rollmean(MA_GrossRev$gross_revenue, k = 5, fill = NA),
#         grossrev_2 = rollmean(MA_GrossRev$gross_revenue, k = 13, fill = NA),
#         grossrev_3 = rollmean(MA_GrossRev$gross_revenue, k = 18, fill = NA),
 #        grossrev_4 = rollmean(MA_GrossRev$gross_revenue, k = 23, fill = NA),
 #       )


## plot the rolling value (stating version)

#grossrev_static  %>%
#  gather(metric, value, grossrev:grossrev_4) %>%
#  ggplot(aes(date, value, color = metric)) +
#  geom_line()

## Modelling

#grossrev_choice<- reactive({MA_GrossRev %>%
 #   mutate(grossrev_y = rollmean(MA_GrossRev$avg_gross_rev, k = input$months_over, fill = NA))
#})


#renderPlot({ ggplot(grossrev_choice(),aes(x=grossrev_choice()$date, y=grossrev_choice()$grossrev_y )) +
 #   geom_line()+
  #  theme_tq()+
   # ylab("Revenue") + xlab("Time window")
#})

MA_GrossRev_xts<-xts(MA_GrossRev$avg_gross_rev, order.by = MA_GrossRev$date)


renderDygraph({
  dygraph(MA_GrossRev_xts) %>% 
  dyRangeSelector()%>%
  dyOptions(labelsKMB = TRUE)%>%
  dyRoller(rollPeriod = input$months_over)
  })
       


```

column
--------------------------------------

### Trend Analysis

```{r context= "server"}

tsMA_GrossRev <- reactive({ ts(MA_GrossRev$avg_gross_rev, start = c(2012,5), frequency = input$months_over)})

components.ts<-reactive({ decompose(tsMA_GrossRev())})
renderPlot({plot(components.ts())})

```

### Meaning of the 4 components

* __Observed__ – the actual data plot
* __Trend__ – the overall upward or downward movement of the data points
* __Seasonal__ – any monthly/yearly pattern of the data points
* __Random__ – unexplainable part of the data    
   
In additive time series these 4 components  add together to make the time series. While in multiplicative time series the components multiply together to make the time series.e.g.  if you have an increasing trend, the amplitude of seasonal activity increases.


Product Category Demand 2012 - 2020 {data-icon="fas fa-cocktail"} 
=======================================================================================

Column {.sidebar}
-----------------------------------------------------------------------

```{r}

numericInput('year1', 'Total Annual Liquor Demand by Category', 2019,
              min = 2012, max = 2020)

```

Pick a year to see the demand volume per liquor categories



row
--------------------------------------

### Liquor Demand Overview (2012 - 2020)

````{r}


renderDT ({
  DT::datatable(Product_category%>%
    filter(year == input$year1),
                colnames=c(  'Liquor Category'= 'category','Demad'= 'liters_demand', 'Year' = 'year'),
                fillContainer=TRUE,
                rownames = FALSE, options = list(
                  pageLength = 50,
                  lengthMenu = c(10,50,200)
                ))
})


````


### Liquor Category Demand Comparison

```{r}


renderHighchart({
hchart(
  Product_category%>%filter(year == input$year1), 
   type="column",
       hcaes(x = category, y = liters_demand, group = category)
          )
})

```
