###### UN Datathon Maps  #######

#libraries
library(tidyverse)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(leaflet.extras2)
library(readr)
library(cancensus)
library(viridis)

#set options
options(cancensus.api_key = "INSERT API KEY") 
#api key -- see https://cran.r-project.org/web/packages/cancensus/vignettes/cancensus.html for information

#set cache path
options(cancensus.cache_path = "INSERT DIRECTORY PATH")


####CANADA - PROVINCES

# sf-class data frame
pr <- get_census(dataset='CA21', regions=list(C="1"),
                 vectors=c( "nodiploma"= "v_CA21_5811"),
                 level='PR', use_cache = FALSE, geo_format = 'sf')

#calculate no diploma proportion
pr$nodip_per<-round((pr$nodiploma/pr$Population)*100, digits = 2)

#set bins
bins <- c(0,5,10,15,20,25, Inf)

#set colour palette
palp <- colorBin("viridis", domain = pr$nodip_per, bins = bins,na.color = "#FFFFFF")

#labels
labelp<- sprintf("Province: <strong>%s</strong> 
                <br/>No Secondary Diploma or equivalency: <strong>%s%%</strong>", 
                 pr$name,pr$nodip_per) %>% 
  lapply(htmltools::HTML)


####CANADA - CENSUS DIVISIONS

# sf-class data frame
cd <- get_census(dataset='CA21', regions=list(C="1"),
                 vectors=c( "nodiploma"= "v_CA21_5811"),
                 level='CD', use_cache = FALSE, geo_format = 'sf')

#calculate no diploma proportion
cd$nodip_per<-round((cd$nodiploma/cd$Population)*100, digits = 2)


#set colour palette
pal <- colorBin("viridis", domain = cd$nodip_per, bins = bins)

#labels
label<- sprintf("Census Division: <strong>%s</strong> 
                <br/>No Secondary Diploma or equivalency: <strong>%s%%</strong>", 
                cd$name,cd$nodip_per) %>% 
  lapply(htmltools::HTML)


####BRITISH COLUMBIA - CENSUS SUBDIVIONS

# sf-class data frame
ca <- get_census(dataset='CA21', regions=list(PR="59"),
                 vectors=c( "nodiploma"= "v_CA21_5802"),
                 level='CSD', use_cache = FALSE, geo_format = 'sf')

#calculate no diploma proportion
ca$nodip_per<-round((ca$nodiploma/ca$Population)*100, digits = 2)

#palette
palc <- colorBin("viridis", domain = ca$nodip_per, bins = bins,na.color = "#FFFFFF")

#labels
labelc<- sprintf("Census Subdivision: <strong>%s</strong> 
                <br/>No Secondary Diploma or equivalency: <strong>%s%%</strong>", 
                 ca$name,ca$nodip_per) %>% 
  lapply(htmltools::HTML)



####SURREY, BC, CANADA - DISSEMINATION AREAS

# sf-class data frame
bn <- get_census(dataset='CA21', regions=list(CMA="5915004"),
                 vectors=c( "nodiploma"= "v_CA21_5802"),
                 level='DA', use_cache = FALSE, geo_format = 'sf')

#calculate no diploma proportion
bn$nodip_per<-round((bn$nodiploma/bn$Population)*100, digits = 2)

#palette
palb <- colorBin("viridis", domain = bn$nodip_per, bins = bins,na.color = "#FFFFFF")

#labels
labelb<- sprintf("Dissemination Area: <strong>%s</strong> 
                <br/>No Secondary Diploma or equivalency: <strong>%s%%</strong>", 
                 bn$name,bn$nodip_per) %>% 
  lapply(htmltools::HTML)


###COMBINED MAP
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-98.3, 57.5, 4) %>% 
  addResetMapButton()%>%
  addControl("<strong>Data Source</strong>: Statistics Canada;<br/>
  2021 Canadian Census; cancensus R pkg<br/>
              <strong>Variable</strong>: 25% Data;
              Adults aged 25-64;<br/> 
             No High School Diploma or equivalency", 
             position = "bottomleft")%>%
  addPolygons(data = pr, fillColor = ~palp(nodip_per),
              color = "black",
              weight = 0.5,
              opacity = 1,
              group = "Provincial",
              fillOpacity = 0.9,label = labelp,
              highlightOptions = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                color = "white",
                bringToFront = TRUE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addPolygons(data = cd, fillColor = ~pal(nodip_per),
              color = "black",
              weight = 0.5,
              opacity = 1,
              group = "Census Division",
              fillOpacity = 0.9,label = label,
              highlightOptions = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                color = "white",
                bringToFront = TRUE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = ca, fillColor = ~palc(nodip_per),
              color = "black",
              weight = 0.5,
              opacity = 1,
              group = "British Columbia",
              fillOpacity = 0.9,label = labelc,
              highlightOptions = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                color = "white",
                bringToFront = TRUE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = bn, fillColor = ~palb(nodip_per),
              color = "black",
              weight = 0.5,
              opacity = 1,
              group = "Surrey, BC",
              fillOpacity = 0.9,label = labelb,
              highlightOptions = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                color = "white",
                bringToFront = TRUE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = palp, values = pr$nodip_per, opacity = 0.7, 
            title = "No Secondary<br/>Diploma",
            position = "topright", na.label = "", labFormat = labelFormat(suffix = "%"))%>%
  addLayersControl(baseGroups = c("Provincial", "Census Division",
                                  "British Columbia", "Surrey, BC"),
                   options = layersControlOptions(collapsed = FALSE))
