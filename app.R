library(shiny)
library(tigris)
library(leaflet)
library(tidyverse)

#####
## Download Congressional District Shape Files
cd114500kt <- congressional_districts(cb = TRUE, resolution = '500k', year = 2015)

## Bring In Cadet Data
cadets = read_csv("AA_HistoricalData_00to20.csv")
cadets$STATEDIST


 cadets$STATEDIST = ifelse(substr(cadets$STATEDIST,start = 3,stop = 4)=="AL",paste0(substr(cadets$STATEDIST,start = 1,stop = 2),"00"),cadets$STATEDIST)


## Bring in state abbreviation codes to refrence later
statecodes = read_csv("statecodes.csv")

## Prepare Data for plotting

plottingdf = data.frame(statecode = cd114500kt$STATEFP, districtcode = cd114500kt$CD114FP, matchthis = as.character(paste0(cd114500kt$STATEFP,cd114500kt$CD114FP)))

shading = cadets %>%
  filter(FILESTATUS=="ACCEPTED") %>%    ## only want cadets who were accepted
  mutate(GRADYR = `GRAD YR`) %>%        ## change name of variable for ease of analysis
  select(CEER, STATEDIST, GRADYR) %>%   ## Select columns of interest
  # group_by(GRADYR, STATEDIST) %>%       ## group to enable summaries
  group_by(STATEDIST) %>%       ## group to enable summaries
  summarise(TOTCADETS = n()) %>%        ## summaries total cadets -- here is where we need demographics data
  ungroup() %>%                         ## ungroup for filter
  # filter(GRADYR == 2007) %>%            ## filter the class year of interest
  # Split the STATEDIST code for matching the text state codes
  mutate(Abb = substr(STATEDIST,start = 1, stop = 2), districtcode = substr(STATEDIST,start = 3,stop = 4)) %>%
  left_join(statecodes, by = "Abb") %>%  ## Join with the state codes
  ## give single digit state codes a leading 0
  mutate(NumericCode = ifelse(nchar(NumericCode)==1,paste0("0",NumericCode),NumericCode)) %>%
  mutate(matchthis = paste0(NumericCode,districtcode)) %>%   ## combine the NumericCode and District codes together
  select(matchthis, TOTCADETS, STATEDIST)


plottingdf2 =
  plottingdf %>%
  left_join(shading, by = "matchthis") %>%
  mutate(TOTCADETS = ifelse(is.na(TOTCADETS),0,TOTCADETS)) %>%
  select(TOTCADETS)

forggplotshading = plottingdf2$TOTCADETS
forggplotshading = forggplotshading/max(forggplotshading)

# ifelse(nchar(shading$NumericCode)==1,paste0("0",shading$NumericCode),shading$NumericCode)

vec1 = NA
vec2 = NA
for (i in 1:441) {
  vec1[i]= cd114500kt@polygons[i][[1]]@labpt[1]
  vec2[i]= cd114500kt@polygons[i][[1]]@labpt[2]
}

texthelper2 =
  plottingdf %>%
  left_join(shading, by = "matchthis") %>%
  mutate(TOTCADETS = ifelse(is.na(TOTCADETS),0,TOTCADETS)) %>%
  select(STATEDIST)

texthelper = texthelper2$STATEDIST
#####

# Define UI for application that draws a histogram
# ui <- bootstrapPage(
#  leafletOutput("map", width = "100%", height = "100%")
#
# )
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                #             value = range(quakes$mag), step = 0.1
                # ),
                # selectInput("colors", "Color Scheme",
                #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                # ),
                checkboxInput("legend", "Show Data", FALSE)
  )
)

  # Define server logic required to draw a histogram
server <- function(input, output) {

  output$map <- renderLeaflet({

    leaflet(data = cd114500kt,options = leafletOptions(minZoom = 3, maxZoom = 8)) %>%
      addTiles() %>%
      setView(lng = -97, lat = 38, zoom =3) %>%
      addPolygons(stroke = TRUE, fillOpacity = forggplotshading, smoothFactor = 0.5,
                  color = "black", opacity = 1, weight = 1)
    # %>%
    #   addCircleMarkers(radius = .1, color = "black",
    #                    lng=vec1, lat = vec2, opacity = .5, fillOpacity = forggplotshading,
    #                    label = NULL, popup = paste("AA Enrolled", forggplotshading),
    #                    labelOptions = labelOptions(clickable = TRUE, noHide = T, textOnly = FALSE, zoomAnimation = TRUE)
    #                    )

  })


  observe({

      if (input$legend) {
        leafletProxy("map") %>%
          addCircleMarkers(radius = 5, color = "red",
          lng=vec1, lat = vec2, opacity = 0, fillOpacity = 1,
          label = NULL, popup = paste(texthelper, ": AA Enrolled", forggplotshading),
          labelOptions = labelOptions(clickable = TRUE, noHide = T, textOnly = FALSE, zoomAnimation = TRUE)
          )
      }

  })
  observe({

      if (input$legend==FALSE) {
        leafletProxy("map") %>%
          clearMarkers()
          }

  })


}

# Run the application
shinyApp(ui = ui, server = server)

