## ----------------------------
## ArcLakeVis
## ----------------------------

library(shiny)
# library(shinydashboard)
library(leaflet)
library(ggplot2)
library(knitr)
library(RJSONIO)
library(raster)
library(maps)
library(DT)
library(RColorBrewer)
library(mgcv)


# colours used to represent different groups
# cols <- c("Dark blue", "Mid blue", "Light blue", "Yellow", "Orange", "Red", "Pink", "Light Green","Dark Green")


## -----------------
## The UI function
## -----------------
ui <- fluidPage(
    # tags$head(includeScript("google-analytics.js")),
    fluidRow(
      column(5, offset=1,
             h2('GloboLakes','ARCLAKE Time Series')),
      
      column(5, 
             br(),
             br(),
             # htmlWidgetOutput(
             #    outputId = 'desc',
             #    HTML(paste(
             #    hr(),
             #    'The map is centered at <span id="lng"></span>, <span id="lat"></span>',
             #    'with a zoom level of <span id="zoom"></span>.<br/>'     ,
             #     sep="")))
             textOutput('center')
      )),
    
    # tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
    # tags$head(tags$link(rel='stylesheet', href='https://unpkg.com/leaflet@1.3.1/dist/leaflet.css')),
    # tags$script(src="https://unpkg.com/@mapbox/mapbox-sdk/umd/mapbox-sdk.js"),
    
    leaflet::leafletOutput('basemap', width="100%", height=450),
    
    hr(),
    sidebarLayout(
      
      sidebarPanel(
        
        sliderInput(inputId = "x_range",
                    label = paste("Time Range"),
                    min = 1995, max = 2012, sep="",
                    value = c(1995, 2012)),
        
        checkboxInput('lakespecmean', 'Show group mean curve for this lake', FALSE),
        
        selectizeInput('addmean', 
                       label=h6("Add Group Mean Curves"),
                       choices=c('G4 Dark blue'=4, 'G5 Mid blue'=5, 'G9 Light blue'=9, 'G1 Yellow'=1,  
                                 'G2 Orange'=2, 'G3 Red'=3, 'G6 Pink'=6, 'G8 Light green'=8, 'G7 Dark green'=7), 
                       multiple = TRUE,
                       options = list(
                         placeholder = 'Please select',
                         onInitialize = I('function() { this.setValue(""); }'))),
        
        checkboxInput('allmeans', 'Show plot of all lake Mean Curves', FALSE),
        
        checkboxInput('lakemask', 'Show plot of lake mask', FALSE),
        
        width=3
      ),
      
      mainPanel(
        
        tabsetPanel(
          
          tabPanel("Main",  plotOutput('LakeTimeSeries'),
                   conditionalPanel("input.allmeans == TRUE", plotOutput('AllMeanCurves',height = "500px")),
                   conditionalPanel("input.lakemask == TRUE", plotOutput('maskplot',height = "600px"))
          ),
          
          tabPanel('Model',
             fluidRow(
                  column(11,
                  h3("Are the seasonal and trend patterns in the time series significant?
                     The generalized additive model (GAM) can help. Click the button to run the model."),
                  br(),
                  actionButton('gam', strong('Fit the model', style = 'color:steelblue')),
                  # checkboxInput('gam', strong('Fit the model', style = 'color:gray'), FALSE),
                  br(),
                  br(),
                  h4("We can look at the effective degrees of freedom (edf) of the smooth functions of time and month
                      to see if any of these are significantly non-constant. For time series data, an edf greater than 1 
                      suggests that there is certain linear or curved feature.",
                     style = 'color:gray'),
                  h4("We can also plot the fitted smooth functions to have a better idea of this linear or curved feature.",
                     style = 'color:gray'),
                  # checkboxInput('plotSmooth', 'Plot the fitted smooth functions.', FALSE),
                  hr()),
                  column(1)
               
            ),
      
            fluidRow(
                  column(11,
                  conditionalPanel("is.null(input['gam']) == FALSE",  # input['xx'] to get the values
                             br(),
                             verbatimTextOutput("GamSummary"),
                             plotOutput("GamPlot", height = "380px"))),
                  column(1)
            )),
            
          tabPanel('Data',  DT::dataTableOutput("contents")),
          
          tabPanel("About",  includeHTML('APPTAB2.html'))   # includeMarkdown('APPTAB2.md')
        ))
    )
  )



newalldat <- readRDS('newalldat.rds')
newalldat2 <- readRDS('newalldat2.rds')
maskbrick <- readRDS('maskbrick.rds')
# meanefd <- readRDS('meanefd.rds')

makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3], alpha=alpha, maxColorValue=255)})
}

# These are the colours used the lakebioms app
groupcols <- c("#0000CD", "#1E90FF", "#ADD8E6", "#EEC900", "#FF8C00", "#EE0000", 
               "#EEA9B8", "#9ACD32", "#228B22")[order(c(4, 5, 9, 1, 2, 3, 6, 8, 7))]
# the order matches the climate zones
colorder <- order(c(4, 5, 9, 1, 2, 3, 6, 8, 7))

cnames <- c("Northern cold",
            "Northern cool",
            "Northern temperate",
            "Northern warm",
            "Northern hot",
            "Equatorial hot",
            "Southern hot",
            "Southern warm",
            "Southern temperate")
codes <- c("NC", "NO", "NT", "NW", "NH", "EH", "SH", "SW", "ST" )
groupdat <- data.frame(cbind("Name"=cnames, "Code"=codes, "Colour"=groupcols))

sm <- read.csv("SmoothMeans_9groups.csv")[,-1]
sse <- read.csv("SmoothStandardErrors_9groups.csv")[,-1]

tab <- read.csv("ArcLakeGroupInfo_9groups.csv", stringsAsFactors=FALSE)[,-1]
# names(tab)
tab[,"OverallAvg"] <- round(tab[,"OverallAvg"],2)
tab[,"OverallMeanAmp"] <- round(tab[,"OverallMeanAmp"],2)
# tab[,"Group"]<- as.character(formatC(tab[,"Group"], flag="0", digits=1))

smooth <- newalldat2[,10:407]
rec <- newalldat[,10:407]
time <- round(as.numeric(names(rec)),2)

Lakes <- newalldat2[, 1:9]   # For all lakes
npix <- Lakes$LakeSize   # this is for the maps only...



## ---------------------
## The server function
## ---------------------
server <- function(input, output) {
  
  makeReactiveBinding('selectedLake')
  # Turns a normal variable into a reactive variable
  # Define some reactives for accessing the data
  
  ## (1) The map on the top
  # Display the center of the map
  output$center <- renderText({
    cent <- input$basemap_center
    cent.lng <- round(as.numeric(cent[1]), 2)
    cent.lat <- round(as.numeric(cent[2]), 2)
    paste0('This map is centered at (', cent.lng, ', ', cent.lat, ').')
  })
  
  # Retrieve the IDs of the column that contains the selected year's data
  monthCol <- reactive({
    x_range <- input$x_range
    time <- as.numeric(names(rec))
    ab <-  c(c(time >= min(x_range)) & c(time <= max(x_range)))
    r <- range(time[ab], na.rm=TRUE)
    a <- which(time==r[1])
    b <- which(time==r[2])
    colids <- c(a:b)  # column IDs
  })
  
  # The lakes that are within the visible bounds of the map
  # LakesInBounds <- reactive({
  #   if (is.null(input$basemap_bounds))
  #   return(newalldat[FALSE,])  # return an empty data set
  
  #   bounds <- input$basemap_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  
  #   Lakesub <- subset(newalldat2,
  #          newalldat2$Latitude >= latRng[1] & newalldat2$Latitude <= latRng[2] &
  #          newalldat2$Longitude >= lngRng[1] & newalldat2$Longitude <= lngRng[2])
  #   Lakesub
  # })
  
  # Create the map using provider's map
  # OpenStreetMap.Mapnik
  # CartoDB.PositronNoLabels
  # Esri.WorldShadedRelief
  map0 <- leaflet::leaflet(options=leafletOptions(minZoom=2, maxZoom=4)) %>% 
    leaflet::addProviderTiles(provider='Esri.WorldShadedRelief', 
                              options=providerTileOptions(errorTileUrl='No idea why it failed...'))
  
  output$basemap <- leaflet::renderLeaflet(map0)
  
  # The rest will be on a 'map proxy'
  leaflet::leafletProxy('basemap') %>%
    leaflet::addCircles(lng=Lakes$Longitude, lat=Lakes$Latitude, radius=10000, 
                        layerId=Lakes$GloboLakes_ID, color=groupcols[Lakes$Group], 
                        fill=TRUE, weight=((npix/max(npix))*10)+10)
  # this is fixed
  
  # output$basemap <- leaflet::renderLeaflet({
  #   LakeB <- LakesInBounds()
  #   npixB <- LakeB$LakeSize
  #   map <- map0 %>% leaflet::addCircles(lng=LakeB$Longitude, lat=LakeB$Latitude, radius=10000, 
  #                                       layerId=LakeB$GloboLakes_ID, color=savecols[LakeB$Group], 
  #                                       fill=TRUE, weight=((npixB/max(npixB))*10)+10)
  # })
  
  observe({
    event <- input$basemap_shape_click
    if (is.null(event))
      return()
    
    isolate({
      # Lakes <- LakesInBounds()
      Lake <- Lakes[Lakes[,1] == event$id,]
      selectedLake <<- Lake
      
      content <- as.character(tagList(
        tags$strong(paste0('ID ', Lake$GloboLakes_ID, ', ', Lake$LakeName, ', ', Lake$Country,  
                           ", Elevation: ", Lake$Elevation, "m, Group ", Lake$Group)),
        tags$br()
      ))
      
      leaflet::leafletProxy('basemap') %>% 
        leaflet::clearMarkers() %>%
        leaflet::addPopups(event$lng, event$lat, content, event$id)
      
    })
  })
  
  
  ## (2) The time series plot of the selected lake
  output$LakeTimeSeries <- renderPlot({
    
    validate(need(!is.null(selectedLake), 'Please select a lake'))
    Lake <- NULL
    if (!is.null(selectedLake)) {Lake <- selectedLake}
    else{
      cat('')
      # ixx <- which.max(LakesInBounds()$npix)
      # Lake <- LakesInBounds()[ixx,]
    }
    
    Lakeid <- Lake[1,1]
    id <- which(newalldat$GloboLakes_ID==Lakeid)
    monthcol <- monthCol()
    ts <- rec[id, monthcol]
    ts.smooth <- smooth[id, monthcol]
    tempexp <- expression(paste("Temperature (",degree,"C)"))
    par(mfrow=c(1,2), oma=c(0,0,5,0), mar=c(4,6,4,2), cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
    ti <- time[monthcol]
    
    plot(ti, ts, xlab="Time", ylab=tempexp, 
         type="p", pch=19, ylim=c(-1,35))
    lines(ti, ts.smooth, xlab="Time", ylab=tempexp, 
          type="l", pch=19, col="gray50", lwd=2)
    rug(as.numeric(ti), col="gray30")
    
    if (input$lakespecmean==TRUE){   # add lake specified mean
      gid <- Lake$Group
      ms <- sm[gid, monthcol]
      lines(ti, ms, col=groupcols[gid], lwd=2)
      title(paste("Group", gid), line=1)
    }
    
    if(!is.null(Lake)) {   # add group means
      iadm <- unlist(as.numeric(input$addmean))
      for(ii in 1:length(iadm)){
        lines(ti, sm[iadm[ii], monthcol], col=groupcols[iadm[ii]], lwd=2)
      }
    } 
    
    monthlab <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    month <- c(ti-floor(ti)) %/% c(1/12) + 1
    plot(month, ts, pch=19, ylab=tempexp , xlab="Month", ylim=c(0,35), xaxt="n")
    axis(side=1, at=1:12, monthlab)
    
    title1 <- data.frame(lapply(newalldat2[id, c(1:9)], as.character), stringsAsFactors=FALSE)
    # mytitle <- paste("GLWD ID:",title1[1],", Site Name:" ,title1[2], 
    #                  "Country:", title1[5],", Type:", title1[6])
    # colnames(newalldat)
    
    title(paste("GLWD ID:", title1[1], ", Name:" ,title1[2], "\n Country:", title1[5],", Type:", title1[6]),
          cex.main=1.5, outer=TRUE)
    
  })
  
  
  output$maskplot <- renderPlot({
    
    validate(need(!is.null(selectedLake), ''))
    Lake <- NULL
    if (!is.null(selectedLake)) {Lake <- selectedLake}
    else {
      cat('A lake, please!')
      # ixx <-which.max(LakesInBounds()$npix)
      # Lake <- LakesInBounds()[ixx,]
    }
    
    Lakeid <- Lake[1,1]
    id <- which(newalldat2$GloboLakes_ID==Lakeid)
    
    if(input$lakemask){
      par(cex=1.5, mar=c(4,4,2,2), oma=c(2,0,3,5))
      plot(maskbrick[[id]], legend=FALSE, col='gray30', xlab='Longitude', ylab='Latitude')
    }
  })
  
  
    output$AllMeanCurves <- renderPlot({
    monthcol <- monthCol()
    cses <- sse[, monthcol]
    cmeans <- sm[, monthcol]
    tempexp <- expression(paste("Temperature (",degree,"C)"))
    par(mfrow=c(1,2), oma=c(0,0,5,0), mar=c(4,6,4,2), cex.axis=1.5, cex.lab=1.5)
    ti <- time[monthcol]
    
    if(input$allmeans){
      par(fig=c(x1=0, x2=7.5/10, y1=0, y2=1))
      plot(ti, ti, xlab="Time", ylab=tempexp, type="n", ylim=c(-1,35))
      ups   <- cmeans + 1.96*cses # 1.96*(cses)
      downs <- cmeans - 1.96*cses
      for (kk in 1:9){
        lines(ti, cmeans[kk,], lty=1, lwd=2, col=groupcols[kk])
        up  <- ups[kk, ]
        down <- downs[kk,] 
        polygon(c(ti, rev(ti)), c(up, rev(down)), col=makeTransparent(groupcols[kk], alpha=60), 
                border=makeTransparent(groupcols[kk], alpha=60), lwd=2)
      }
      
      # matplot(ti, t(cmeans), type='l', lty=1, lwd=3, col=groupcols, 
      #         xlab='Time', ylab=tempexp, ylim=c(-1, 35))
      
      par(fig=c(x1=6.8/10, x2=1, y1=0, y2=1), new=T)
      plot(0, 1, type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
      legend(cex=1.25, "topleft", bty="n", legend=groupdat$Name[order(c(4, 5, 9, 1, 2, 3, 6, 8, 7))], 
             pch=19, col=groupcols[1:9], title="Group name")
    }
  })
  
  
  ## (3) The data table for lake information
  output$contents <- DT::renderDataTable(datatable(tab, filter = 'top'))  # The table is on its own
  
  
  ## (4) The gam model for the significance of trend and seasonality
  GamData <- eventReactive(input$gam, {
    fitGam <- NULL
    
    if (!is.null(selectedLake)) {
      Lake <- selectedLake
      Lakeid <- Lake[1,1]
      id <- which(newalldat$GloboLakes_ID==Lakeid)
      lswt <- as.vector(data.matrix(rec[id,]))
      decimal_month <- time - floor(time)
      data_mat <- data.frame(lswt, time, decimal_month) 
      fitGam <- gam(lswt ~ s(time, bs='ps') + s(decimal_month, bs='cc'), data=data_mat)
    }
    
    fitGam
  })
  
  
  output$GamSummary <- renderPrint({
    # validate(need(!is.null(selectedLake), ''))
    fitGam <- GamData()
    if (!is.null(fitGam)) {
      summary(fitGam)
    }
    else {print('')}
  })
  
  
  output$GamPlot <- renderPlot({
    fitGam <- GamData()
    par(mfrow=c(1,2), cex=1.3)
    if (!is.null(fitGam)) {
      plot(fitGam, ylab='', lwd=2)
    }
    else {plot(0, 1, type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")}
  })
  
}


## Run the app
shinyApp(ui = ui, server = server)

