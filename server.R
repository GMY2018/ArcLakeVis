library(shiny)
library(leaflet)
library(magrittr)
library(raster)
library(maps)
library(DT)
library(mgcv)


newalldat <- readRDS('newalldat.rds')
newalldat2 <- readRDS('newalldat2.rds')
maskbrick <- readRDS('maskbrick.rds')

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
tab[,"OverallAvg"] <- round(tab[,"OverallAvg"],2)
tab[,"OverallMeanAmp"] <- round(tab[,"OverallMeanAmp"],2)
# tab[,"Group"]<- as.character(formatC(tab[,"Group"], flag="0", digits=1))

smooth <- newalldat2[,10:407]
rec <- newalldat[,10:407]
time <- round(as.numeric(names(rec)), 2)

Lakes <- newalldat2[, 1:9]   # For all lakes
npix <- Lakes$LakeSize   # this is for the maps only...


## The server function
function(input, output, session) {
  
  makeReactiveBinding('selectedLake')
  # Turns a normal variable into a reactive variable
  # This object is from the observe function and is used throughout the server function
  
  ## (1) For the map
  # Display the center of the map
  output$center <- renderText({
    cent <- input$basemap_center
    cent_lng <- round(as.numeric(cent[1]), 2)
    cent_lat <- round(as.numeric(cent[2]), 2)
    paste0('This map is centered at (', cent_lng, ', ', cent_lat, ').')
  })
  
  ## Create the map using provider's map
  # OpenStreetMap.Mapnik
  # CartoDB.PositronNoLabels
  # Esri.WorldShadedRelief
  map0 <- leaflet::leaflet(options=leafletOptions(minZoom=2, maxZoom=4)) %>% 
          leaflet::addProviderTiles(provider='Esri.WorldShadedRelief', 
                   options=providerTileOptions(errorTileUrl='Oh dear!'))
  
  output$basemap <- leaflet::renderLeaflet(map0)
  
  # object that lets us control the leaflet map on the page.
  # map0 <- leaflet::leaflet(options=leafletOptions(minZoom=2, maxZoom=5)) %>% 
  #            leaflet::addTiles("//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
  #            attribution=HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>')) %>% 
  #            leaflet::setView(38, 9, zoom=2)
  
  # The rest will be on a 'map proxy'
  leaflet::leafletProxy('basemap') %>%
  leaflet::addCircles(lng=Lakes$Longitude, lat=Lakes$Latitude, radius=10000, 
                      layerId=Lakes$GloboLakes_ID, color=groupcols[Lakes$Group], 
                      fill=TRUE, weight=((npix/max(npix))*10)+10)
  
  
  ## (2) For the pop-ups informations and the LSWT plot
  # LakesInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #       return(newalldat[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  #   
  #   subset(newalldat2,
  #          newalldat2$Latitude >= latRng[1] & newalldat2$Latitude <= latRng[2] &
  #          newalldat2$Longitude >= lngRng[1] & newalldat2$Longitude <= lngRng[2])
  # })
  
  # For the pop-ups and also create the reactive object 'selectedLake'
  observe({
    event <- input$basemap_shape_click
    if (is.null(event)) return()
    else Lake <- Lakes[Lakes[,1] == event$id,]
    selectedLake <<- Lake   # for the reactive binding
    
    content <- as.character(tagList(
      tags$strong(paste0('ID ', Lake$GloboLakes_ID, ', ', Lake$LakeName, ', ', Lake$Country,  
                         ", Elevation: ", Lake$Elevation, "m, Group ", Lake$Group)),
      tags$br()
    ))
    
    leaflet::leafletProxy('basemap') %>%
    leaflet::clearMarkers() %>%
    leaflet::addPopups(event$lng, event$lat, content, event$id)
  })
  
  
  # For the time series plot
  # Retrieve the IDs of the column that contains the selected years' data
  monthCol <- reactive({
    x_range <- input$x_range
    time <- as.numeric(names(rec))
    ab <-  c(c(time >= min(x_range)) & c(time <= max(x_range)))
    r <- range(time[ab], na.rm=TRUE)
    a <- which(time==r[1])
    b <- which(time==r[2])
    colids <- c(a:b)  # column IDs
  })
  
  output$LakeTimeSeries <- renderPlot({
    # validate(need(!is.null(selectedLake), ''))
    # Lake <- NULL
    # if (!is.null(selectedLake)) {Lake <- selectedLake}
    # else{
    #   cat('Please select a lake!')
    #   ixx <- which.max(LakesInBounds()$npix)
    #   Lake <- LakesInBounds()[ixx,]
    # }
    validate(need(!is.null(selectedLake), ''))
    Lake <- selectedLake
    
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
    title(paste("GLWD ID:", title1[1], ", Name:" ,title1[2], "\n Country:", title1[5],", Type:", title1[6]),
         cex.main=1.5, outer=TRUE)
  
    })

  
  output$maskplot <- renderPlot({
    validate(need(!is.null(selectedLake), ''))
    Lake <- selectedLake
    
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
      ups   <- cmeans + 1.96*cses
      downs <- cmeans - 1.96*cses
      for (kk in 1:9){
        lines(ti, cmeans[kk,], lty=1, lwd=2, col=groupcols[kk])
        up  <- ups[kk, ]
        down <- downs[kk,] 
        polygon(c(ti, rev(ti)), c(up, rev(down)), col=makeTransparent(groupcols[kk], alpha=60), 
                border=makeTransparent(groupcols[kk], alpha=60), lwd=2)
      }
    
      par(fig=c(x1=6.8/10, x2=1, y1=0, y2=1), new=T)
      plot(0, 1, type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
      legend(cex=1.25, "topleft", bty="n", legend=groupdat$Name[order(c(4, 5, 9, 1, 2, 3, 6, 8, 7))], 
           pch=19, col=groupcols[1:9], title="Group name")
    }})
  
  
  ## (3) For the data table
  output$contents <- DT::renderDataTable(datatable(tab, filter = 'top'))  
  
  
  ## (4) For the GAM model 
  GamData <- eventReactive(input$gam, {
    fitGam <- NULL
    validate(need(!is.null(selectedLake), ''))
    Lake <- isolate(selectedLake)
    
    if (!is.null(Lake)) {
      Lakeid <- Lake[1,1]
      Lakename <- Lake[1,2]
      id <- which(newalldat$GloboLakes_ID==Lakeid)
      lswt <- as.vector(data.matrix(rec[id,]))
      decimal_month <- time - floor(time)
      data_mat <- data.frame(lswt, time, decimal_month) 
      fitGam <- gam(lswt ~ s(time, bs='ps') + s(decimal_month, bs='cc'), data=data_mat)
    }
    
    list(fitGam, Lakename)
  })
  
  
  output$GamSummary <- renderPrint({
    fitGam <- GamData()[[1]]
    fitname <- GamData()[[2]]
    if (!is.null(fitGam)) {
      print(paste('Here is the fitted model for lake', fitname))
      summary(fitGam)
    }
    else {print('Please select a lake!')}
  })
  
  
  output$GamPlot <- renderPlot({
    fitGam <- GamData()[[1]]
    par(mfrow=c(1,2), cex=1.3)
    if (!is.null(fitGam)) {
      plot(fitGam, ylab='', lwd=2)
    }
    else {plot(0, 1, type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")}
  })

}




