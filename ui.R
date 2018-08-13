library(shiny)
library(leaflet)
library(magrittr)
library(raster)
library(maps)
library(DT)
library(mgcv)


# colours used to represent different groups
# "Dark blue", "Mid blue", "Light blue", "Yellow", "Orange", "Red", "Pink", "Light Green", "Dark Green"

fluidPage(
    # tags$head(includeScript("google-analytics.js")),
    fluidRow(
      column(5, offset=1,
             h2('GloboLakes','ARCLAKE Time Series')),
    
      column(5, 
             br(),
             br(),
             textOutput('center')
      )),
    
    # tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
    tags$head(tags$link(rel='stylesheet', href='https://unpkg.com/leaflet@1.3.1/dist/leaflet.css')),
    # tags$script(src="https://unpkg.com/@mapbox/mapbox-sdk/umd/mapbox-sdk.js"),
    
    leaflet::leafletOutput('basemap', width="100%", height=450),
    
    hr(),
    sidebarLayout(

      sidebarPanel(width=3,
        
        # For the main panel
        sliderInput(inputId = "x_range",
                  label = paste("Time Range"),
                  min = 1995, max = 2012, sep="",
                  value = c(1995, 2012)),
      
        checkboxInput('lakespecmean', 'Show group mean curve for this lake', FALSE),
      
      
        selectizeInput('addmean', 
                label=strong("Add Group Mean Curves"),
                choices=c('G4 Dark blue'=4, 'G5 Mid blue'=5, 'G9 Light blue'=9, 'G1 Yellow'=1,  
                          'G2 Orange'=2, 'G3 Red'=3, 'G6 Pink'=6, 'G8 Light green'=8, 'G7 Dark green'=7), 
                multiple = TRUE,
                options = list(
                placeholder = 'Please select',
                onInitialize = I('function() { this.setValue(""); }'))),
      
        
        checkboxInput('allmeans', 'Show plot of all lake Mean Curves', FALSE),
        
        
        checkboxInput('lakemask', 'Show plot of lake mask', FALSE)
        
    ),
    
    
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Main",  plotOutput('LakeTimeSeries'),
               conditionalPanel("input.allmeans == TRUE", plotOutput('AllMeanCurves', height = "500px")),
               conditionalPanel("input.lakemask == TRUE", plotOutput('maskplot', height = "600px"))
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
                  p("We can look at the effective degrees of freedom (edf) of the smooth functions of time and month
                     to see if any of these are significantly non-constant. For time series data, an edf greater than 1 
                     suggests that there is certain linear or curved feature. We can also plot the fitted smooth functions
                     to have a better idea of this linear or curved feature.",
                     style = 'color:gray'),
                  hr()),
           column(1)
               
      ),
      
      fluidRow(
        column(11,
               # conditionalPanel("is.null(input['gam']) == FALSE", ...) 
               # input['xx'] to get the values
               br(),
               verbatimTextOutput("GamSummary"),
               plotOutput("GamPlot", height = "380px")),
        column(1)
      )),
      
      
      tabPanel('Data',  DT::dataTableOutput("contents")),
     
      
      tabPanel("About",  includeHTML('APPTAB2.html'))   # includeMarkdown('APPTAB2.md')
    ))
  )

)


