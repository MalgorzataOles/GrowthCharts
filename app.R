library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
# library(plotly)
library(lubridate)

source("R/utils.R")

charts = list(olaf = loadChart("data/Curves_PolandOLAF.xlsx"),
              waw = loadChart("data/Curves_Warsaw2001.xlsx"),
              who1 = loadChart("data/Curves_WHO.0-3.xlsx"),
              who2 = loadChart("data/Curves_WHO.2-20.xlsx"))




#-----------------------------------------------------------------------------
ui <- page_sidebar(theme = bs_theme(preset="sandstone"),
                   # SIDEBAR
                   sidebar=sidebar(
                     fileInput("inFile", "Data File", multiple=FALSE,
                               accept=".xlsx"),
                     textOutput("uploadStatus"),
                     radioButtons("inFeature", "Feature",
                                  choices=c("height", "weight", "head", "BMI"),
                                  selected="height"),
                     sliderInput("inAge", "Age", min=0, max=20, value=c(0,20))),
                   # MAIN
                   navset_card_tab(
                     nav_panel("WHO",
                               card(tags$b("0-20"), plotOutput("plotWHO1")),
                               card("0-3", plotOutput("plotWHO2"))),
                     nav_panel("Warsaw_2001", plotOutput("plotWAW")),
                     nav_panel("Poland_OLAF", plotOutput("plotOLA")))
)

#-----------------------------------------------------------------------------
server <- function(input, output) {
  
  #----------------------
  # reactive values
  #----------------------
  # feature to plot
  feature = reactive({input$inFeature})
  # x-axis breaks
  breakFreq = reactive({
    span = input$inAge[2] - input$inAge[1]
    ifelse(span<10, 1, ifelse(span<15, 2, 5))
  })
  # y-axis label
  featUnit = reactive({
    switch(feature(),
           height="Height [cm]", weight="Weight [kg]",
           head="Circumference [cm]", BMI="BMI [kg/m^2]")
  })
  # input measurements
  data = reactive({
    req(input$inFile)
    loadMeasurements(input$inFile)
    })
  # assign colors to names
  colAssign = reactive({
    req(input$inFile)
    nm = unique(data()$NAME)
    lnm = length(nm)
    if(lnm<=lcolDef)
      setNames(colDef[1:lnm], nm=nm)
    else {
      set.seed(1364)
      setNames(c(colDef, sample(colors(), lnm-lcolDef)), nm=nm)
    }
  })

  #----------------------
  # plots
  output$plotOLA <- renderPlot({
    plotDF = charts$olaf %>% filter(FEATURE==feature())
    gg = ggplot() +
      geom_smooth(plotDF,
                  mapping=aes(x=YEARS, y=VALUE, color=PERCENTILE),
                  method="gam", se=FALSE, linewidth=0.25) +
      theme_bw() +
      scale_x_continuous(limits=c(pmax(input$inAge[1], min(plotDF$YEARS)),
                                  pmin(input$inAge[2], max(plotDF$YEARS))),
                        breaks=seq(0,20, breakFreq())) +
      xlab("Time [years]") + ylab(featUnit()) +
      facet_grid(FEATURE~SEX, scales="free_y")
    if(!is.null(input$inFile)) {
      gg = gg +
        geom_point(data(), mapping=aes(x=AGE, y=get(toupper(feature())), fill=NAME), shape=21) +
        scale_fill_manual(values=colAssign())
    }
    gg
  })
}

#-----------------------------------------------------------------------------
shinyApp(ui=ui, server=server)
