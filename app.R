library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(plotly)

source("R/utils.R")

charts = list(olaf = loadChart("data/Curves_PolandOLAF.xlsx"),
              waw = loadChart("data/Curves_Warsaw2001.xlsx"),
              who1 = loadChart("data/Curves_WHO.0-3.xlsx"),
              who2 = loadChart("data/Curves_WHO.2-20.xlsx"))


#-----------------------------------------------------------------------------
ui <- page_sidebar(theme = bs_theme(preset="sandstone"),
                   # SIDEBAR
                   sidebar=sidebar(
                     fileInput("inFile", "Data File"),
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
  feature = reactive({input$inFeature})
  breakFreq = reactive({
    span = input$inAge[2] - input$inAge[1]
    ifelse(span<10, 1, ifelse(span<15, 2, 5))
  })
  featUnit = reactive({
    switch(feature(),
           height="Height [cm]", weight="Weight [kg]",
           head="Circumference [cm]", BMI="BMI [kg/m^2]")
  })

  #----------------------
  # plots
  output$plotOLA <- renderPlot({
    plotDF = charts$olaf %>% filter(FEATURE==feature())
    ggplot() +
      geom_smooth(plotDF,
                  mapping=aes(x=YEARS, y=VALUE, color=PERCENTILE),
                  method="gam", se=FALSE, linewidth=0.25) +
      theme_bw() +
      scale_x_continuous(limits=c(pmax(input$inAge[1], min(plotDF$YEARS)),
                                  pmin(input$inAge[2], max(plotDF$YEARS))),
                        breaks=seq(0,20, breakFreq())) +
      xlab("Time [years]") + ylab(featUnit())
  })
}

#-----------------------------------------------------------------------------
shinyApp(ui=ui, server=server)
