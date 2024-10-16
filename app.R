# Author: Dr. Malgorzata Oles

library(shiny)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(readxl)

# load growth data
source("R/readGrowthStandards.R")

# z-scores for the desired percentiles
percentiles <- c(0.03, 0.1, 0.25, 0.5, 0.75, 0.9, 0.97)
z_scores <- qnorm(percentiles)

# LMS method to calculate values based on Z-scores
calculate_growth_curve <- function(L, M, S, Z) {
  M * (1 + L * S * Z)^(1 / L)
}

# define distinct colors for percentiles
percentiles <- c(0.03, 0.1, 0.25, 0.5, 0.75, 0.9, 0.97)
colors_percentiles <- c("P3" = "grey70", "P10" = "grey90", "P25" = "grey90", 
                       "P50" = "grey50", "P75" = "grey90", "P90" = "grey90", 
                       "P97" = "grey70")

#============================================================================
# ui
#============================================================================
ui = fluidPage(
  titlePanel("Growth Curve App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data File"),
      selectInput("chart", "Chart", choices = names(growth_data)),  # Dynamically set choices
      radioButtons("feature", "Feature", choices = c("Height", "Weight", "BMI")),
      sliderInput("age", "Age (Years)", min = 0, max = 18, value = c(7, 18), step = 1, round = TRUE),
      radioButtons("sex", "Sex", choices = c("Male", "Female")),
      checkboxGroupInput("people", "People", choices = NULL)  # Initialize with NULL
    ),
    mainPanel(
      plotOutput("growthPlot")
    )
  )
)

#============================================================================
# server
#============================================================================
server = function(input, output, session) {
  
  # reactive to load the Excel file and read the data
  data_input = reactive({
    req(input$file)
    
    # read the excel file
    file_data = readxl::excel_sheets(input$file$datapath)
    general_data = readxl::read_excel(input$file$datapath, sheet = "General")
    
    # list the people available based on sex selection
    available_people = general_data$Name[general_data$Sex == input$sex]
    
    # get only the sheets that match the people names
    valid_people = available_people[available_people %in% file_data]
    
    # debugging output
    print(paste("Valid People:", paste(valid_people, collapse = ", ")))
    
    # update checkbox group with valid people
    updateCheckboxGroupInput(session, "people", choices=valid_people,
                             selected=valid_people)
    
    # read the individual measurement data for valid people
    people_data = lapply(valid_people, function(person) {
      person_data = readxl::read_excel(input$file$datapath, sheet=person)
      
      # calculate age based on Birthday from the General sheet
      birthday = as.Date(general_data$Birthday[general_data$Name == person],
                         format = "%d.%m.%Y")
      person_data$Age = as.numeric(difftime(
        person_data$Date, birthday, units="days")) / 365.25  # Convert to years
      
      return(person_data)
    })
    
    names(people_data) = valid_people
    return(people_data)
  })
  
  # observe changes to the file input and update checkboxes
  observeEvent(input$file, {
    req(input$file)
    data_input()  # This will trigger the data reading process
    
    general_data = readxl::read_excel(input$file$datapath, sheet = "General")
    available_people = general_data$Name[general_data$Sex == input$sex]
    
    # ensure that file_data is obtained correctly
    file_data = readxl::excel_sheets(input$file$datapath)
    valid_people = available_people[available_people %in% file_data]
    
    # debugging output
    print(paste("Available People:", paste(available_people, collapse=", ")))
    print(paste("Valid People after file check:",paste(valid_people, collapse=", ")))
    
    updateCheckboxGroupInput(session, "people",
                             choices=valid_people, selected=valid_people)
  })
  
  # observe changes to the chart selection and dynamically update 'sex' radio buttons
  observe({
    selected_chart = input$chart
    
    req(growth_data[[selected_chart]])  # Ensure data exists for the selected chart
    
    available_sexes = names(growth_data[[selected_chart]])
    
    if (!(input$sex %in% available_sexes)) {
      updateRadioButtons(session, "sex", choices=available_sexes, selected=available_sexes[1])
    } else {
      updateRadioButtons(session, "sex", choices=available_sexes, selected=input$sex)
    }
  })
  
  # observe changes to the sex selection and dynamically update 'feature' radio buttons
  observe({
    selected_chart = input$chart
    selected_sex = input$sex
    
    req(growth_data[[selected_chart]][[selected_sex]])  # Ensure data exists for the selected chart and sex
    
    available_features = names(growth_data[[selected_chart]][[selected_sex]])
    
    if (!(input$feature %in% available_features)) {
      updateRadioButtons(session, "feature", choices=available_features,
                         selected=available_features[1])
    } else {
      updateRadioButtons(session, "feature", choices=available_features,
                         selected=input$feature)
    }
  })
  
  # observe changes to 'chart', 'sex', or 'feature' and update the 'age' slider
  observe({
    selected_chart = input$chart
    selected_sex = input$sex
    selected_feature = input$feature
    
    req(growth_data[[selected_chart]][[selected_sex]][[selected_feature]])  # Ensure data exists for the selected feature
    
    age_data = growth_data[[selected_chart]][[selected_sex]][[selected_feature]]$Age
    
    # convert the age from months to years
    age_data_years = age_data / 12
    
    # get the range of ages in years
    min_age <- min(age_data_years, na.rm=TRUE)
    max_age <- max(age_data_years, na.rm=TRUE)
    
    # update the sliderInput for age
    updateSliderInput(session,  
                      "age",  
                      min = min_age,  
                      max = max_age,  
                      value = c(min_age, max_age))
  })
  
  # render the plot
  output$growthPlot = renderPlot({
    selected_chart = input$chart
    selected_sex = input$sex
    selected_feature = input$feature
    
    req(growth_data[[selected_chart]][[selected_sex]][[selected_feature]])  # Ensure data exists for rendering
    
    growth_df = growth_data[[selected_chart]][[selected_sex]][[selected_feature]]
    
    # set the age limits from the slider
    age_range = input$age
    growth_df_filtered = growth_df[growth_df$Age / 12 >= age_range[1] &
                                     growth_df$Age / 12 <= age_range[2], ]
    
    # check if filtered data is not empty
    req(nrow(growth_df_filtered) > 0)
    
    # prepare to plot growth curves with ggplot2
    p = ggplot() +
      xlim(min(age_range), max(age_range)) +
      labs(x="Age (years)", y=selected_feature) +
      theme_bw()
    
    # add percentile lines to the plot
    for (i in seq_along(percentiles)) {
      p = p + geom_line(data=growth_df_filtered,
                        aes_string(x = "Age / 12",
                                   y=sprintf("M * (1 + S * qnorm(%f))", percentiles[i])),
                        color=I(colors_percentiles[i]))
    }
    
    # if people are selected, plot their data points on the same graph
    if (!is.null(input$people)) {
      people_data = data_input()
      colors = rainbow(length(input$people))
      for (i in seq_along(input$people)) {
        person = input$people[i]
        person_data = people_data[[person]]
        
        # ensure the person_data is not NULL
        req(!is.null(person_data))
        
        # if the feature is BMI, calculate BMI from Height and Weight
        if (selected_feature == "BMI") {
          person_data$BMI = person_data$Weight / (person_data$Height / 100)^2
          y_data = person_data$BMI
        } else {
          y_data = person_data[[selected_feature]]
        }
        
        # combine person data into a dataframe for ggplot
        person_plot_data = data.frame(Age=person_data$Age, Value=y_data)
       
        # calculate percentiles
        calc_perc = function(df) {
          # index of closest data row in standards
          df$idx = sapply(df$Age*12, function(x)
            which.min(abs(growth_df_filtered$Age - x)))
          df %>%
            mutate(L=growth_df_filtered$L[idx],
                   M=growth_df_filtered$M[idx],
                   S=growth_df_filtered$S[idx]) %>%
            mutate(zscore=(((Value / M)^L) - 1) / (S * L),
                   percentile=pnorm(zscore) * 100)
        }
        person_plot_data = calc_perc(person_plot_data)
        
        # remove NA values to make line continuous
        person_plot_data = person_plot_data %>%
          filter(!is.na(Value))
        
        # plot points and lines for each person
        p = p + geom_point(data = person_plot_data, aes(x=Age, y=Value), color=colors[i]) +
          geom_line(data=person_plot_data, aes(x=Age, y=Value), color=colors[i], lwd=1) +
          geom_text(data=person_plot_data, aes(x=Age, y=Value, label=sprintf("%.2f%%", percentile)),
                    color=colors[i], vjust=-0.5)
      }
    }
    print(p)
  })
}

#============================================================================
# run app
#============================================================================
shinyApp(ui=ui, server=server)
