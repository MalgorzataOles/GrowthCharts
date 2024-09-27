library(shiny)
library(ggplot2)
library(reshape2)
library(readxl)
library(RColorBrewer)

# Create the temporary data frame 'tmp' to use in growth_data
tmp <- data.frame(
  Age = seq(84, 216, by = 12),  # Age in months
  L = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  M = c(124.5763, 130.5084, 136.2700, 141.4685, 146.7490, 152.9406,
        160.2044, 167.2116, 172.4996, 175.7266, 177.6036, 178.6756),
  S = c(0.0407, 0.0422, 0.0441, 0.0457, 0.0473, 0.0499, 0.0511,
        0.0481, 0.043, 0.0392, 0.037, 0.0357)
)

# Define growth_data as specified
growth_data = list(
  "WHO"=list(
    Male=list(
      Height=tmp,
      Weight=tmp,
      BMI=tmp
    ),
    Female=list(
      Height=tmp,
      Weight=tmp,
      BMI=tmp
    )
  ),
  "Poland 2000"=list(
    Male=list(
      Height=tmp,
      Weight=tmp,
      BMI=tmp
    ),
    Female=list(
      Height=tmp,
      Weight=tmp,
      BMI=tmp
    )
  )
)

# Z-scores for the desired percentiles
percentiles <- c(0.03, 0.1, 0.25, 0.5, 0.75, 0.9, 0.97)
z_scores <- qnorm(percentiles)

# LMS method to calculate values based on Z-scores
calculate_growth_curve <- function(L, M, S, Z) {
  M * (1 + L * S * Z)^(1 / L)
}

# Define distinct colors for percentiles
percentile_colors <- c("P3" = "red", "P10" = "orange", "P25" = "yellow", 
                       "P50" = "blue", "P75" = "green", "P90" = "purple", 
                       "P97" = "pink")

# UI for Shiny app
ui <- fluidPage(
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

# Server logic for Shiny app
server <- function(input, output, session) {
  
  # Reactive value to store colors for people
  colors <- reactiveVal(NULL)
  
  # Function to update checkbox group based on the "General" sheet and selected sex
  update_people_choices <- function(general_data) {
    # Filter names based on the selected sex
    people_choices <- general_data$Name[general_data$Sex == input$sex & 
                                          general_data$Name %in% excel_sheets(input$file$datapath)]
    updateCheckboxGroupInput(session, "people", choices = people_choices)
    
    # Generate distinct colors for each person
    color_palette <- RColorBrewer::brewer.pal(length(people_choices), "Set1")
    color_mapping <- setNames(color_palette, people_choices)
    colors(color_mapping)  # Update the reactive value
  }
  
  # Observe when a file is uploaded
  observeEvent(input$file, {
    req(input$file)  # Ensure a file has been uploaded
    
    # Read the Excel file
    general_data <- read_excel(input$file$datapath, sheet = "General")  # Read the "General" sheet
    
    # Initial update of checkbox group based on the uploaded file
    update_people_choices(general_data)
  })
  
  # Observe changes in the radio button for sex
  observeEvent(input$sex, {
    req(input$file)  # Ensure a file has been uploaded before filtering
    general_data <- read_excel(input$file$datapath, sheet = "General")  # Read the "General" sheet again
    update_people_choices(general_data)  # Update checkbox group based on selected sex
  })
  
  # Reactive expression to update growth curve based on age and sex
  output$growthPlot <- renderPlot({
    selected_data <- growth_data[[input$chart]][[input$sex]][[input$feature]]
    
    # For each Z-score, calculate the corresponding curve
    growth_curves <- data.frame(Age = selected_data$Age / 12)  # Convert Age to years
    for (i in seq_along(z_scores)) {
      growth_curves[[paste0("P", percentiles[i] * 100)]] <- 
        mapply(calculate_growth_curve, selected_data$L, selected_data$M, selected_data$S, Z = z_scores[i])
    }
    
    # Reshape data for plotting
    growth_curves_long <- melt(growth_curves, id.vars = "Age", variable.name = "Percentile", value.name = "Value")
    
    # Create the base plot
    p <- ggplot(growth_curves_long, aes(x = Age, y = Value, color = Percentile)) +
      geom_line(size = 1) +
      labs(title = paste("Growth Curves for", input$chart, "-", input$sex, "-", input$feature),
           x = "Age (Years)", y = input$feature) +
      scale_color_manual(values = percentile_colors) +  # Keep percentile colors distinct
      xlim(input$age[1], input$age[2]) +  # Set x-axis limits based on age slider (in years)
      theme_minimal()
    
    # If people are selected, read measurement data and add to the plot
    if (!is.null(input$people) && length(input$people) > 0) {
      general_data <- read_excel(input$file$datapath, sheet = "General")  # Read the "General" sheet again
      measurement_data_list <- lapply(input$people, function(person) {
        # Read each person's measurement sheet
        person_data <- read_excel(input$file$datapath, sheet = person)
        
        # Calculate age for each measurement
        birthday <- general_data$Birthday[general_data$Name == person]
        person_data$Age <- as.numeric(difftime(person_data$Date, birthday, units = "weeks")) / 4.345 / 12  # Convert weeks to months, then to years
        
        # Return the relevant feature data
        if (input$feature == "BMI") {
          # Calculate BMI
          person_data$BMI <- person_data$Weight / (person_data$Height / 100)^2  # Height in meters
          return(person_data[, c("Age", "BMI")])
        } else {
          return(person_data[, c("Age", input$feature)])
        }
      })
      
      # Combine measurement data into a single data frame
      combined_measurement_data <- do.call(rbind, lapply(seq_along(measurement_data_list), function(i) {
        df <- measurement_data_list[[i]]
        df$Name <- input$people[i]
        return(df)
      }))
      
      # Calculate percentiles for the combined measurement data
      combined_measurement_data$Percentile <- NA
      for (i in 1:nrow(combined_measurement_data)) {
        measurement_value <- combined_measurement_data[i, input$feature]
        age_years <- combined_measurement_data$Age[i]
        
        # Find the row in the growth curve data corresponding to the measurement's age
        growth_row <- growth_curves[growth_curves$Age == age_years,]
        
        # Only calculate percentile if the growth_row exists
        if (nrow(growth_row) > 0) {
          for (j in seq_along(z_scores)) {
            # Get the growth value for the specific percentile
            growth_value <- growth_row[[paste0("P", percentiles[j] * 100)]]
            combined_measurement_data$Percentile[i] <- sum(measurement_value > growth_value) / length(growth_row) * 100
          }
        }
      }
      
      # Add points and lines for measurements
      p <- p + geom_point(data = combined_measurement_data, aes(x = Age, y = get(input$feature), color = Name), size = 2) +
        geom_line(data = combined_measurement_data, aes(x = Age, y = get(input$feature), color = Name), linetype = "dashed") +
        scale_color_manual(values = c(percentile_colors, colors()))  # Keep percentile colors and add person colors
      
      # Annotate percentiles above each measurement point
      p <- p + geom_text(data = combined_measurement_data,
                         aes(x = Age, y = get(input$feature), 
                             label = paste0(round(Percentile, 2), "%")),
                         vjust = -1, size = 3, color = "black")
    }
    
    # Print the final plot
    print(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
