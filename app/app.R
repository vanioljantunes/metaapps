# Define UI
ui <- fluidPage(
  titlePanel("Editable Spreadsheet with Forest Plot"),
  fluidRow(
    # Left column: Buttons and Editable Table
    column(6,  # Half the width
           actionButton("save", "Save Changes"),
           actionButton("reset", "Reset Table"),
           actionButton("run_code", "Run Code"),
           br(), br(),
           rHandsontableOutput("editableTable")
    ),
    # Right column: Forest Plot Canvas
    column(6,  # Half the width
           h4("Forest Plot:"),
           plotOutput("resultCanvas")  # Output area for the forest plot
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Initialize dataset with predefined columns and 5 empty rows
  initial_data <- data.frame(
    Study = paste("Outcome", 1:6),
    Estimate = c(0.2, 0.5, 0.7, -0.3, -0.1, 0.4),
    Lower_CI = c(0.1, 0.3, 0.5, -0.5, -0.3, 0.2),
    Upper_CI = c(0.3, 0.7, 0.9, -0.1, 0.1, 0.6),
    p_value = c(0.03, 0.01, 0.01, 0.04, 0.2, 0.04),
    stringsAsFactors = FALSE
  )
  
  # Reactive data storage
  values <- reactiveValues(data = initial_data)
  
  # Render editable table
  output$editableTable <- renderRHandsontable({
    rhandsontable(values$data, rowHeaders = NULL)
  })
  
  # Update data on table edit
  observeEvent(input$editableTable, {
    if (!is.null(input$editableTable)) {
      values$data <- hot_to_r(input$editableTable)
    }
  })
  
  # Save data (you can replace `print` with file-saving logic)
  observeEvent(input$save, {
    print("Saving data:")
    print(values$data)
    # Example: write.csv(values$data, "output.csv", row.names = FALSE)
  })
  
  # Reset to initial table
  observeEvent(input$reset, {
    values$data <- initial_data
  })
  
  # Display forest plot when "Run Code" is pressed
  observeEvent(input$run_code, {
    output$resultCanvas <- renderPlot({
      # Prepare the plot dynamically using current table data
      data <- values$data %>%
        arrange(-Estimate) %>%
        mutate(Study = factor(Study, levels = Study))  # Reorder Study factor levels
      
      # Define MetBrewer palette
      met_palette <- met.brewer("Hokusai2")
      
      # Calculate extra space for the X-axis dynamically
      x_padding <- max(data$Upper_CI, na.rm = TRUE) * 0.3  # 30% extra space for annotations
      x_limit <- c(min(data$Lower_CI, na.rm = TRUE), max(data$Upper_CI, na.rm = TRUE) + x_padding)
      
      # Generate the forest plot
      plot <- ggplot(data, aes(x = Estimate, y = Study)) +
        # Customized error bars
        geom_errorbarh(
          aes(xmin = Lower_CI, xmax = Upper_CI, color = Estimate),
          height = 0.2,
          size = 1
        ) +
        # Customized points
        geom_point(
          aes(fill = Estimate),
          size = 4,
          shape = 23,
          color = "black",
          stroke = 0.8
        ) +
        # Null hypothesis line
        geom_vline(aes(xintercept = 0, linetype = "Null hypothesis"), 
                   color = met_palette[3], size = 1) +
        # Gradient scale for points
        scale_fill_gradientn(colors = met_palette, name = "Effect Size", guide = "colorbar") +
        scale_color_gradientn(colors = met_palette, guide = "none") +
        scale_linetype_manual(name = "", values = c("Null hypothesis" = "solid")) +
        # Add p_value annotations
        geom_text(
          aes(label = sprintf("p = %.2f", p_value), x = max(Upper_CI) + x_padding / 4),
          hjust = 0,
          size = 4,
          color = "black"
        ) +
        # Expand X-axis limits
        scale_x_continuous(limits = x_limit, expand = c(0, 0)) +
        # Custom theme
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.text.y = element_text(color = "black", size = 12),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.position = "bottom"
        )
      # Return the plot
      print(plot)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)