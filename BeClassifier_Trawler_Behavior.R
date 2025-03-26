library(shiny)
library(dplyr)
library(ggplot2)
library(scales)

ui <- fluidPage(
  titlePanel("Escape Panel Activation with Light"),
  
  tabsetPanel(
    tabPanel("Live Detection",
             sidebarLayout(
               sidebarPanel(
                 fileInput("csv_file", "Upload Fish Tracks CSV", accept = ".csv"),
                 uiOutput("frameSliderUI"),
                 h4("Cod Detection Status"),
                 uiOutput("lightStatus"),
                 verbatimTextOutput("codCount")
               ),
               mainPanel(
                 plotOutput("framePlot", height = "400px")
               )
             )
    ),
    
    tabPanel("Catch Over Time",
             fluidRow(
               column(8, plotOutput("catchPlot", height = "300px")),
               column(4, plotOutput("catchPie", height = "300px"))
             ),
             verbatimTextOutput("catchTotal")
    ),
    
    tabPanel("Behavior Analysis",
             plotOutput("behaviorPlot", height = "300px"),
             verbatimTextOutput("behaviorText")
    )
  )
)

server <- function(input, output, session) {
  
  danger_x <- 0.6
  danger_y <- 0.8
  danger_radius <- 0.1
  
  data <- reactive({
    req(input$csv_file)
    df <- read.csv(input$csv_file$datapath)
    validate(need(all(c("frame", "fish_id", "species", "x", "y") %in% colnames(df)),
                  "CSV must have columns: frame, fish_id, species, x, y"))
    df
  })
  
  max_frame <- reactive({
    req(data())
    max(data()$frame, na.rm = TRUE)
  })
  
  auto <- reactiveValues(playing = FALSE)
  
  observeEvent(input$play, {
    auto$playing <- TRUE
  })
  
  observeEvent(input$stop, {
    auto$playing <- FALSE
  })
  
  observe({
    invalidateLater(10, session)  # ultra fast animation
    isolate({
      if (auto$playing && !is.null(input$frameSlider)) {
        next_frame <- input$frameSlider + 1
        if (next_frame <= max_frame()) {
          updateSliderInput(session, "frameSlider", value = next_frame)
        } else {
          auto$playing <- FALSE
        }
      }
    })
  })
  
  output$frameSliderUI <- renderUI({
    req(max_frame())
    tagList(
      sliderInput("frameSlider", "Frame", min = 1, max = max_frame(), value = 1, step = 1),
      actionButton("play", "▶ Play"),
      actionButton("stop", "⏹ Stop")
    )
  })
  
  # 🔔 Green light ON only for frames 205–234
  reactiveCodCount <- reactive({
    req(input$frameSlider)
    if (input$frameSlider >= 205 && input$frameSlider <= 234) {
      return(10)
    } else {
      return(0)
    }
  })
  
  output$lightStatus <- renderUI({
    if (reactiveCodCount() >= 10) {
      tags$div(style = "width:100px; height:100px; background-color:green; border-radius:50%;")
    } else {
      tags$div(style = "width:100px; height:100px; background-color:red; border-radius:50%;")
    }
  })
  
  output$codCount <- renderText({
    if (input$frameSlider >= 205 && input$frameSlider <= 234) {
      "Cod light is ON: Active escape panel period (frames 205–234)"
    } else {
      "Cod light is OFF: Outside of active escape window"
    }
  })
  
  output$framePlot <- renderPlot({
    req(data(), input$frameSlider)
    trail_length <- 5
    start_frame <- max(1, input$frameSlider - (trail_length - 1))
    
    trail_data <- data() %>%
      filter(frame >= start_frame, frame <= input$frameSlider) %>%
      dplyr::mutate(alpha = scales::rescale(frame, to = c(0.3, 1), from = c(start_frame, input$frameSlider)))
    
    ggplot(trail_data, aes(x = x, y = y, color = species, group = fish_id)) +
      geom_point(aes(alpha = alpha), size = 3) +
      geom_point(aes(x = danger_x, y = danger_y), color = "red", size = 5, shape = 19) +
      geom_vline(xintercept = 0.7, linetype = "dashed", color = "black", linewidth = 1) +
      scale_alpha_identity() +
      scale_y_reverse() +
      coord_cartesian(ylim = c(1, 0), xlim = c(0, 1)) +
      labs(title = paste("Frame:", input$frameSlider),
           x = "X Position", y = "Y Position") +
      theme_minimal()
  })
  
  output$catchPlot <- renderPlot({
    req(data())
    catch_data <- data() %>%
      dplyr::filter(x > 0.95) %>%
      dplyr::group_by(frame, species) %>%
      dplyr::summarise(count = n(), .groups = "drop")
    
    ggplot(catch_data, aes(x = frame, y = count, color = species)) +
      geom_line(linewidth = 1) +
      labs(title = "Catch Over Time (x > 0.95)", x = "Frame", y = "Catch Count") +
      theme_minimal()
  })
  
  output$catchTotal <- renderText({
    req(data())
    total <- data() %>%
      filter(x > 0.95)
    
    species_count <- total %>%
      dplyr::group_by(species) %>%
      dplyr::summarise(count = n(), .groups = "drop")
    
    total_count <- nrow(total)
    paste0("Total Catch: ", total_count, "\n",
           paste(species_count$species, species_count$count, sep = ": ", collapse = "\n"))
  })
  
  output$catchPie <- renderPlot({
    req(data())
    pie_data <- data() %>%
      dplyr::filter(x > 0.95) %>%
      dplyr::group_by(species) %>%
      dplyr::summarise(count = n(), .groups = "drop")
    
    if (nrow(pie_data) == 0) return(NULL)
    
    ggplot(pie_data, aes(x = "", y = count, fill = species)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      labs(title = "Catch Proportion") +
      theme_void() +
      theme(legend.position = "right")
  })
  
  behavior_analysis <- reactive({
    req(data())
    df <- data() %>%
      arrange(fish_id, frame) %>%
      dplyr::mutate(dist = sqrt((x - danger_x)^2 + (y - danger_y)^2)) %>%
      group_by(fish_id) %>%
      dplyr::mutate(dist_next = lead(dist),
             swimming_away = ifelse(dist < danger_radius & dist_next > dist, TRUE, FALSE)) %>%
      ungroup()
    
    df %>%
      filter(swimming_away) %>%
      group_by(species) %>%
      dplyr::summarise(away_count = n(), .groups = "drop")
  })
  
  output$behaviorPlot <- renderPlot({
    df <- behavior_analysis()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = species, y = away_count, fill = species)) +
      geom_col() +
      labs(title = "Fish Avoidance Behavior",
           x = "Species",
           y = "Swim-Away Events (from red point)") +
      theme_minimal()
  })
  
  output$behaviorText <- renderText({
    df <- behavior_analysis()
    if (nrow(df) == 0) return("No swim-away behavior detected.")
    
    paste("Species showing avoidance behavior:\n",
          paste(df$species, df$away_count, sep = ": ", collapse = "\n"))
  })
}

shinyApp(ui, server)
