library(shiny)
library(shinythemes)
library(tidyverse)
library(nflplotR)
library(readr)
library(zoo)
library(ggplot2)

# Define divisions for each conference (loaded dynamically later)
# Define playoff week labels and values
playoff_week_labels <- c("WC", "DIV", "CONF", "SB")
playoff_week_values <- c(19, 20, 21, 22)
names(playoff_week_values) <- playoff_week_labels

# UI Definition (unchanged)
ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$head(
    tags$style(HTML("
      body { font-family: 'Arial', sans-serif; color: #f0f0f0; }
      .shiny-input-container { color: #f0f0f0; }
      h1, h2, h3, h4, h5, h6 { color: #ffffff; }
      table { color: #f0f0f0; background-color: #333333; border-color: #555555; }
      th, td { border-color: #555555; }
      .percentile-0 { color: #FF0000; }
      .percentile-25 { color: #FF6600; }
      .percentile-50 { color: #FFA500; }
      .percentile-75 { color: #99CC00; }
      .percentile-100 { color: #00FF00; }
    "))
  ),
  titlePanel("OASIS (Opponent-Adjusted Situational Impact Score) 2024 Season"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h3("What is OASIS?"),
      p("OASIS (Opponent-Adjusted Situational Impact Score) is an NFL statistic designed to improve upon Expected Points Added (EPA)."),
      p("OASIS adjusts for both the opponent and the game situation. A 10-yard gain in a close 4th-quarter game against an elite defense carries more weight than the same gain in garbage time against a weak defense."),
      h4("How is OASIS Calculated?"),
      p("OASIS builds on EPA by adding two key adjustments:"),
      tags$ul(
        tags$li("Play-by-play EPA is first weighted by game leverage (win probability impact)."),
        tags$li("Opponent strength is determined using market-implied scores from betting lines."),
        tags$li("Offensive and defensive performance is adjusted based on the strength of the opponent they faced.")
      ),
      h4("How to Read the Chart"),
      p("Each NFL logo represents a team’s performance. The axes indicate how teams compare in offensive and defensive OASIS scores."),
      tags$ul(
        tags$li("X-Axis (OASIS Defense): Higher means better defensive performance."),
        tags$li("Y-Axis (OASIS Offense): Higher means better offensive performance."),
        tags$li("Top-Right: Strong offense and defense."),
        tags$li("Top-Left: Strong offense, weaker defense."),
        tags$li("Bottom-Right: Strong defense, weaker offense."),
        tags$li("Bottom-Left: Struggling in both areas.")
      ),
      h3("Filters"),
      sliderInput("reg_week", "Regular Season Week:", min = 1, max = 18, value = c(1, 18)),
      checkboxInput("include_playoffs", "Include Playoffs", value = FALSE),
      conditionalPanel(
        condition = "input.include_playoffs == true",
        sliderInput(
          "playoff_week",
          "Playoff Week:",
          min = 1,
          max = 4,
          value = c(1, 4),
          step = 1,
          ticks = FALSE,
          width = "100%"
        )
      ),
      checkboxGroupInput("conferences", "Conference:", choices = c("NFC", "AFC"), selected = c("NFC", "AFC")),
      checkboxGroupInput("divisions", "Division:", choices = NULL, selected = NULL),
      checkboxGroupInput("downs", "Down:", choices = c(1, 2, 3, 4), selected = c(1, 2, 3, 4))
    ),
    mainPanel(
      width = 8,
      selectInput("team_select", "Select Team for Weekly Breakdown:", choices = NULL, selected = "BUF", width = "300px"),
      fluidRow(
        column(8,
               plotOutput("oasisPlot", width = "100%", height = "800px"),
               h4("Overall Team Percentiles"),
               tableOutput("percentileTable")
        ),
        column(4,
               h4(textOutput("weeklyHeader")),
               tableOutput("weeklyBreakdown")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Load data reactively
  pbp_2024 <- reactive({ read.csv("pbp_2024_slim.csv") })
  team_strength <- reactive({ read.csv("team_strength.csv") })
  season_off_oasis <- reactive({ read.csv("season_off_oasis.csv") })
  season_def_oasis <- reactive({ read.csv("season_def_oasis.csv") })
  
  # Define divisions and update inputs dynamically
  observe({
    ts <- team_strength()
    nfc_divisions <- unique(ts$team_division[ts$team_conf == "NFC"])
    afc_divisions <- unique(ts$team_division[ts$team_conf == "AFC"])
    all_divisions <- unique(ts$team_division)
    updateCheckboxGroupInput(session, "divisions", choices = all_divisions, selected = all_divisions)
    updateSelectInput(session, "team_select", choices = sort(unique(ts$team)), selected = "BUF")
  })
  
  # Precompute ECDFs
  off_ecdf <- reactive({ ecdf(season_off_oasis()$off_oasis) })
  def_ecdf <- reactive({ ecdf(season_def_oasis()$def_oasis) })
  
  # Observe conference and division logic (unchanged)
  observe({
    if (!is.null(input$conferences)) {
      selected_conferences <- input$conferences
      ts <- team_strength()
      if (length(selected_conferences) == 0) {
        updateCheckboxGroupInput(session, "divisions", selected = character(0))
      } else {
        selected_divisions <- character(0)
        if ("NFC" %in% selected_conferences) {
          selected_divisions <- c(selected_divisions, unique(ts$team_division[ts$team_conf == "NFC"]))
        }
        if ("AFC" %in% selected_conferences) {
          selected_divisions <- c(selected_divisions, unique(ts$team_division[ts$team_conf == "AFC"]))
        }
        updateCheckboxGroupInput(session, "divisions", selected = selected_divisions)
      }
    }
  })
  
  observe({
    if (!is.null(input$divisions)) {
      selected_divisions <- input$divisions
      ts <- team_strength()
      selected_conferences <- character(0)
      if (any(unique(ts$team_division[ts$team_conf == "NFC"]) %in% selected_divisions)) {
        selected_conferences <- c(selected_conferences, "NFC")
      }
      if (any(unique(ts$team_division[ts$team_conf == "AFC"]) %in% selected_divisions)) {
        selected_conferences <- c(selected_conferences, "AFC")
      }
      updateCheckboxGroupInput(session, "conferences", selected = selected_conferences)
    }
  })
  
  # Filtered team data
  filtered_team_strength <- reactive({
    req(input$conferences, input$divisions)
    team_strength() %>%
      filter(team_conf %in% input$conferences,
             team_division %in% input$divisions)
  })
  
  # Filtered data (adjusted to use reactive data)
  filtered_data <- reactive({
    req(input$reg_week, input$downs)
    withProgress(message = "Filtering data...", value = 0, {
      pbp <- pbp_2024()
      reg_season_filter <- (pbp$week >= input$reg_week[1] & pbp$week <= input$reg_week[2])
      playoff_filter <- FALSE
      if (input$include_playoffs) {
        req(input$playoff_week)
        playoff_weeks_slider <- input$playoff_week
        playoff_weeks <- playoff_week_values[playoff_weeks_slider[1]:playoff_weeks_slider[2]]
        playoff_filter <- (pbp$week %in% playoff_weeks)
      }
      
      filtered_pbp <- pbp %>%
        filter(reg_season_filter | playoff_filter,
               down %in% as.numeric(input$downs)) %>%
        left_join(filtered_team_strength() %>% select(team, def_strength_z), by = c("defteam" = "team")) %>%
        left_join(filtered_team_strength() %>% select(team, off_strength_z), by = c("posteam" = "team")) %>%
        left_join(filtered_team_strength() %>% select(team, off_strength_z), by = c("defteam" = "team"), suffix = c("_posteam", "_defteam"))
    })
  })
  
  # wp_epa_df (adjusted)
  wp_epa_df <- reactive({
    req(filtered_data())
    filtered_teams <- filtered_team_strength()$team
    pbp <- filtered_data() %>%
      filter(posteam %in% filtered_teams, defteam %in% filtered_teams)
    
    validate(need(nrow(pbp) > 0, "No data available based on current filters."))
    
    pbp <- pbp %>%
      mutate(
        wp_change = abs(wpa),
        leverage_factor = 1 + 3 * wp_change,
        off_weight = exp(0.2 * def_strength_z),
        def_weight = exp(0.2 * off_strength_z_defteam),
        adjusted_off_epa = epa * leverage_factor * off_weight,
        adjusted_def_epa = -epa * leverage_factor * def_weight
      )
    
    team_adjusted_off_epa <- pbp %>%
      group_by(posteam) %>%
      summarize(
        adjusted_off_epa_per_play = sum(adjusted_off_epa, na.rm = TRUE) / sum(leverage_factor * off_weight, na.rm = TRUE),
        .groups = 'drop'
      )
    
    team_adjusted_def_epa <- pbp %>%
      group_by(defteam) %>%
      summarize(
        adjusted_def_epa_per_play = sum(adjusted_def_epa, na.rm = TRUE) / sum(leverage_factor * def_weight, na.rm = TRUE),
        .groups = 'drop'
      )
    
    wp_epa_df <- team_adjusted_off_epa %>%
      inner_join(team_adjusted_def_epa, by = c("posteam" = "defteam")) %>%
      rename(team = posteam) %>%
      mutate(
        oasis_offense = adjusted_off_epa_per_play,
        oasis_defense = adjusted_def_epa_per_play,
        off_percentile = percent_rank(oasis_offense) * 100,
        def_percentile = percent_rank(oasis_defense) * 100,
        selected = team == input$team_select
      )
    return(wp_epa_df)
  })
  
  # Weekly breakdown (adjusted)
  weekly_breakdown <- reactive({
    withProgress(message = "Calculating weekly breakdown...", value = 0, {
      pbp <- pbp_2024()
      ts <- team_strength()
      off_data <- pbp %>%
        filter(!is.na(epa), !is.na(wp), !is.na(wpa)) %>%
        left_join(ts %>% select(team, def_strength_z), by = c("defteam" = "team")) %>%
        mutate(
          wp_change = abs(wpa),
          leverage_factor = 1 + 3 * wp_change,
          off_weight = exp(0.2 * def_strength_z),
          adjusted_off_epa = epa * leverage_factor * off_weight
        ) %>%
        group_by(posteam, week) %>%
        summarize(
          off_oasis = sum(adjusted_off_epa, na.rm = TRUE) / sum(leverage_factor * off_weight, na.rm = TRUE),
          opponent = first(defteam),
          .groups = 'drop'
        ) %>%
        rename(team = posteam) %>%
        filter(team == input$team_select)
      
      def_data <- pbp %>%
        filter(!is.na(epa), !is.na(wp), !is.na(wpa)) %>%
        left_join(ts %>% select(team, off_strength_z), by = c("defteam" = "team")) %>%
        mutate(
          wp_change = abs(wpa),
          leverage_factor = 1 + 3 * wp_change,
          def_weight = exp(0.2 * off_strength_z),
          adjusted_def_epa = -epa * leverage_factor * def_weight
        ) %>%
        group_by(defteam, week) %>%
        summarize(
          def_oasis = sum(adjusted_def_epa, na.rm = TRUE) / sum(leverage_factor * def_weight, na.rm = TRUE),
          opponent = first(posteam),
          .groups = 'drop'
        ) %>%
        rename(team = defteam) %>%
        filter(team == input$team_select)
      
      off_data <- off_data %>%
        mutate(off_percentile = round(off_ecdf()(off_oasis) * 100)) %>%
        select(week, opponent, off_percentile)
      
      def_data <- def_data %>%
        mutate(def_percentile = round(def_ecdf()(def_oasis) * 100)) %>%
        select(week, opponent, def_percentile)
      
      weekly_data <- off_data %>%
        left_join(def_data, by = c("week", "opponent")) %>%
        arrange(week) %>%
        mutate(
          off_class = case_when(
            off_percentile <= 25 ~ "percentile-25",
            off_percentile <= 50 ~ "percentile-50",
            off_percentile <= 75 ~ "percentile-75",
            TRUE ~ "percentile-100"
          ),
          def_class = case_when(
            def_percentile <= 25 ~ "percentile-25",
            def_percentile <= 50 ~ "percentile-50",
            def_percentile <= 75 ~ "percentile-75",
            TRUE ~ "percentile-100"
          ),
          "Offensive Percentile" = sprintf('<span class="%s">%d</span>', off_class, off_percentile),
          "Defensive Percentile" = sprintf('<span class="%s">%d</span>', def_class, def_percentile)
        ) %>%
        select(week, opponent, "Offensive Percentile", "Defensive Percentile") %>%
        rename("Week" = week, "Opponent" = opponent)
      return(weekly_data)
    })
  })
  
  # Outputs (unchanged)
  output$oasisPlot <- renderPlot({
    data <- wp_epa_df()
    min_val <- min(min(data$oasis_defense, na.rm = TRUE), min(data$oasis_offense, na.rm = TRUE))
    max_val <- max(max(data$oasis_defense, na.rm = TRUE), max(data$oasis_offense, na.rm = TRUE))
    buffer <- 0.1 * (max_val - min_val)
    min_plot <- min_val - buffer
    max_plot <- max_val + buffer
    
    ggplot(data, aes(x = oasis_defense, y = oasis_offense)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "white") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "white") +
      geom_hline(yintercept = mean(data$oasis_offense, na.rm = TRUE), linetype = "solid", color = "grey50") +
      geom_vline(xintercept = mean(data$oasis_defense, na.rm = TRUE), linetype = "solid", color = "grey50") +
      geom_nfl_logos(aes(team_abbr = team, width = ifelse(selected, 0.13, 0.065)), alpha = 1) +
      labs(
        title = "OASIS (Opponent-Adjusted Situational Impact Score) 2024 Season",
        x = "OASIS Defense",
        y = "OASIS Offense",
        caption = "OASIS = EPA adjusted for opponent strength & situational leverage - created by Eagles Eric \n@EaglesXsandOs\n"
      ) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "white") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "white") +
      xlim(min_plot, max_plot) + ylim(min_plot, max_plot) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#333333", color = NA),
        panel.background = element_rect(fill = "#333333", color = NA),
        panel.grid.major = element_line(color = "#555555"),
        panel.grid.minor = element_line(color = "#555555"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 16, face = "bold"),
        plot.title = element_text(color = "white", size = 18, face = "bold"),
        plot.caption = element_text(color = "white", size = 10)
      )
  })
  
  output$percentileTable <- renderTable({
    wp_epa_df() %>%
      select(team, off_percentile, def_percentile) %>%
      mutate(
        off_percentile = round(off_percentile),
        def_percentile = round(def_percentile),
        off_class = case_when(
          off_percentile <= 25 ~ "percentile-25",
          off_percentile <= 50 ~ "percentile-50",
          off_percentile <= 75 ~ "percentile-75",
          TRUE ~ "percentile-100"
        ),
        def_class = case_when(
          def_percentile <= 25 ~ "percentile-25",
          def_percentile <= 50 ~ "percentile-50",
          def_percentile <= 75 ~ "percentile-75",
          TRUE ~ "percentile-100"
        ),
        "Offensive Percentile" = sprintf('<span class="%s">%d</span>', off_class, off_percentile),
        "Defensive Percentile" = sprintf('<span class="%s">%d</span>', def_class, def_percentile)
      ) %>%
      select(team, "Offensive Percentile", "Defensive Percentile")
  }, sanitize.text.function = function(x) x)
  
  output$weeklyHeader <- renderText({
    paste("Weekly Percentiles for", input$team_select)
  })
  
  output$weeklyBreakdown <- renderTable({
    weekly_breakdown()
  }, sanitize.text.function = function(x) x)
}

shinyApp(ui = ui, server = server)