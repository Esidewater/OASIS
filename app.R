options(rsconnect.max.bundle.size = 5368709120)
library(shiny)
library(shinythemes)
library(nflfastR)
library(tidyverse)
library(nflplotR)
library(readr)
library(nflreadr)
library(zoo)
library(ggplot2)

# Preload and pre-filter play-by-play data for 2018-2024
pbp_all <- tryCatch({
  load_pbp(2018:2024) %>%
    filter(!is.na(epa), !is.na(wp), !is.na(wpa))
}, error = function(e) {
  stop("Failed to load play-by-play data. Check your internet connection or nflfastR package.")
})

# Check for missing betting lines
missing_lines <- sum(is.na(pbp_all$spread_line) | is.na(pbp_all$total_line))
if (missing_lines > 0) {
  warning(paste(missing_lines, "plays have missing spread_line or total_line values, which may affect strength calculations."))
}

# Compute game data with implied scores
game_data <- pbp_all %>%
  select(game_id, home_team, away_team, spread_line, total_line, week, result, season) %>%
  distinct() %>%
  mutate(
    implied_home_score = (total_line + spread_line) / 2,
    implied_away_score = (total_line - spread_line) / 2
  )

# Create team game data with results
team_game_data <- bind_rows(
  game_data %>%
    select(team = home_team, opponent = away_team, implied_points_scored = implied_home_score, 
           implied_points_allowed = implied_away_score, week, result, season) %>%
    mutate(result = result),
  game_data %>%
    select(team = away_team, opponent = home_team, implied_points_scored = implied_away_score, 
           implied_points_allowed = implied_home_score, week, result, season) %>%
    mutate(result = -result)
)

# Compute team strength metrics
team_strength <- team_game_data %>%
  group_by(team) %>%
  summarize(
    avg_implied_points_scored = mean(implied_points_scored, na.rm = TRUE),
    avg_implied_points_allowed = mean(implied_points_allowed, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    off_strength = avg_implied_points_scored - mean(avg_implied_points_scored),
    def_strength = mean(avg_implied_points_allowed) - avg_implied_points_allowed,
    off_strength_z = off_strength / sd(off_strength),
    def_strength_z = def_strength / sd(def_strength)
  )

# Add team information
team_info <- nflreadr::load_teams() %>%
  select(team_abbr, team_conf, team_division)

team_strength <- team_strength %>%
  left_join(team_info, by = c("team" = "team_abbr"))

# Define divisions for each conference
nfc_divisions <- unique(team_strength$team_division[team_strength$team_conf == "NFC"])
afc_divisions <- unique(team_strength$team_division[team_strength$team_conf == "AFC"])

# Define playoff week labels and values
playoff_week_labels <- c("WC", "DIV", "CONF", "SB")
playoff_week_values <- c(19, 20, 21, 22)
names(playoff_week_values) <- playoff_week_labels

# Precompute season-wide OASIS scores
season_off_oasis <- pbp_all %>%
  filter(!is.na(epa), !is.na(wp), !is.na(wpa)) %>%
  left_join(team_strength %>% select(team, def_strength_z), by = c("defteam" = "team")) %>%
  mutate(
    wp_change = abs(wpa),
    leverage_factor = 1 + 3 * wp_change,
    off_weight = exp(0.2 * def_strength_z),
    adjusted_off_epa = epa * leverage_factor * off_weight
  ) %>%
  group_by(posteam, season, week) %>%
  summarize(
    off_oasis = sum(adjusted_off_epa, na.rm = TRUE) / sum(leverage_factor * off_weight, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(team = posteam)

season_def_oasis <- pbp_all %>%
  filter(!is.na(epa), !is.na(wp), !is.na(wpa)) %>%
  left_join(team_strength %>% select(team, off_strength_z), by = c("defteam" = "team")) %>%
  mutate(
    wp_change = abs(wpa),
    leverage_factor = 1 + 3 * wp_change,
    def_weight = exp(0.2 * off_strength_z),
    adjusted_def_epa = -epa * leverage_factor * def_weight
  ) %>%
  group_by(defteam, season, week) %>%
  summarize(
    def_oasis = sum(adjusted_def_epa, na.rm = TRUE) / sum(leverage_factor * def_weight, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(team = defteam)

# Precompute ECDFs for percentiles
off_ecdf <- ecdf(season_off_oasis$off_oasis)
def_ecdf <- ecdf(season_def_oasis$def_oasis)

# UI Definition
ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        color: #f0f0f0;
      }
      .shiny-input-container {
        color: #f0f0f0;
      }
      h1, h2, h3, h4, h5, h6 {
        color: #ffffff;
        margin-top: 5px;
        margin-bottom: 5px;
      }
      p {
        font-size: 14px;
        margin-top: 5px;
        margin-bottom: 5px;
      }
      .small-text h3 {
        font-size: 14px;
      }
      .small-text h4 {
        font-size: 12px;
      }
      .small-text p, .small-text li {
        font-size: 10px;
      }
      table {
        color: #f0f0f0;
        background-color: #333333;
        border-color: #555555;
      }
      th, td {
        border-color: #555555;
      }
      .percentile-0 { color: #FF0000; }
      .percentile-25 { color: #FF6600; }
      .percentile-50 { color: #FFA500; }
      .percentile-75 { color: #99CC00; }
      .percentile-100 { color: #00FF00; }
      .win { color: #00FF00; }
      .loss { color: #FF0000; }
      .week-column { text-align: center; }
      .progress-bar {
        background: linear-gradient(to right, #00FF00, #FFA500);
        animation: progressAnimation 2s infinite;
      }
      @keyframes progressAnimation {
        0% { background-position: 0% 50%; }
        50% { background-position: 100% 50%; }
        100% { background-position: 0% 50%; }
      }
      .glow {
        filter: drop-shadow(0 0 5px #00FF00) drop-shadow(0 0 10px #00FF00);
      }
    "))
  ),
  titlePanel("OASIS (Opponent-Adjusted Situational Impact Score)"),
  fluidRow(
    column(12, class = "small-text",
           h3("What is OASIS?"),
           p("OASIS (Opponent-Adjusted Situational Impact Score) is an NFL statistic designed to improve upon Expected Points Added (EPA)."),
           p("OASIS adjusts for both the opponent and the game situation. A 10-yard gain in a close 4th-quarter game against an elite defense carries more weight than the same gain in garbage time against a weak defense.")
    )
  ),
  fluidRow(
    column(6, class = "small-text",
           h4("How is OASIS Calculated?"),
           p("OASIS builds on EPA by adding two key adjustments:"),
           tags$ul(
             tags$li("Play-by-play EPA is first weighted by game leverage (win probability impact)."),
             tags$li("Opponent strength is determined using market-implied scores from betting lines."),
             tags$li("Offensive and defensive performance is adjusted based on the strength of the opponent they faced.")
           )
    ),
    column(6, class = "small-text",
           h4("How to Read the Chart"),
           p("Each NFL logo represents a team’s performance. The axes indicate how teams compare in offensive and defensive OASIS scores."),
           tags$ul(
             tags$li("X-Axis (OASIS Defense): Higher means better defensive performance."),
             tags$li("Y-Axis (OASIS Offense): Higher means better offensive performance."),
             tags$li("Top-Right: Strong offense and defense."),
             tags$li("Top-Left: Strong offense, weaker defense."),
             tags$li("Bottom-Right: Strong defense, weaker offense."),
             tags$li("Bottom-Left: Struggling in both areas.")
           )
    )
  ),
  fluidRow(
    column(12,
           h3("Filters"),
           checkboxGroupInput("years", "Years:", choices = 2018:2024, selected = 2024, inline = TRUE),
           sliderInput("reg_week", "Regular Season Week:", min = 1, max = 18, value = c(1, 18), width = "100%"),
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
           checkboxGroupInput("conferences", "Conference:",
                              choices = unique(team_strength$team_conf),
                              selected = unique(team_strength$team_conf),
                              inline = TRUE),
           checkboxGroupInput("divisions", "Division:",
                              choices = unique(team_strength$team_division),
                              selected = unique(team_strength$team_division),
                              inline = TRUE),
           checkboxGroupInput("downs", "Down:", 
                              choices = c(1, 2, 3, 4), 
                              selected = c(1, 2, 3, 4),
                              inline = TRUE)
    )
  ),
  fluidRow(
    column(12,
           selectInput("team_select", "Select Team for Weekly Breakdown:", choices = sort(unique(team_strength$team)), selected = "BUF", width = "300px"),
           plotOutput("oasisPlot", width = "100%", height = "800px")
    )
  ),
  fluidRow(
    column(6,
           h4("Overall Team Percentiles"),
           tableOutput("percentileTable")
    ),
    column(6,
           h4(textOutput("weeklyHeader")),
           tableOutput("weeklyBreakdown")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  observe({
    if (!is.null(input$conferences)) {
      selected_conferences <- input$conferences
      if (length(selected_conferences) == 0) {
        updateCheckboxGroupInput(session, "divisions", selected = character(0))
      } else {
        selected_divisions <- character(0)
        if ("NFC" %in% selected_conferences) {
          selected_divisions <- c(selected_divisions, nfc_divisions)
        }
        if ("AFC" %in% selected_conferences) {
          selected_divisions <- c(selected_divisions, afc_divisions)
        }
        updateCheckboxGroupInput(session, "divisions", selected = selected_divisions)
      }
    }
  })
  
  observe({
    if (!is.null(input$divisions)) {
      selected_divisions <- input$divisions
      selected_conferences <- character(0)
      if (any(nfc_divisions %in% selected_divisions)) {
        selected_conferences <- c(selected_conferences, "NFC")
      }
      if (any(afc_divisions %in% selected_divisions)) {
        selected_conferences <- c(selected_conferences, "AFC")
      }
      updateCheckboxGroupInput(session, "conferences", selected = selected_conferences)
    }
  })
  
  filtered_team_strength <- reactive({
    req(input$conferences, input$divisions)
    team_strength %>%
      filter(team_conf %in% input$conferences,
             team_division %in% input$divisions)
  })
  
  filtered_data <- reactive({
    req(input$years, input$reg_week, input$downs)
    withProgress(message = "Filtering data...", value = 0, {
      reg_season_filter <- (pbp_all$season %in% input$years & 
                              pbp_all$week >= input$reg_week[1] & 
                              pbp_all$week <= input$reg_week[2])
      playoff_filter <- FALSE
      if (input$include_playoffs) {
        req(input$playoff_week)
        playoff_weeks_slider <- input$playoff_week
        playoff_weeks <- playoff_week_values[playoff_weeks_slider[1]:playoff_weeks_slider[2]]
        playoff_filter <- (pbp_all$season %in% input$years & pbp_all$week %in% playoff_weeks)
      }
      filtered_pbp <- pbp_all %>%
        filter(
          reg_season_filter | playoff_filter,
          down %in% as.numeric(input$downs)
        )
      filtered_pbp %>%
        left_join(filtered_team_strength() %>% select(team, def_strength_z), by = c("defteam" = "team")) %>%
        left_join(filtered_team_strength() %>% select(team, off_strength_z), by = c("posteam" = "team")) %>%
        left_join(filtered_team_strength() %>% select(team, off_strength_z), by = c("defteam" = "team"), suffix = c("_posteam", "_defteam"))
    })
  })
  
  wp_epa_df <- reactive({
    req(filtered_data())
    filtered_teams <- filtered_team_strength()$team
    pbp <- filtered_data() %>%
      filter(posteam %in% filtered_teams, defteam %in% filtered_teams)
    validate(
      need(nrow(pbp) > 0, "No data available based on current filters.")
    )
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
  
  weekly_breakdown <- reactive({
    withProgress(message = "Calculating weekly breakdown...", value = 0, {
      game_schedule <- pbp_all %>%
        select(game_id, week, season, home_team, away_team, result) %>%
        distinct() %>%
        mutate(
          team = input$team_select,
          opponent = case_when(
            home_team == input$team_select ~ away_team,
            away_team == input$team_select ~ home_team,
            TRUE ~ NA_character_
          ),
          location = case_when(
            home_team == input$team_select ~ "H",
            away_team == input$team_select ~ "A",
            TRUE ~ NA_character_
          ),
          win_loss = case_when(
            home_team == input$team_select & result > 0 ~ "W",
            home_team == input$team_select & result < 0 ~ "L",
            away_team == input$team_select & result < 0 ~ "W",
            away_team == input$team_select & result > 0 ~ "L",
            result == 0 ~ "T",
            TRUE ~ "NA"
          )
        ) %>%
        filter(!is.na(opponent) & season %in% input$years) %>%
        select(season, week, opponent, location, win_loss)
      
      off_data <- pbp_all %>%
        filter(!is.na(epa), !is.na(wp), !is.na(wpa) & season %in% input$years) %>%
        left_join(team_strength %>% select(team, def_strength_z), by = c("defteam" = "team")) %>%
        mutate(
          wp_change = abs(wpa),
          leverage_factor = 1 + 3 * wp_change,
          off_weight = exp(0.2 * def_strength_z),
          adjusted_off_epa = epa * leverage_factor * off_weight
        ) %>%
        group_by(posteam, season, week) %>%
        summarize(
          off_oasis = sum(adjusted_off_epa, na.rm = TRUE) / sum(leverage_factor * off_weight, na.rm = TRUE),
          opponent = first(defteam),
          .groups = 'drop'
        ) %>%
        rename(team = posteam) %>%
        filter(team == input$team_select) %>%
        mutate(off_percentile = round(off_ecdf(off_oasis) * 100)) %>%
        select(season, week, opponent, off_percentile)
      
      def_data <- pbp_all %>%
        filter(!is.na(epa), !is.na(wp), !is.na(wpa) & season %in% input$years) %>%
        left_join(team_strength %>% select(team, off_strength_z), by = c("defteam" = "team")) %>%
        mutate(
          wp_change = abs(wpa),
          leverage_factor = 1 + 3 * wp_change,
          def_weight = exp(0.2 * off_strength_z),
          adjusted_def_epa = -epa * leverage_factor * def_weight
        ) %>%
        group_by(defteam, season, week) %>%
        summarize(
          def_oasis = sum(adjusted_def_epa, na.rm = TRUE) / sum(leverage_factor * def_weight, na.rm = TRUE),
          opponent = first(posteam),
          .groups = 'drop'
        ) %>%
        rename(team = defteam) %>%
        filter(team == input$team_select) %>%
        mutate(def_percentile = round(def_ecdf(def_oasis) * 100)) %>%
        select(season, week, opponent, def_percentile)
      
      weekly_data <- off_data %>%
        full_join(def_data, by = c("season", "week", "opponent")) %>%
        full_join(game_schedule, by = c("season", "week", "opponent")) %>%
        arrange(season, week)
      
      weekly_data <- weekly_data %>%
        mutate(
          Year = season,
          Week = sprintf('<span class="week-column">%d</span>', week),
          Opponent = ifelse(location == "A", paste0("@ ", opponent), opponent),
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
          win_class = case_when(
            win_loss == "W" ~ "win",
            win_loss == "L" ~ "loss",
            TRUE ~ ""
          ),
          "Offensive Percentile" = sprintf('<span class="%s">%d</span>', off_class, off_percentile),
          "Defensive Percentile" = sprintf('<span class="%s">%d</span>', def_class, def_percentile),
          "Result" = sprintf('<span class="%s">%s</span>', win_class, win_loss)
        ) %>%
        select(Year, Week, Opponent, "Offensive Percentile", "Defensive Percentile", "Result")
      
      return(weekly_data)
    })
  })
  
  output$oasisPlot <- renderPlot({
    data <- wp_epa_df()
    min_val <- min(min(data$oasis_defense, na.rm = TRUE), min(data$oasis_offense, na.rm = TRUE))
    max_val <- max(max(data$oasis_defense, na.rm = TRUE), max(data$oasis_offense, na.rm = TRUE))
    buffer <- 0.1 * (max_val - min_val)
    min_plot <- min_val - buffer
    max_plot <- max_val + buffer
    
    selected_team <- data %>% filter(selected)
    
    ggplot(data, aes(x = oasis_defense, y = oasis_offense)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "white") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "white") +
      geom_hline(yintercept = mean(data$oasis_offense, na.rm = TRUE), linetype = "solid", color = "grey50") +
      geom_vline(xintercept = mean(data$oasis_defense, na.rm = TRUE), linetype = "solid", color = "grey50") +
      geom_nfl_logos(aes(team_abbr = team, width = ifelse(selected, 0.10, 0.05)), alpha = 1) +
      geom_nfl_logos(data = selected_team, aes(team_abbr = team), width = 0.10, alpha = 0.05) +
      geom_text(data = selected_team, 
                aes(label = paste("Off:", round(off_percentile), "\nDef:", round(def_percentile))),
                hjust = 0.5, vjust = -0.8, color = "white", size = 8, fontface = "bold") +
      labs(
        title = "OASIS (Opponent-Adjusted Situational Impact Score)",
        x = "OASIS Defense",
        y = "OASIS Offense",
        caption = "OASIS = EPA adjusted for opponent strength & situational leverage - created by Eagles Eric \n@EaglesXsandOs\n"
      ) +
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