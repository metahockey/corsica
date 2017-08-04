### PYTHAGOREAN EXPONENT ###

## Dependencies
require(dplyr)
require(RSQLite)


## Functions
# Meta
loss_function <- function(x, data) {
  
  data$loss <- ((1 + (data$GA/data$GF)^x)^(-1) - (data$W/(data$W + data$L)))^2
  
  sum(data$loss)
  
}

# General
pythag <- function(data, season) {
  
  season_data <- filter(data, season == season)
  
  opt <- optimize(loss_function,
                  interval = c(0, 6),
                  data = season_data[, c(3:6)]
                  )
  
  opt$minimum
  
}


## Load Data
# Connect to database
conn <- dbConnect(SQLite(), "~/corsica_data/corsica.sqlite")

# Read table
team_stats <- dbReadTable(conn, "team_stats")

# Disconnect
dbDisconnect(conn)


## Prepare Data
# Add game results
team_stats %>%
  group_by(team, game_id) %>%
  mutate(won = 1*(sum(GF) > sum(GA)),
         lost = 1*(sum(GF) < sum(GA))
         ) %>%
  data.frame() ->
  newstats

# Season summary
newstats %>%
  filter(game_strength_state %in% st.strength_states) %>%
  group_by(team, season, game_id) %>%
  summarise(GF = sum(GF),
            GA = sum(GA),
            won = first(won),
            lost = first(lost)
            ) %>%
  group_by(team, season) %>%
  summarise(W = sum(won),
            L = sum(lost),
            GF = sum(GF),
            GA = sum(GA)
            ) %>%
  data.frame() ->
  team_summary


## Find Exponent
foreach(i = unique(team_summary$season), .combine = c) %do% {
  
  pythag(team_summary, i)
  
} %>%
  unlist() ->
  exponents

exp_mat <- data.frame(season = unique(team_summary$season),
                      e = exponents
                      )

## Goals to Wins
newstats %>%
  filter(game_strength_state %in% st.strength_states) %>%
  group_by(team, season, game_id) %>%
  summarise(GF = sum(GF)) %>%
  group_by(season) %>%
  summarise(l = mean(GF)) %>%
  data.frame() ->
  runs

runs$e <- exp_mat$e[match(runs$season, exp_mat$season)]

4*runs$l/runs$e
