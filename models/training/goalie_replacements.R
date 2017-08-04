### GOALIE REPLACEMENTS ###
# Last edit: Manny (2017-06-23)

## Dependencies
require(dplyr); require(RSQLite); require(doMC); require(Kmisc); 
require(survival); require(glmnet); require(caret); require(Matrix);
require(RCurl); require(rvest); require(rjson)
load("/srv/shiny-server/modules/user_functions.RData")
load("/srv/shiny-server/modules/stats.RData")
load("/srv/shiny-server/modules/dryscrape.RData")
load("/srv/shiny-server/models/xg_model.RData")
load("/srv/shiny-server/models/adjustments_model.RData")


## Switches
season <- "20122013"


## Functions
# Meta
is_on <- function(player, pbp, venue) {
  
  if(tolower(venue) == "home") {
    
    vect <- 1*(pbp$home_on_1 == player) +
            1*(pbp$home_on_2 == player) +
            1*(pbp$home_on_3 == player) +
            1*(pbp$home_on_4 == player) +
            1*(pbp$home_on_5 == player) +
            1*(pbp$home_on_6 == player)
    
  } else if (tolower(venue) == "away") {
    
    vect <- 1*(pbp$away_on_1 == player) +
            1*(pbp$away_on_2 == player) +
            1*(pbp$away_on_3 == player) +
            1*(pbp$away_on_4 == player) +
            1*(pbp$away_on_5 == player) +
            1*(pbp$away_on_6 == player)
    
  }
  
  vect[which(is.na(vect) == TRUE)] <- 0
  vect[which(vect > 1)] <- 1
  
  return(vect)
  
}

is_on2 <- function(player, pbp, ref) {
  
  if(tolower(ref) == "for") {
    
    vect <- (1*(pbp$home_on_1 == player) +
             1*(pbp$home_on_2 == player) +
             1*(pbp$home_on_3 == player) +
             1*(pbp$home_on_4 == player) +
             1*(pbp$home_on_5 == player) +
             1*(pbp$home_on_6 == player)
             )*(pbp$event_team == pbp$home_team) +
            (1*(pbp$away_on_1 == player) +
             1*(pbp$away_on_2 == player) +
             1*(pbp$away_on_3 == player) +
             1*(pbp$away_on_4 == player) +
             1*(pbp$away_on_5 == player) +
             1*(pbp$away_on_6 == player)
             )*(pbp$event_team == pbp$away_team)
    
  } else if (tolower(ref) == "against") {
    
    vect <- (1*(pbp$home_on_1 == player) +
             1*(pbp$home_on_2 == player) +
             1*(pbp$home_on_3 == player) +
             1*(pbp$home_on_4 == player) +
             1*(pbp$home_on_5 == player) +
             1*(pbp$home_on_6 == player)
             )*(pbp$event_team == pbp$away_team) +
            (1*(pbp$away_on_1 == player) +
             1*(pbp$away_on_2 == player) +
             1*(pbp$away_on_3 == player) +
             1*(pbp$away_on_4 == player) +
             1*(pbp$away_on_5 == player) +
             1*(pbp$away_on_6 == player)
             )*(pbp$event_team == pbp$home_team)
    
  }
  
  vect[which(is.na(vect) == TRUE)] <- 0
  vect[which(vect > 1)] <- 1
  
  return(vect)
  
}

player_dummy <- function(player, pbp, type) {
  
  if(tolower(type) == "shooting") {
    
    vect <- 1*(as.character(pbp$event_player_1) == player)
    
  } else if(tolower(type) == "pens") {
    
    vect <- 1*(as.character(pbp$player) == player)
    
  }
  
  vect[which(is.na(vect) == TRUE)] <- 0
  vect[which(vect > 1)] <- 1
  
  return(vect)
  
}

OR2dProb <- function(OR, prob1) {
  
  prob2 <- (OR - 1)*(prob1/(1 - prob1))/
           (1 + (OR - 1)*(prob1/(1 - prob1)))
  
  return(prob2)
  
}

rep_dummy <- function(reps, pbp) {
  
  vect <- 1*(as.character(pbp$goalie) %in% reps)
   
  vect[which(is.na(vect) == TRUE)] <- 0
  vect[which(vect > 1)] <- 1
  
  return(vect)
  
}

## Load data
# Connect to database
conn <- dbConnect(SQLite(), "~/corsica_data/raw.sqlite")

# Query PBP
pbp_query <- dbSendQuery(conn, paste("SELECT * FROM pbp WHERE season == '",
                                     season,
                                     "'",
                                     sep = ""
                                     )
                         )

pbp <- fetch(pbp_query, -1)

# Disconnect
dbDisconnect(conn)

# Load replacement players list
load("/srv/shiny-server/models/replacement_players.RData")
greps <- as.character(na.omit(replacement_players$player_id[which(replacement_players$position == "G" & replacement_players$season == season)]))


## Prepare data
# Enhance PBP
pbp <- st.pbp_enhance(pbp)

# Get goalie list
glist <- unique(c(pbp$home_goalie,
                  pbp$away_goalie
                  )
                )

# Search names
lapply(as.list(glist), 
       ds.who
       ) %>%
  unlist() ->
  player_name

player_df <- data.frame(player_id = glist[which(glist != 0 & !is.na(glist) & nchar(glist) == 7)],
                        player_name = player_name
                        )

# Baseline rates
pbp %>%
  filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
  summarise(TOI = sum(na.omit(event_length))/60,
            home_FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
            away_FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
            home_GF = sum(event_type == "GOAL" & event_team == home_team),
            away_GF = sum(event_type == "GOAL" & event_team == away_team)
            ) %>%
  mutate(home_rate = home_FF/TOI,
         away_rate = away_FF/TOI,
         home_fsh = home_GF/home_FF,
         away_fsh = away_GF/away_FF,
         total_fsh = (home_GF + away_GF)/(home_FF + away_FF)
         ) %>%
  data.frame() ->
  baseline_rates

# Goalie stats
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
           !is.na(home_goalie),
           game_strength_state %in% st.strength_states
           ) %>%
    group_by(home_goalie) %>%
    rename(player = home_goalie) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
           !is.na(away_goalie),
           game_strength_state %in% st.strength_states
           ) %>%
    group_by(away_goalie) %>%
    rename(player = away_goalie) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team)
              )
) %>%
  data.frame() %>%
  group_by(player) %>%
  summarise(TOI = sum(na.omit(TOI)),
            FA = sum(FA)
            ) %>%
  data.frame() ->
  goalie_stats

# Prepare PBP
pbp %>% 
  filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states,
         event_type %in% c(st.fenwick_events,
                           "ON",
                           "PENALTY",
                           "FACEOFF"
                           )
         ) %>%
  arrange(game_id, event_index) %>%
  mutate(faceoff_ref = cumsum(event_type == "FACEOFF")) %>%
  group_by(game_id, faceoff_ref) %>%
  arrange(event_index) %>%
  mutate(seconds_since = nabs(game_seconds) - min(nabs(game_seconds)),
         rinkside_start = first(event_rinkside),
         home_zonestart = 1*(rinkside_start == home_rinkside) + 2*(rinkside_start == "N") + 3*(rinkside_start != home_rinkside & rinkside_start != "N"),
         home_score_adv = home_score - away_score
         ) %>%
  ungroup() %>%
  arrange(game_id, event_index) %>%
  data.frame() ->
  newpbp

newpbp %>%
  filter(event_type %in% st.fenwick_events,
         !{{is.na(home_goalie) == TRUE & event_team == away_team} | 
           {is.na(away_goalie) == TRUE & event_team == home_team}
           },
         !is.na(prob_goal)
         ) %>%
  mutate(is_home_team = 1*(event_team == home_team)) %>%
  data.frame() ->
  goalie_pbp

# Build design matrix
goalie_pbp$goalie <- ifelse(goalie_pbp$is_home_team == 1,
                            goalie_pbp$away_goalie,
                            goalie_pbp$home_goalie
                            )

goalie_pbp$goalie_rep <- 1*(goalie_pbp$goalie %in% greps)

goalie_pbp$shooter_strength_state <- ifelse(goalie_pbp$is_home_team == 1,
                                            goalie_pbp$game_strength_state,
                                            str_rev(goalie_pbp$game_strength_state)
                                            )

goalie_pbp$shooter_score_adv <- ifelse(goalie_pbp$is_home_team == 1,
                                       goalie_pbp$home_score - goalie_pbp$away_score,
                                       goalie_pbp$away_score - goalie_pbp$home_score
                                       )

goalie_pbp$shooter_strength_state <- as.factor(goalie_pbp$shooter_strength_state)
goalie_pbp$event_player_1 <- as.factor(goalie_pbp$event_player_1)
goalie_pbp$goalie <- as.factor(goalie_pbp$goalie)
goalie_pbp$is_goal <- as.factor(1 + 1*(goalie_pbp$event_type == "GOAL"))

design_goalie <- dummyVars(as.factor(is_goal) ~
                           shooter_strength_state +
                           is_home_team +
                           shooter_score_adv*game_seconds +
                           goalie_rep +
                           event_player_1 +
                           prob_goal,
                           data = goalie_pbp,
                           contrasts = TRUE,
                           fullRank = FALSE
                           )

goalie_mat <- cbind(predict(design_goalie,
                            goalie_pbp
                            ) %>%
                    Matrix(sparse = TRUE)
                    )


## Regression
glm_goalie <- cv.glmnet(x = as.matrix(goalie_mat),
                        y = as.factor(goalie_pbp$is_goal),
                        family = "binomial",
                        nfolds = 5,
                        nlambda = 100,
                        alpha = 0.01
                        )

goalie_coefs <- data.frame(var = c("Intercept", colnames(goalie_mat)),
                           coeff = matrix(exp(coef(glm_goalie, s = "lambda.min")))
                           )

## Save
save(list = "goalie_coefs",
     file = paste("~/corsica_data/goalie_replacement_coefs",
                  season,
                  ".RData",
                  sep = ""
                  )
     )

########################################################################################################################
########################################################################################################################
########################################################################################################################

load("~/corsica_data/goalie_replacement_coefs20122013.RData")
coef1 <- goalie_coefs$coeff[which(goalie_coefs$var == "goalie_rep")]

load("~/corsica_data/goalie_replacement_coefs20132014.RData")
coef2 <- goalie_coefs$coeff[which(goalie_coefs$var == "goalie_rep")]

load("~/corsica_data/goalie_replacement_coefs20142015.RData")
coef3 <- goalie_coefs$coeff[which(goalie_coefs$var == "goalie_rep")]

load("~/corsica_data/goalie_replacement_coefs20152016.RData")
coef4 <- goalie_coefs$coeff[which(goalie_coefs$var == "goalie_rep")]

load("~/corsica_data/goalie_replacement_coefs20162017.RData")
coef5 <- goalie_coefs$coeff[which(goalie_coefs$var == "goalie_rep")]

mean(c(coef1, coef2, coef3, coef4, coef5))
