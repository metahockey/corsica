### WAR TRAINING ###
# Last edit: Manny (2017-05-22)

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
season <- "20162017"


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

player_search <- function(player_id) {
  
  pdata <- ds.scrape_player_profile(player_id, try_tolerance = 3, agents = ds.user_agents)
  
  pdata %>%
    select(player_id,
           player_name_full,
           player_position
           ) %>%
    data.frame()
  
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


## Prepare data
# Enhance PBP
pbp <- st.pbp_enhance(pbp)

# Get goalie list
glist <- unique(c(pbp$home_goalie,
                  pbp$away_goalie
                  )
                )

# Get player list
plist <- c(pbp$home_on_1,
           pbp$home_on_2,
           pbp$home_on_3,
           pbp$home_on_4,
           pbp$home_on_5,
           pbp$home_on_6,
           pbp$away_on_1,
           pbp$away_on_2,
           pbp$away_on_3,
           pbp$away_on_4,
           pbp$away_on_5,
           pbp$away_on_6
           ) %>%
  na.omit()

data.frame(player = plist) %>%
  group_by(player) %>%
  summarise(events = n()) %>%
  filter(events > 1000,
         player %in% glist == FALSE
         ) %>%
  data.frame() ->
  player_df

# Search names
do.call("rbind",
        lapply(as.list(player_df$player), 
               player_search
               )
        ) ->
  player_df

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

# Player stats
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(home_on_1) %>%
    rename(player = home_on_1) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(home_on_2) %>%
    rename(player = home_on_2) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(home_on_3) %>%
    rename(player = home_on_3) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(home_on_4) %>%
    rename(player = home_on_4) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(home_on_5) %>%
    rename(player = home_on_5) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(home_on_6) %>%
    rename(player = home_on_6) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(away_on_1) %>%
    rename(player = away_on_1) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(away_on_2) %>%
    rename(player = away_on_2) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(away_on_3) %>%
    rename(player = away_on_3) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(away_on_4) %>%
    rename(player = away_on_4) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(away_on_5) %>%
    rename(player = away_on_5) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(away_on_6) %>%
    rename(player = away_on_6) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
              iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player)
              )
) %>%
  data.frame() %>%
  group_by(player) %>%
  summarise(HTOI = sum(na.omit(TOI*(venue == "home"))),
            ATOI = sum(na.omit(TOI*(venue == "away"))),
            FF = sum(FF),
            FA = sum(FA),
            iFF = sum(iFF)
            ) %>%
  data.frame() ->
  skater_stats

# Rinkside reference
pbp %>%
  filter(!is.na(home_rinkside),
         !is.na(away_rinkside)
         ) %>%
  group_by(game_id, game_period) %>%
  summarise(home_rinkside = first(home_rinkside),
            away_rinkside = first(away_rinkside)
            ) %>%
  data.frame() ->
  rinkside_df

# Merge with rinkside
merge(pbp %>%
        select(-c(home_rinkside, away_rinkside)) %>%
        data.frame(),
      rinkside_df,
      by.x = c("game_id", "game_period"),
      by_y = c("game_id", "game_period"),
      all.x = TRUE
      ) %>%
  data.frame() ->
  pbp

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


## Shot Rates
# Prepare PBP
newpbp %>%
  filter(event_type %in% c(st.fenwick_events,
                           "ON",
                           "FACEOFF"
                           ),
         !is.na(home_zonestart)
         ) %>%
  group_by(game_id) %>%
  arrange(event_index) %>%
  mutate(elapsed = game_seconds - lag(game_seconds, 1)) %>%
  ungroup() %>%
  filter(!is.na(elapsed)) %>%
  arrange(game_id, event_index) %>%
  data.frame() ->
  rates_pbp

# Build home matrix
do.call("cbind",
        lapply(as.list(player_df$player_id), 
               is_on,
               rates_pbp,
               venue = "Home"
               )
        ) %>%
  Matrix(sparse = TRUE) ->
  home_on_mat

colnames(home_on_mat) <- paste(player_df$player_id, "home", sep = "_")

# Build away matrix
do.call("cbind",
        lapply(as.list(player_df$player_id), 
               is_on,
               rates_pbp,
               venue = "Away"
               )
        ) %>%
  Matrix(sparse = TRUE) ->
  away_on_mat

colnames(away_on_mat) <- paste(player_df$player_id, "away", sep = "_")

# Build design matrix
rates_pbp$game_strength_state <- as.factor(rates_pbp$game_strength_state)
rates_pbp$home_zonestart <- as.factor(rates_pbp$home_zonestart)

rates_pbp$home_hazard <- 1*(rates_pbp$event_type %in% st.fenwick_events & rates_pbp$event_team == rates_pbp$home_team)
rates_pbp$away_hazard <- 1*(rates_pbp$event_type %in% st.fenwick_events & rates_pbp$event_team == rates_pbp$away_team)

design_rates <- dummyVars(home_hazard ~
                          game_strength_state +
                          home_score_adv*game_seconds +
                          home_zonestart*seconds_since,
                          data = rates_pbp,
                          contrasts = TRUE,
                          fullRank = FALSE
                          )

rates_mat <- cBind(predict(design_rates,
                           rates_pbp
                           ) %>%
                   Matrix(sparse = TRUE),
                   home_on_mat,
                   away_on_mat
                   )

# Model home rates
home_index <- which({rates_pbp$event_type %in% st.fenwick_events == FALSE & rates_pbp$elapsed > 0} | 
                    rates_pbp$home_hazard > 0
                    )

rates_pbp_home <- rates_pbp[home_index, ]
rates_mat_home <- rates_mat[home_index, ]

rates_pbp_home$elapsed[which(rates_pbp_home$elapsed <= 0)] <- 0.1
home_surv <- Surv(rates_pbp_home$elapsed, rates_pbp_home$home_hazard)

# Model away rates
away_index <- which({rates_pbp$event_type %in% st.fenwick_events == FALSE & rates_pbp$elapsed > 0} | 
                    rates_pbp$away_hazard > 0
                    )

rates_pbp_away <- rates_pbp[away_index, ]
rates_mat_away <- rates_mat[away_index, ]

rates_pbp_away$elapsed[which(rates_pbp_away$elapsed <= 0)] <- 0.1
away_surv <- Surv(rates_pbp_away$elapsed, rates_pbp_away$away_hazard)


## Shot Quality
# Prepare PBP
newpbp %>%
  filter(event_type %in% st.fenwick_events,
         !is.na(home_zonestart),
         !is.na(prob_goal)
         ) %>%
  mutate(is_home_team = 1*(event_team == home_team)) %>%
  data.frame() ->
  qual_pbp

# Build for matrix
do.call("cbind",
        lapply(as.list(player_df$player_id), 
               is_on2,
               qual_pbp,
               ref = "For"
               )
        ) %>%
  Matrix(sparse = TRUE) ->
  for_on_mat

colnames(for_on_mat) <- paste(player_df$player_id, "for", sep = "_")

# Build against matrix
do.call("cbind",
        lapply(as.list(player_df$player_id), 
               is_on2,
               qual_pbp,
               ref = "Against"
               )
        ) %>%
  Matrix(sparse = TRUE) ->
  against_on_mat

colnames(against_on_mat) <- paste(player_df$player_id, "against", sep = "_")

# Build design matrix
qual_pbp$shooter_strength_state <- ifelse(qual_pbp$is_home_team == 1,
                                          qual_pbp$game_strength_state,
                                          str_rev(qual_pbp$game_strength_state)
                                          )

qual_pbp$shooter_score_adv <- ifelse(qual_pbp$is_home_team == 1,
                                     qual_pbp$home_score - qual_pbp$away_score,
                                     qual_pbp$away_score - qual_pbp$home_score
                                     )

qual_pbp$shooter_strength_state <- as.factor(qual_pbp$shooter_strength_state)

design_qual <- dummyVars(prob_goal ~
                         shooter_strength_state +
                         is_home_team +
                         shooter_score_adv*game_seconds,
                         data = qual_pbp,
                         contrasts = TRUE,
                         fullRank = FALSE
                         )

qual_mat <- cBind(predict(design_qual,
                          qual_pbp
                          ) %>%
                  Matrix(sparse = TRUE),
                  for_on_mat,
                  against_on_mat
                  )

## Shooting
# Prepare PBP
newpbp %>%
  filter(event_type %in% st.fenwick_events,
         !{{is.na(home_goalie) == TRUE & event_team == away_team} | 
           {is.na(away_goalie) == TRUE & event_team == home_team}
           },
         !is.na(prob_goal),
         event_player_1 %in% player_df$player_id
         ) %>%
  mutate(is_home_team = 1*(event_team == home_team)) %>%
  data.frame() ->
  shooting_pbp

# Build design matrix
shooting_pbp$goalie <- ifelse(shooting_pbp$is_home_team == 1,
                              shooting_pbp$away_goalie,
                              shooting_pbp$home_goalie
                              )

shooting_pbp$shooter_strength_state <- ifelse(shooting_pbp$is_home_team == 1,
                                              shooting_pbp$game_strength_state,
                                              str_rev(shooting_pbp$game_strength_state)
                                              )

shooting_pbp$shooter_score_adv <- ifelse(shooting_pbp$is_home_team == 1,
                                         shooting_pbp$home_score - shooting_pbp$away_score,
                                         shooting_pbp$away_score - shooting_pbp$home_score
                                         )

shooting_pbp$shooter_strength_state <- as.factor(shooting_pbp$shooter_strength_state)
shooting_pbp$event_player_1 <- as.factor(shooting_pbp$event_player_1)
shooting_pbp$goalie <- as.factor(shooting_pbp$goalie)
shooting_pbp$is_goal <- as.factor(1 + 1*(shooting_pbp$event_type == "GOAL"))

do.call("cbind",
        lapply(as.list(player_df$player_id), 
               player_dummy,
               shooting_pbp,
               type = "shooting"
               )
        ) %>%
  Matrix(sparse = TRUE) ->
  shooter_mat

colnames(shooter_mat) <- paste("event_player_1", player_df$player_id, sep = ".")

design_shooting <- dummyVars(as.factor(is_goal) ~
                             shooter_strength_state +
                             is_home_team +
                             shooter_score_adv*game_seconds +
                             goalie +
                             prob_goal,
                             data = shooting_pbp,
                             contrasts = TRUE,
                             fullRank = FALSE
                             )

shooting_mat <- cbind(predict(design_shooting,
                              shooting_pbp
                              ) %>%
                        Matrix(sparse = TRUE),
                      shooter_mat
                      )


## Penalties
# Prepare PBP
pbp %>%
  group_by(game_id, game_seconds) %>%
  mutate(pens = sum(event_type == "PENALTY" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == FALSE),
         home_score_adv = home_score - away_score
         ) %>%
  group_by(game_id) %>%
  arrange(event_index) %>%
  mutate(pref = cumsum(event_type == "PENALTY" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == FALSE & pens < 2)) %>%
  group_by(game_id, pref) %>%
  mutate(last_penalty = ifelse(is.na(first(event_team)) == TRUE,
                               0,
                               nabs(first(event_team))
                               ),
         is_PP = 2 - cumsum({event_type == "GOAL" & event_team != last_penalty} |
                            event_type %in% c("PENALTY", "GAME_END")
                            ) +
                     1*({event_type == "GOAL" & event_team != last_penalty} |
                        event_type %in% c("PENALTY", "GAME_END")
                        )
         ) %>%
  group_by(game_id) %>%
  arrange(event_index) %>%
  mutate(streak_change = cumsum(event_type == "PENALTY" & 
                                grepl("ps \\-|match|fighting|major", tolower(event_detail)) == FALSE & 
                                pens < 2 & 
                                event_team != lag(last_penalty, 1)
                                )
         ) %>%
  group_by(game_id, streak_change) %>%
  arrange(event_index) %>%
  mutate(streak = cumsum(event_type == "PENALTY" & 
                         grepl("ps \\-|match|fighting|major", tolower(event_detail)) == FALSE & 
                         pens < 2
                         )
         ) %>%
  ungroup() %>%
  arrange(game_id, event_index) %>%
  data.frame() ->
  pens_pbp

pens_pbp$streak <- (lag(pens_pbp$last_penalty == pens_pbp$home_team))*lag(pens_pbp$streak, 1) -
                   (lag(pens_pbp$last_penalty == pens_pbp$away_team))*lag(pens_pbp$streak, 1)

# Get penalty value
pens_pbp %>%
  mutate(home_last = 1*(last_penalty == home_team)) %>%
  group_by(game_id, pref, home_last) %>%
  filter(is_PP == 1) %>%
  summarise(home_goals = sum(event_type == "GOAL" & event_team == home_team & game_seconds - min(game_seconds) < 120),
            away_goals = sum(event_type == "GOAL" & event_team == away_team & game_seconds - min(game_seconds) < 120)
            ) %>%
  data.frame() %>%
  summarise(gf = sum((home_last == 1)*away_goals + (home_last == 0)*home_goals),
            ga = sum((home_last == 0)*away_goals + (home_last == 1)*home_goals),
            pens = sum(pref > 0)
            ) ->
  sum_pens

penalty_value <- (sum_pens$gf - sum_pens$ga)/sum_pens$pens

# Compile counts
bind_rows(
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, home_on_1) %>%
    rename(player = home_on_1) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, home_on_2) %>%
    rename(player = home_on_2) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, home_on_3) %>%
    rename(player = home_on_3) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, home_on_4) %>%
    rename(player = home_on_4) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, home_on_5) %>%
    rename(player = home_on_5) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, home_on_6) %>%
    rename(player = home_on_6) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, away_on_1) %>%
    rename(player = away_on_1) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, away_on_2) %>%
    rename(player = away_on_2) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, away_on_3) %>%
    rename(player = away_on_3) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, away_on_4) %>%
    rename(player = away_on_4) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, away_on_5) %>%
    rename(player = away_on_5) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, away_on_6) %>%
    rename(player = away_on_6) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                 1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_1 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                 1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_player_2 == player & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              )
) %>%
  data.frame() %>%
  filter(player %in% player_df$player_id) %>%
  group_by(game_id, player, venue, game_strength_state, home_score_adv, streak) %>%
  summarise(TOI = sum(TOI),
            PENT = sum(PENT),
            PEND = sum(PEND)
            ) %>%
  ungroup() %>%
  filter(TOI > 0) %>%
  data.frame() ->
  pens_df

# Get baseline rate
pens_df %>%
  summarise(TOI = sum(TOI),
            PENT = sum(PENT)
            ) %>%
  mutate(penalty_rate = PENT/TOI) %>%
  data.frame() ->
  baseline_pens

# Build design matrix
pens_df$strength_state <- ifelse(pens_df$venue == "home",
                                         pens_df$game_strength_state,
                                         str_rev(pens_df$game_strength_state)
                                         )

pens_df$score_adv <- ifelse(pens_df$venue == "home",
                            pens_df$home_score_adv,
                            -pens_df$home_score_adv
                            )

pens_df$streak <- ifelse(pens_df$venue == "home",
                         -pens_df$streak,
                         pens_df$streak
                         )

pens_df$strength_state <- as.factor(pens_df$strength_state)
pens_df$player <- as.factor(pens_df$player)

do.call("cbind",
        lapply(as.list(player_df$player_id), 
               player_dummy,
               pens_df,
               type = "pens"
               )
        ) %>%
  Matrix(sparse = TRUE) ->
  pens_player_mat

colnames(pens_player_mat) <- paste("player", player_df$player_id, sep = "_")

design_pens <- dummyVars(PENT ~
                         strength_state +
                         venue +
                         score_adv +
                         streak,
                         data = pens_df,
                         contrasts = TRUE,
                         fullRank = FALSE
                         )

pens_mat <- cbind(predict(design_pens,
                          pens_df
                          ) %>%
                    Matrix(sparse = TRUE),
                  pens_player_mat
                  )


## Zones
# Prepare PBP
newpbp %>%
  group_by(game_id) %>%
  arrange(event_index) %>%
  mutate(ref = cumsum(event_type == "FACEOFF") - 1*(event_type == "FACEOFF")) %>%
  group_by(game_id, ref) %>%
  arrange(event_index) %>%
  mutate(home_zonestart_next = 1*(last(event_type) == "FACEOFF" & last(event_rinkside) == last(home_rinkside)) +
                               2*(last(event_type) == "FACEOFF" & last(event_rinkside) == "N") +
                               3*(last(event_type) == "FACEOFF" & last(event_rinkside) == last(away_rinkside)),
         shift_end = last(game_seconds),
         seconds_until = shift_end - game_seconds
         ) %>%
  ungroup() %>%
  filter(event_type == "ON",
         home_zonestart_next > 0,
         seconds_until > 0,
         seconds_until <= 120
         ) %>%
  group_by(game_id) %>%
  arrange(event_index) %>%
  mutate(sref = cumsum(event_type == "ON")) %>%
  group_by(game_id, 
           ref,
           sref, 
           home_on_1,
           home_on_2,
           home_on_3,
           home_on_4,
           home_on_5,
           home_on_6,
           away_on_1,
           away_on_2,
           away_on_3,
           away_on_4,
           away_on_5,
           away_on_6
           ) %>%
  summarise(shift_start = min(game_seconds),
            game_strength_state = first(game_strength_state),
            home_score_adv = first(home_score_adv),
            home_zonestart = first(home_zonestart),
            home_zonestart_next = first(home_zonestart_next),
            seconds_until = min(seconds_until),
            seconds_since = min(seconds_since),
            game_seconds = min(game_seconds)
            ) %>%
  ungroup() %>%
  arrange(game_id, game_seconds) %>%
  data.frame() ->
  zones_pbp

# Zone shift baselines
pbp %>%
  filter(event_type == "FACEOFF") %>%
  mutate(home_zonestart = 1*(event_rinkside == home_rinkside) + 
                          2*(event_rinkside == "N") + 
                          3*(event_rinkside != home_rinkside & event_rinkside != "N"),
         total = n()
         ) %>%
  summarise(pct_1 = sum(home_zonestart == 1)/first(total),
            pct_2 = sum(home_zonestart == 2)/first(total),
            pct_3 = sum(home_zonestart == 3)/first(total)
            ) %>%
  data.frame() ->
  baseline_zones

# Zone start value
newpbp %>%
  filter(!is.na(home_zonestart)) %>%
  group_by(home_zonestart) %>%
  summarise(HG = sum(event_type == "GOAL" & event_team == home_team),
            AG = sum(event_type == "GOAL" & event_team == away_team),
            HPENT = sum(na.omit(1*(event_type == "PENALTY" & event_team == home_team) +
                                1*(event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                1*(event_type == "PENALTY" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                                )),
            APENT = sum(na.omit(1*(event_type == "PENALTY" & event_team == away_team) +
                                1*(event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                1*(event_type == "PENALTY" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                                )),
            count = sum(event_type == "FACEOFF")
            ) %>%
  mutate(value = (HG - AG + (APENT - HPENT)*penalty_value)/count) %>%
  data.frame() ->
  faceoff_value

# Zone shift stats
bind_rows(
  zones_pbp %>%
    group_by(home_on_1) %>%
    rename(player = home_on_1) %>%
    summarise(venue = "home",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(home_on_2) %>%
    rename(player = home_on_2) %>%
    summarise(venue = "home",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(home_on_3) %>%
    rename(player = home_on_3) %>%
    summarise(venue = "home",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(home_on_4) %>%
    rename(player = home_on_4) %>%
    summarise(venue = "home",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(home_on_5) %>%
    rename(player = home_on_5) %>%
    summarise(venue = "home",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(home_on_6) %>%
    rename(player = home_on_6) %>%
    summarise(venue = "home",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(away_on_1) %>%
    rename(player = away_on_1) %>%
    summarise(venue = "away",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(away_on_2) %>%
    rename(player = away_on_2) %>%
    summarise(venue = "away",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(away_on_3) %>%
    rename(player = away_on_3) %>%
    summarise(venue = "away",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(away_on_4) %>%
    rename(player = away_on_4) %>%
    summarise(venue = "away",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(away_on_5) %>%
    rename(player = away_on_5) %>%
    summarise(venue = "away",
              ZF = length(unique(ref))
              ),
  
  zones_pbp %>%
    group_by(away_on_6) %>%
    rename(player = away_on_6) %>%
    summarise(venue = "away",
              ZF = length(unique(ref))
              )
) %>%
  data.frame() %>%
  group_by(player) %>%
  summarise(HZF = sum(ZF*(venue == "home")),
            AZF = sum(ZF*(venue == "away"))
            ) %>%
  data.frame() ->
  zones_stats


# Build home matrix
do.call("cbind",
        lapply(as.list(player_df$player_id), 
               is_on,
               zones_pbp,
               venue = "Home"
               )
        ) %>%
  Matrix(sparse = TRUE) ->
  home_on_mat

colnames(home_on_mat) <- paste(player_df$player_id, "home", sep = "_")

# Build away matrix
do.call("cbind",
        lapply(as.list(player_df$player_id), 
               is_on,
               zones_pbp,
               venue = "Away"
               )
        ) %>%
  Matrix(sparse = TRUE) ->
  away_on_mat

colnames(away_on_mat) <- paste(player_df$player_id, "away", sep = "_")

# Build design matrix
zones_pbp$game_strength_state <- as.factor(zones_pbp$game_strength_state)
zones_pbp$home_zonestart <- as.factor(zones_pbp$home_zonestart)
zones_pbp$home_zonestart_next <- as.factor(zones_pbp$home_zonestart_next)

design_zones <- dummyVars(home_zonestart_next ~
                          game_strength_state +
                          home_score_adv*game_seconds +
                          home_zonestart*seconds_since,
                          data = zones_pbp,
                          contrasts = TRUE,
                          fullRank = FALSE
                          )

zones_mat <- cBind(predict(design_zones,
                           zones_pbp
                           ) %>%
                   Matrix(sparse = TRUE),
                   home_on_mat,
                   away_on_mat
                   )

########################################################################################################################
########################################################################################################################
########################################################################################################################

### SHOT RATES ###

## Home
home_surv <- Surv(rates_pbp_home$elapsed, rates_pbp_home$home_hazard)

# registerDoMC(cores = 4)
cox_home_rates <- cv.glmnet(x = as.matrix(rates_mat_home),
                            y = home_surv,
                            family = "cox",
                            nfolds = 3,
                            nlambda = 25,
                            alpha = 1,
                            parallel = FALSE
                            )

home_rate_coefs <- data.frame(var = colnames(rates_mat_home),
                              coeff = matrix(exp(coef(cox_home_rates, s = "lambda.min")))
                              )

## Away
away_surv <- Surv(rates_pbp_away$elapsed, rates_pbp_away$away_hazard)

# registerDoMC(cores = 4)
cox_away_rates <- cv.glmnet(x = as.matrix(rates_mat_away),
                            y = away_surv,
                            family = "cox",
                            nfolds = 3,
                            nlambda = 25,
                            alpha = 1,
                            parallel = FALSE
                            )

away_rate_coefs <- data.frame(var = colnames(rates_mat_away),
                              coeff = matrix(exp(coef(cox_away_rates, s = "lambda.min")))
                              )


### SHOT QUALITY ###

# registerDoMC(cores = 4)
glm_qual <- cv.glmnet(x = as.matrix(qual_mat),
                      y = as.numeric(qual_pbp$prob_goal),
                      family = "gaussian",
                      nfolds = 5,
                      nlambda = 100,
                      alpha = 1,
                      parallel = FALSE
                      )

qual_coefs <- data.frame(var = c("Intercept", colnames(qual_mat)),
                         coeff = matrix(coef(glm_qual, s = "lambda.min"))
                         )

### SHOOTING ###

# registerDoMC(cores = 4)
glm_shooting <- cv.glmnet(x = as.matrix(shooting_mat),
                          y = as.factor(shooting_pbp$is_goal),
                          family = "binomial",
                          nfolds = 5,
                          nlambda = 100,
                          alpha = 1,
                          parallel = FALSE
                          )

shooting_coefs <- data.frame(var = c("Intercept", colnames(shooting_mat)),
                             coeff = matrix(exp(coef(glm_shooting, s = "lambda.min")))
                             )


### PENALTIES ###

# registerDoMC(cores = 4)
poiss_taken <- cv.glmnet(x = as.matrix(pens_mat),
                         y = as.numeric(pens_df$PENT),
                         offset = log(as.numeric(pens_df$TOI)),
                         family = "poisson",
                         alpha = 1,
                         nfolds = 5,
                         nlambda = 50,
                          parallel = FALSE
                         )

taken_coefs <- data.frame(var = c("Intercept", colnames(pens_mat)),
                          coeff = matrix(exp(coef(poiss_taken, s = "lambda.min")))
                          )

# registerDoMC(cores = 4)
poiss_drawn <- cv.glmnet(x = as.matrix(pens_mat),
                         y = as.numeric(pens_df$PEND),
                         offset = log(as.numeric(pens_df$TOI)),
                         family = "poisson",
                         alpha = 1,
                         nfolds = 5,
                         nlambda = 50,
                          parallel = FALSE
                         )

drawn_coefs <- data.frame(var = c("Intercept", colnames(pens_mat)),
                          coeff = matrix(exp(coef(poiss_drawn, s = "lambda.min")))
                          )


### ZONE SHIFTS ###

index <- unique(which(is.na(zones_pbp) == TRUE, arr.ind = TRUE)[, 1])

# registerDoMC(cores = 4)
glm_zones <- cv.glmnet(x = as.matrix(zones_mat[-index, ]),
                       y = as.factor(zones_pbp$home_zonestart_next[-index]),
                       weights = 1/log(zones_pbp$seconds_until[-index] + 1, base = 10),
                       family = "multinomial",
                       alpha = 1,
                       nfolds = 5,
                       parallel = FALSE
                       )

zones_coefs <- data.frame(var = c("Intercept", colnames(zones_mat)),
                          coeff_1 = matrix(exp(coef(glm_zones, s = "lambda.min")$`1`)),
                          coeff_2 = matrix(exp(coef(glm_zones, s = "lambda.min")$`2`)),
                          coeff_3 = matrix(exp(coef(glm_zones, s = "lambda.min")$`3`))
                          )


########################################################################################################################
########################################################################################################################
########################################################################################################################

load("/srv/shiny-server/models/replacement_coeffs.RData")

## Shot rates
home_rate_coefs %>%
  filter(grepl("_away|_home", var) == TRUE) %>%
  mutate(player_id = gsub("_away|_home", "", var),
         home = 1*(grepl("_home", var) == TRUE),
         player_name = player_df$player_name[match(player_id, player_df$player_id)]
         ) %>%
  data.frame() ->
  home_rate_coefs

away_rate_coefs %>%
  filter(grepl("_away|_home", var) == TRUE) %>%
  mutate(player_id = gsub("_away|_home", "", var),
         away = 1*(grepl("_away", var) == TRUE),
         player_name = player_df$player_name[match(player_id, player_df$player_id)]
         ) %>%
  data.frame() ->
  away_rate_coefs

home_rate_coefs %>%
  group_by(player_id, player_name) %>%
  summarise(RF_home = sum((home == 1)*coeff),
            RA_away = sum((home == 0)*coeff)
            ) %>%
  data.frame() ->
  home_rates_sum

away_rate_coefs %>%
  group_by(player_id, player_name) %>%
  summarise(RF_away = sum((away == 1)*coeff),
            RA_home = sum((away == 0)*coeff)
            ) %>%
  data.frame() ->
  away_rates_sum

merge(home_rates_sum,
      away_rates_sum,
      by.x = c("player_id", "player_name"),
      by.y = c("player_id", "player_name")
      ) %>%
  data.frame() %>%
  mutate(HTOI = skater_stats$HTOI[match(player_id, skater_stats$player)],
         ATOI = skater_stats$ATOI[match(player_id, skater_stats$player)],
         WAR_RF = (RF_home - 1)*baseline_rates$home_rate*HTOI*baseline_rates$home_fsh + 
                  (RF_away - 1)*baseline_rates$away_rate*ATOI*baseline_rates$away_fsh,
         WAR_RA = (1 - RA_home)*baseline_rates$away_rate*HTOI*baseline_rates$away_fsh + 
                  (1 - RA_away)*baseline_rates$home_rate*ATOI*baseline_rates$home_fsh,
         WAR_Rates = WAR_RF + WAR_RA
         ) %>%
  data.frame() ->
  rates_sum


## Shot quality
qual_coefs %>%
  filter(grepl("_for|_against", var) == TRUE) %>%
  mutate(player_id = gsub("_for|_against", "", var),
         is_for = 1*(grepl("_for", var) == TRUE),
         player_name = player_df$player_name[match(player_id, player_df$player_id)]
         ) %>%
  data.frame() ->
  qual_coefs

qual_coefs %>%
  group_by(player_id, player_name) %>%
  summarise(QF = sum((is_for == 1)*coeff),
            QA = sum((is_for == 0)*coeff)
            ) %>%
  mutate(FF = skater_stats$FF[match(player_id, skater_stats$player)],
         FA = skater_stats$FA[match(player_id, skater_stats$player)],
         WAR_QF = QF*FF,
         WAR_QA = -QA*FA,
         WAR_Qual = WAR_QF + WAR_QA
         ) %>%
  data.frame() ->
  qual_sum

## Shooting
shooting_coefs %>%
  filter(grepl("event_player_1\\.|goalie\\.", var) == TRUE) %>%
  mutate(player_id = gsub("event_player_1\\.|goalie\\.", "", var),
         is_shooter = 1*(grepl("event_player_1", var) == TRUE),
         player_name = player_df$player_name[match(player_id, player_df$player_id)]
         ) %>%
  data.frame() ->
  shooting_coefs

shooting_coefs %>%
  group_by(player_id, player_name) %>%
  summarise(SF = sum((is_shooter == 1)*coeff)
            ) %>%
  mutate(iFF = skater_stats$iFF[match(player_id, skater_stats$player)],
         WAR_Shooting = OR2dProb(SF, baseline_rates$total_fsh)*iFF
         ) %>%
  data.frame() ->
  shooting_sum

## Penalties
taken_coefs %>%
  filter(grepl("player_", var) == TRUE) %>%
  mutate(player_id = gsub("player_", "", var),
         player_name = player_df$player_name[match(player_id, player_df$player_id)]
         ) %>%
  data.frame() ->
  taken_coefs

drawn_coefs %>%
  filter(grepl("player_", var) == TRUE) %>%
  mutate(player_id = gsub("player_", "", var),
         player_name = player_df$player_name[match(player_id, player_df$player_id)]
         ) %>%
  data.frame() ->
  drawn_coefs

merge(taken_coefs %>%
        rename(coef_taken = coeff) %>%
        data.frame(),
      drawn_coefs %>%
        rename(coef_drawn = coeff) %>%
        data.frame(),
      by.x = c("player_id", "player_name"),
      by.y = c("player_id", "player_name")
      ) %>%
  data.frame() %>%
  mutate(TOI = skater_stats$HTOI[match(player_id, skater_stats$player)] +
               skater_stats$ATOI[match(player_id, skater_stats$player)],
         WAR_PT = (1 - coef_taken)*baseline_pens$penalty_rate*TOI*penalty_value,
         WAR_PD = (coef_drawn - 1)*baseline_pens$penalty_rate*TOI*penalty_value,
         WAR_Pens = WAR_PT + WAR_PD
         ) %>%
  data.frame() ->
  pens_sum

## Zone shifts
zones_coefs %>%
  filter(grepl("_away|_home", var) == TRUE) %>%
  mutate(player_id = gsub("_away|_home", "", var),
         home = 1*(grepl("_home", var) == TRUE),
         player_name = player_df$player_name[match(player_id, player_df$player_id)]
         ) %>%
  data.frame() ->
  zones_coefs

zones_coefs %>%
  group_by(player_id, player_name) %>%
  summarise(Z1_home = sum((home == 1)*coeff_1),
            Z1_away = sum((home == 0)*coeff_1),
            Z2_home = sum((home == 1)*coeff_2),
            Z2_away = sum((home == 0)*coeff_2),
            Z3_home = sum((home == 1)*coeff_3),
            Z3_away = sum((home == 0)*coeff_3)
            ) %>%
  mutate(HZF = zones_stats$HZF[match(player_id, zones_stats$player)],
         AZF = zones_stats$AZF[match(player_id, zones_stats$player)],
         WAR_DZF = OR2dProb(Z1_home, baseline_zones$pct_1)*HZF*faceoff_value$value[which(faceoff_value$home_zonestart == 1)] -
                   OR2dProb(Z3_away, baseline_zones$pct_3)*AZF*faceoff_value$value[which(faceoff_value$home_zonestart == 3)],
         WAR_NZF = OR2dProb(Z2_home, baseline_zones$pct_2)*HZF*faceoff_value$value[which(faceoff_value$home_zonestart == 2)] -
                   OR2dProb(Z2_away, baseline_zones$pct_2)*AZF*faceoff_value$value[which(faceoff_value$home_zonestart == 2)],
         WAR_OZF = OR2dProb(Z3_home, baseline_zones$pct_3)*HZF*faceoff_value$value[which(faceoff_value$home_zonestart == 3)] -
                   OR2dProb(Z1_away, baseline_zones$pct_1)*AZF*faceoff_value$value[which(faceoff_value$home_zonestart == 1)],
         WAR_Zones = WAR_DZF + WAR_NZF + WAR_OZF
         ) %>%
  data.frame() ->
  zones_sum


## WAR
merge(rates_sum %>%
        select(player_id, player_name, WAR_RF, WAR_RA, WAR_Rates) %>%
        data.frame(),
      qual_sum %>%
        select(player_id, player_name, WAR_QF, WAR_QA, WAR_Qual) %>%
        data.frame(),
      by.x = c("player_id", "player_name"),
      by.y = c("player_id", "player_name")
      ) %>%
  merge(shooting_sum %>%
        select(player_id, player_name, WAR_Shooting) %>%
        data.frame(),
        by.x = c("player_id", "player_name"),
        by.y = c("player_id", "player_name")
        ) %>%
  merge(pens_sum %>%
        select(player_id, player_name, WAR_PT, WAR_PD, WAR_Pens) %>%
        data.frame(),
        by.x = c("player_id", "player_name"),
        by.y = c("player_id", "player_name")
        ) %>%
  merge(zones_sum %>%
        select(player_id, player_name, WAR_DZF, WAR_NZF, WAR_OZF, WAR_Zones) %>%
        data.frame(),
        by.x = c("player_id", "player_name"),
        by.y = c("player_id", "player_name")
        ) %>%
  mutate(season = season,
         OWAR = WAR_RF + WAR_QF + WAR_Shooting + WAR_PD + WAR_OZF,
         DWAR = WAR_RA + WAR_QA + WAR_PT + WAR_DZF + WAR_NZF,
         WAR = WAR_Rates + WAR_Qual + WAR_Shooting + WAR_Pens + WAR_Zones
         ) %>%
  data.frame() ->
  war_df

war_df$position <- as.character(player_df$player_position[match(war_df$player_id, player_df$player_id)])
war_df$position[which(grepl("R|L|C", war_df$position) == TRUE)] <- "F"

bind_rows(war_df %>%
            filter(position == "F") %>%
            mutate(HTOI = skater_stats$HTOI[match(player_id, skater_stats$player)],
                   ATOI = skater_stats$ATOI[match(player_id, skater_stats$player)],
                   FF = skater_stats$FF[match(player_id, skater_stats$player)],
                   FA = skater_stats$FA[match(player_id, skater_stats$player)],
                   iFF = skater_stats$iFF[match(player_id, skater_stats$player)],
                   TOI = HTOI + ATOI,
                   HZF = zones_stats$HZF[match(player_id, zones_stats$player)],
                   AZF = zones_stats$AZF[match(player_id, zones_stats$player)],
                   REP_RF = (rep_coefs_summary$f_home_rate_for - 1)*baseline_rates$home_rate*HTOI*baseline_rates$home_fsh + 
                            (rep_coefs_summary$f_away_rate_for - 1)*baseline_rates$away_rate*ATOI*baseline_rates$away_fsh,
                   REP_RA = (1 - rep_coefs_summary$f_home_rate_aga)*baseline_rates$away_rate*HTOI*baseline_rates$away_fsh + 
                            (1 - rep_coefs_summary$f_away_rate_aga)*baseline_rates$home_rate*ATOI*baseline_rates$home_fsh,
                   REP_Rates = REP_RF + REP_RA,
                   REP_QF = rep_coefs_summary$f_qual_for*FF,
                   REP_QA = -rep_coefs_summary$f_qual_aga*FA,
                   REP_Qual = REP_QF + REP_QA,
                   REP_Shooting = OR2dProb(rep_coefs_summary$f_shooting, baseline_rates$total_fsh)*iFF,
                   REP_PT = (1 - rep_coefs_summary$f_taken)*baseline_pens$penalty_rate*TOI*penalty_value,
                   REP_PD = (rep_coefs_summary$f_drawn - 1)*baseline_pens$penalty_rate*TOI*penalty_value,
                   REP_Pens = REP_PT + REP_PD,
                   REP_DZF = OR2dProb(rep_coefs_summary$f_home_dz, baseline_zones$pct_1)*HZF*faceoff_value$value[which(faceoff_value$home_zonestart == 1)] -
                             OR2dProb(rep_coefs_summary$f_away_dz, baseline_zones$pct_3)*AZF*faceoff_value$value[which(faceoff_value$home_zonestart == 3)],
                   REP_NZF = OR2dProb(rep_coefs_summary$f_home_nz, baseline_zones$pct_2)*HZF*faceoff_value$value[which(faceoff_value$home_zonestart == 2)] -
                             OR2dProb(rep_coefs_summary$f_away_nz, baseline_zones$pct_2)*AZF*faceoff_value$value[which(faceoff_value$home_zonestart == 2)],
                   REP_OZF = OR2dProb(rep_coefs_summary$f_home_oz, baseline_zones$pct_3)*HZF*faceoff_value$value[which(faceoff_value$home_zonestart == 3)] -
                             OR2dProb(rep_coefs_summary$f_away_oz, baseline_zones$pct_1)*AZF*faceoff_value$value[which(faceoff_value$home_zonestart == 1)],
                   REP_Zones = REP_DZF + REP_NZF + REP_OZF
                   ) %>%
            data.frame(),
          
          war_df %>%
            filter(position == "D") %>%
            mutate(HTOI = skater_stats$HTOI[match(player_id, skater_stats$player)],
                   ATOI = skater_stats$ATOI[match(player_id, skater_stats$player)],
                   FF = skater_stats$FF[match(player_id, skater_stats$player)],
                   FA = skater_stats$FA[match(player_id, skater_stats$player)],
                   iFF = skater_stats$iFF[match(player_id, skater_stats$player)],
                   TOI = HTOI + ATOI,
                   HZF = zones_stats$HZF[match(player_id, zones_stats$player)],
                   AZF = zones_stats$AZF[match(player_id, zones_stats$player)],
                   REP_RF = (rep_coefs_summary$d_home_rate_for - 1)*baseline_rates$home_rate*HTOI*baseline_rates$home_fsh + 
                            (rep_coefs_summary$d_away_rate_for - 1)*baseline_rates$away_rate*ATOI*baseline_rates$away_fsh,
                   REP_RA = (1 - rep_coefs_summary$d_home_rate_aga)*baseline_rates$away_rate*HTOI*baseline_rates$away_fsh + 
                            (1 - rep_coefs_summary$d_away_rate_aga)*baseline_rates$home_rate*ATOI*baseline_rates$home_fsh,
                   REP_Rates = REP_RF + REP_RA,
                   REP_QF = rep_coefs_summary$d_qual_for*FF,
                   REP_QA = -rep_coefs_summary$d_qual_aga*FA,
                   REP_Qual = REP_QF + REP_QA,
                   REP_Shooting = OR2dProb(rep_coefs_summary$d_shooting, baseline_rates$total_fsh)*iFF,
                   REP_PT = (1 - rep_coefs_summary$d_taken)*baseline_pens$penalty_rate*TOI*penalty_value,
                   REP_PD = (rep_coefs_summary$d_drawn - 1)*baseline_pens$penalty_rate*TOI*penalty_value,
                   REP_Pens = REP_PT + REP_PD,
                   REP_DZF = OR2dProb(rep_coefs_summary$d_home_dz, baseline_zones$pct_1)*HZF*faceoff_value$value[which(faceoff_value$home_zonestart == 1)] -
                             OR2dProb(rep_coefs_summary$d_away_dz, baseline_zones$pct_3)*AZF*faceoff_value$value[which(faceoff_value$home_zonestart == 3)],
                   REP_NZF = OR2dProb(rep_coefs_summary$d_home_nz, baseline_zones$pct_2)*HZF*faceoff_value$value[which(faceoff_value$home_zonestart == 2)] -
                             OR2dProb(rep_coefs_summary$d_away_nz, baseline_zones$pct_2)*AZF*faceoff_value$value[which(faceoff_value$home_zonestart == 2)],
                   REP_OZF = OR2dProb(rep_coefs_summary$d_home_oz, baseline_zones$pct_3)*HZF*faceoff_value$value[which(faceoff_value$home_zonestart == 3)] -
                             OR2dProb(rep_coefs_summary$d_away_oz, baseline_zones$pct_1)*AZF*faceoff_value$value[which(faceoff_value$home_zonestart == 1)],
                   REP_Zones = REP_DZF + REP_NZF + REP_OZF
                   ) %>%
            data.frame()
          ) %>%
  data.frame() %>%
  mutate(WAR_RF = (WAR_RF - REP_RF)/4.5,
         WAR_RA = (WAR_RA - REP_RA)/4.5,
         WAR_Rates = WAR_RF + WAR_RA,
         WAR_QF = (WAR_QF - REP_QF)/4.5,
         WAR_QA = (WAR_QA - REP_QA)/4.5,
         WAR_Qual = WAR_QF + WAR_QA,
         WAR_Shooting = (WAR_Shooting - REP_Shooting)/4.5,
         WAR_PT = (WAR_PT - REP_PT)/4.5,
         WAR_PD = (WAR_PD - REP_PD)/4.5,
         WAR_Pens = WAR_PT + WAR_PD,
         WAR_OZF = (WAR_OZF - REP_OZF)/4.5,
         WAR_NZF = (WAR_NZF - REP_NZF)/4.5,
         WAR_DZF = (WAR_DZF - REP_DZF)/4.5,
         WAR_Zones = WAR_OZF + WAR_NZF + WAR_DZF,
         OWAR = WAR_RF + WAR_QF + WAR_Shooting + WAR_PD + WAR_OZF,
         DWAR = WAR_RA + WAR_QA + WAR_PT + WAR_DZF + WAR_NZF,
         WAR = WAR_Rates + WAR_Qual + WAR_Shooting + WAR_Pens + WAR_Zones
         ) %>%
  data.frame() ->
  war_df


## Save
save(list = "war_df",
     file = paste("~/corsica_data/war_ratings_",
                  season,
                  ".RData",
                  sep = ""
                  )
     )

