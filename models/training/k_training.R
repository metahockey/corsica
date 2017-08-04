### K TRAINING ###
# Last edit: Manny (2017-06-07)

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
OR2dProb <- function(OR, prob1) {
  
  prob2 <- (OR - 1)*(prob1/(1 - prob1))/
           (1 + (OR - 1)*(prob1/(1 - prob1)))
  
  return(prob2)
  
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

# Get team list
tlist <- c(pbp$home_team,
           pbp$away_team
           ) %>%
  na.omit()

data.frame(team = tlist) %>%
  group_by(team) %>%
  summarise(events = n()) %>%
  filter(events > 1000
         ) %>%
  data.frame() ->
  team_df

# Search names
do.call("rbind",
        lapply(as.list(team_df$team), 
               ds.scrape_team_profile,
               try_tolerance = 3,
               agents = ds.user_agents
               )
       ) %>%
  data.frame() ->
  team_name

team_df$team_name <- team_name$team_alias

# Team stats
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(home_team) %>%
    rename(team = home_team) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == team),
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team)
              ),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(away_team) %>%
    rename(team = away_team) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              FF = sum(event_type %in% st.fenwick_events & event_team == team),
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team)
              )
) %>%
  data.frame() %>%
  group_by(team) %>%
  summarise(HTOI = sum(na.omit(TOI*(venue == "home"))),
            ATOI = sum(na.omit(TOI*(venue == "away"))),
            FF = sum(FF),
            FA = sum(FA)
            ) %>%
  data.frame() ->
  team_stats

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

# Build design matrix
rates_pbp$game_strength_state <- as.factor(rates_pbp$game_strength_state)
rates_pbp$home_zonestart <- as.factor(rates_pbp$home_zonestart)
rates_pbp$home_team <- as.factor(rates_pbp$home_team)
rates_pbp$away_team <- as.factor(rates_pbp$away_team)

rates_pbp$home_hazard <- 1*(rates_pbp$event_type %in% st.fenwick_events & rates_pbp$event_team == rates_pbp$home_team)
rates_pbp$away_hazard <- 1*(rates_pbp$event_type %in% st.fenwick_events & rates_pbp$event_team == rates_pbp$away_team)

design_rates <- dummyVars(home_hazard ~
                          home_team + away_team +
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
                   Matrix(sparse = TRUE)
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

# Home
home_surv <- Surv(rates_pbp_home$elapsed, rates_pbp_home$home_hazard)

registerDoMC(cores = 4)
cox_home_rates <- cv.glmnet(x = as.matrix(rates_mat_home),
                            y = home_surv,
                            family = "cox",
                            nfolds = 5,
                            nlambda = 100,
                            alpha = 1,
                            parallel = TRUE
                            )

home_rate_coefs <- data.frame(var = colnames(rates_mat_home),
                              coeff = matrix(exp(coef(cox_home_rates, s = "lambda.min")))
                              )

# Away
away_surv <- Surv(rates_pbp_away$elapsed, rates_pbp_away$away_hazard)

registerDoMC(cores = 4)
cox_away_rates <- cv.glmnet(x = as.matrix(rates_mat_away),
                            y = away_surv,
                            family = "cox",
                            nfolds = 5,
                            nlambda = 100,
                            alpha = 1,
                            parallel = TRUE
                            )

away_rate_coefs <- data.frame(var = colnames(rates_mat_away),
                              coeff = matrix(exp(coef(cox_away_rates, s = "lambda.min")))
                              )


## Shot Quality
# Prepare PBP
newpbp %>%
  filter(event_type %in% st.fenwick_events,
         !is.na(home_zonestart),
         !is.na(prob_goal)
         ) %>%
  mutate(is_home_team = 1*(event_team == home_team),
         shooter = event_team,
         goalie = ifelse(event_team == home_team,
                         away_team,
                         home_team
                         )
         ) %>%
  data.frame() ->
  qual_pbp

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
qual_pbp$shooter <- as.factor(qual_pbp$shooter)
qual_pbp$goalie <- as.factor(qual_pbp$goalie)

design_qual <- dummyVars(prob_goal ~
                         shooter + goalie +
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
                  Matrix(sparse = TRUE)
                  )

registerDoMC(cores = 4)
glm_qual <- cv.glmnet(x = as.matrix(qual_mat),
                      y = as.numeric(qual_pbp$prob_goal),
                      family = "gaussian",
                      nfolds = 5,
                      nlambda = 100,
                      alpha = 1,
                      parallel = TRUE
                      )

qual_coefs <- data.frame(var = c("Intercept", colnames(qual_mat)),
                         coeff = matrix(coef(glm_qual, s = "lambda.min"))
                         )


## Shooting
# Prepare PBP
newpbp %>%
  filter(event_type %in% st.fenwick_events,
         !{{is.na(home_goalie) == TRUE & event_team == away_team} | 
           {is.na(away_goalie) == TRUE & event_team == home_team}
           },
         !is.na(prob_goal)
         ) %>%
  mutate(is_home_team = 1*(event_team == home_team)) %>%
  data.frame() ->
  shooting_pbp

# Build design matrix
shooting_pbp$goalie <- ifelse(shooting_pbp$is_home_team == 1,
                              shooting_pbp$away_team,
                              shooting_pbp$home_team
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
shooting_pbp$event_team <- as.factor(shooting_pbp$event_team)
shooting_pbp$goalie <- as.factor(shooting_pbp$goalie)
shooting_pbp$is_goal <- as.factor(1 + 1*(shooting_pbp$event_type == "GOAL"))

design_shooting <- dummyVars(as.factor(is_goal) ~
                             event_team + goalie +
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
                        Matrix(sparse = TRUE)
                      )

registerDoMC(cores = 4)
glm_shooting <- cv.glmnet(x = as.matrix(shooting_mat),
                          y = as.factor(shooting_pbp$is_goal),
                          family = "binomial",
                          nfolds = 5,
                          nlambda = 100,
                          alpha = 1,
                          parallel = TRUE
                          )

shooting_coefs <- data.frame(var = c("Intercept", colnames(shooting_mat)),
                             coeff = matrix(exp(coef(glm_shooting, s = "lambda.min")))
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
    group_by(game_id, game_strength_state, home_score_adv, streak, home_team) %>%
    rename(team = home_team) %>%
    summarise(venue = "home",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_team == team) +
                                 1*(event_type == "PENALTY" & event_team == team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_team == team & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_team == away_team) +
                                 1*(event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_team == away_team & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              ),
  
  pens_pbp %>%
    filter(!{game_period > 4 & session == "R"},
         game_strength_state %in% st.strength_states
         ) %>%
    group_by(game_id, game_strength_state, home_score_adv, streak, away_team) %>%
    rename(team = away_team) %>%
    summarise(venue = "away",
              TOI = sum(na.omit(event_length))/60,
              PENT = sum(na.omit(1*(event_type == "PENALTY" & event_team == team) +
                                 1*(event_type == "PENALTY" & event_team == team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_team == team & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 )),
              PEND = sum(na.omit(1*(event_type == "PENALTY" & event_team == home_team) +
                                 1*(event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                 1*(event_type == "PENALTY" & event_team == home_team & {{grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE} | {pens > 1}})
                                 ))
              )
) %>%
  data.frame() %>%
  group_by(game_id, team, venue, game_strength_state, home_score_adv, streak) %>%
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
pens_df$team <- as.factor(pens_df$team)

design_pens <- dummyVars(PENT ~
                         team +
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
                    Matrix(sparse = TRUE)
                  )

registerDoMC(cores = 4)
poiss_taken <- cv.glmnet(x = as.matrix(pens_mat),
                         y = as.numeric(pens_df$PENT),
                         offset = log(as.numeric(pens_df$TOI)),
                         family = "poisson",
                         alpha = 1,
                         nfolds = 5,
                         nlambda = 100,
                         parallel = TRUE
                         )

taken_coefs <- data.frame(var = c("Intercept", colnames(pens_mat)),
                          coeff = matrix(exp(coef(poiss_taken, s = "lambda.min")))
                          )

registerDoMC(cores = 4)
poiss_drawn <- cv.glmnet(x = as.matrix(pens_mat),
                         y = as.numeric(pens_df$PEND),
                         offset = log(as.numeric(pens_df$TOI)),
                         family = "poisson",
                         alpha = 1,
                         nfolds = 5,
                         nlambda = 100,
                         parallel = TRUE
                         )

drawn_coefs <- data.frame(var = c("Intercept", colnames(pens_mat)),
                          coeff = matrix(exp(coef(poiss_drawn, s = "lambda.min")))
                          )


############################################################################################################################################
############################################################################################################################################

## Shot rates
home_rate_coefs %>%
  filter(grepl("away_team\\.|home_team\\.", var) == TRUE) %>%
  mutate(team_id = gsub("away_team\\.|home_team\\.", "", var),
         home = 1*(grepl("home_team\\.", var) == TRUE),
         team_name = team_df$team_name[match(team_id, team_df$team)]
         ) %>%
  data.frame() ->
  home_rate_coefs

away_rate_coefs %>%
  filter(grepl("away_team\\.|home_team\\.", var) == TRUE) %>%
  mutate(team_id = gsub("away_team\\.|home_team\\.", "", var),
         away = 1*(grepl("away_team\\.", var) == TRUE),
         team_name = team_df$team_name[match(team_id, team_df$team)]
         ) %>%
  data.frame() ->
  away_rate_coefs

home_rate_coefs %>%
  group_by(team_id, team_name) %>%
  summarise(RF_home = sum((home == 1)*coeff),
            RA_away = sum((home == 0)*coeff)
            ) %>%
  data.frame() ->
  home_rates_sum

away_rate_coefs %>%
  group_by(team_id, team_name) %>%
  summarise(RF_away = sum((away == 1)*coeff),
            RA_home = sum((away == 0)*coeff)
            ) %>%
  data.frame() ->
  away_rates_sum

merge(home_rates_sum,
      away_rates_sum,
      by.x = c("team_id", "team_name"),
      by.y = c("team_id", "team_name")
      ) %>%
  data.frame() %>%
  mutate(HTOI = team_stats$HTOI[match(team_id, team_stats$team)],
         ATOI = team_stats$ATOI[match(team_id, team_stats$team)],
         K_RF = (RF_home - 1)*baseline_rates$home_rate*HTOI*baseline_rates$home_fsh + 
                  (RF_away - 1)*baseline_rates$away_rate*ATOI*baseline_rates$away_fsh,
         K_RA = (1 - RA_home)*baseline_rates$away_rate*HTOI*baseline_rates$away_fsh + 
                  (1 - RA_away)*baseline_rates$home_rate*ATOI*baseline_rates$home_fsh,
         K_Rates = K_RF + K_RA
         ) %>%
  data.frame() ->
  rates_sum


## Shot quality
qual_coefs %>%
  filter(grepl("shooter\\.|goalie\\.", var) == TRUE) %>%
  mutate(team_id = gsub("shooter\\.|goalie\\.", "", var),
         is_for = 1*(grepl("shooter\\.", var) == TRUE),
         team_name = team_df$team_name[match(team_id, team_df$team)]
         ) %>%
  data.frame() ->
  qual_coefs

qual_coefs %>%
  group_by(team_id, team_name) %>%
  summarise(QF = sum((is_for == 1)*coeff),
            QA = sum((is_for == 0)*coeff)
            ) %>%
  mutate(FF = team_stats$FF[match(team_id, team_stats$team)],
         FA = team_stats$FA[match(team_id, team_stats$team)],
         K_QF = QF*FF,
         K_QA = -QA*FA,
         K_Qual = K_QF + K_QA
         ) %>%
  data.frame() ->
  qual_sum

## Shooting
shooting_coefs %>%
  filter(grepl("event_team\\.|goalie\\.", var) == TRUE) %>%
  mutate(team_id = gsub("event_team\\.|goalie\\.", "", var),
         is_shooter = 1*(grepl("event_team", var) == TRUE),
         team_name = team_df$team_name[match(team_id, team_df$team)]
         ) %>%
  data.frame() ->
  shooting_coefs

shooting_coefs %>%
  group_by(team_id, team_name) %>%
  summarise(SF = sum((is_shooter == 1)*coeff),
            SA = sum((is_shooter == 0)*coeff)
            ) %>%
  mutate(FF = team_stats$FF[match(team_id, team_stats$team)],
         FA = team_stats$FA[match(team_id, team_stats$team)],
         K_Shooting = OR2dProb(SF, baseline_rates$total_fsh)*FF,
         K_Goalie = -OR2dProb(SA, baseline_rates$total_fsh)*FA
         ) %>%
  data.frame() ->
  shooting_sum

## Penalties
taken_coefs %>%
  filter(grepl("team\\.", var) == TRUE) %>%
  mutate(team_id = gsub("team\\.", "", var),
         team_name = team_df$team_name[match(team_id, team_df$team)]
         ) %>%
  data.frame() ->
  taken_coefs

drawn_coefs %>%
  filter(grepl("team\\.", var) == TRUE) %>%
  mutate(team_id = gsub("team\\.", "", var),
         team_name = team_df$team_name[match(team_id, team_df$team)]
         ) %>%
  data.frame() ->
  drawn_coefs

merge(taken_coefs %>%
        rename(coef_taken = coeff) %>%
        data.frame(),
      drawn_coefs %>%
        rename(coef_drawn = coeff) %>%
        data.frame(),
      by.x = c("team_id", "team_name"),
      by.y = c("team_id", "team_name")
      ) %>%
  data.frame() %>%
  mutate(TOI = team_stats$HTOI[match(team_id, team_stats$team)] +
               team_stats$ATOI[match(team_id, team_stats$team)],
         K_PT = (1 - coef_taken)*baseline_pens$penalty_rate*TOI*penalty_value,
         K_PD = (coef_drawn - 1)*baseline_pens$penalty_rate*TOI*penalty_value,
         K_Pens = K_PT + K_PD
         ) %>%
  data.frame() ->
  pens_sum


## K
merge(rates_sum %>%
        select(team_id, team_name, K_RF, K_RA, K_Rates) %>%
        data.frame(),
      qual_sum %>%
        select(team_id, team_name, K_QF, K_QA, K_Qual) %>%
        data.frame(),
      by.x = c("team_id", "team_name"),
      by.y = c("team_id", "team_name")
      ) %>%
  merge(shooting_sum %>%
        select(team_id, team_name, K_Shooting, K_Goalie) %>%
        data.frame(),
        by.x = c("team_id", "team_name"),
        by.y = c("team_id", "team_name")
        ) %>%
  merge(pens_sum %>%
        select(team_id, team_name, K_PT, K_PD, K_Pens) %>%
        data.frame(),
        by.x = c("team_id", "team_name"),
        by.y = c("team_id", "team_name")
        ) %>%
  mutate(season = season,
         OK = K_RF + K_QF + K_Shooting + K_PD,
         DK = K_RA + K_QA + K_PT + K_Goalie,
         K = K_Rates + K_Qual + K_Shooting + K_Goalie + K_Pens
         ) %>%
  data.frame() ->
  k_df


## Save
save(list = "k_df",
     file = paste("~/corsica_data/k_ratings_",
                  season,
                  ".RData",
                  sep = ""
                  )
     )

