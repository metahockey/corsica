### xG TESTING ###
# Last edit: Manny (2017-06-03)

## Dependencies
require(dplyr); require(RSQLite); require(doMC)
require(glmnet); require(Kmisc); require(keras);
require(caret); require(xgboost)
load("/srv/shiny-server/modules/user_functions.RData")
load("/srv/shiny-server/modules/stats.RData")
load("/srv/shiny-server/models/xg_model.RData")
load("/srv/shiny-server/models/adjustments_model.RData")

# load("/srv/shiny-server/models/development/xg_testing_data.RData")

## Functions
# Meta

# General
ftable2df <- function(mydata) {
  
  ifelse(class(mydata) == "ftable", 
         mydata <- mydata, 
         mydata <- ftable(mydata)
         )
  
  dfrows <- rev(expand.grid(rev(attr(mydata, "row.vars"))))
  
  dfcols <- as.data.frame.matrix(mydata)
  
  do.call(paste, 
          c(rev(expand.grid(rev(attr(mydata, "col.vars")))), 
            sep = "_"
            )
          ) -> names(dfcols)
  
  cbind(dfrows, dfcols)
  
}

## Load data
# Connect to database
conn <- dbConnect(SQLite(), "~/corsica_data/raw.sqlite")

# Read table
pbp <- dbReadTable(conn, "pbp")

# Disconnect
dbDisconnect(conn)


## Prepare data
# Enhance PBP
pbp <- st.pbp_enhance(pbp)

# Add additional features
pbp %>%
  filter(event_type %in% c("FACEOFF",
                           "TAKEAWAY", 
                           "BLOCKED_SHOT", 
                           "SHOT", 
                           "MISSED_SHOT", 
                           "HIT", 
                           "GIVEAWAY",
                           "GOAL"
                           )
         ) %>%
  group_by(season, game_id) %>%
  arrange(event_index) %>%
  mutate(seconds_since_last = game_seconds - lag(game_seconds, 1),
         event_type_last = lag(event_type, 1),
         event_team_last = lag(event_team, 1),
         event_rinkside_last = lag(event_rinkside, 1),
         coords_x_last = lag(coords_x, 1),
         coords_y_last = lag(coords_y, 1),
         seconds_since_last2 = game_seconds - lag(game_seconds, 2),
         event_type_last2 = lag(event_type, 2),
         event_team_last2 = lag(event_team, 2),
         event_rinkside_last2 = lag(event_rinkside, 2),
         coords_x_last2 = lag(coords_x, 2),
         coords_y_last2 = lag(coords_y, 2)
         ) %>%
  ungroup() %>%
  arrange(season, game_id, event_index) %>%
  select(season,
         session,
         game_id,
         game_date,
         game_period,
         game_seconds,
         game_strength_state,
         game_score_state,
         home_goalie,
         away_goalie,
         home_score,
         away_score,
         home_skaters,
         away_skaters,
         home_team,
         away_team,
         event_type,
         event_detail,
         event_distance,
         event_angle,
         event_rinkside,
         event_team,
         coords_x,
         coords_y,
         seconds_since_last,
         event_type_last,
         event_team_last,
         event_rinkside_last,
         coords_x_last,
         coords_y_last,
         seconds_since_last2,
         event_type_last2,
         event_team_last2,
         event_rinkside_last2,
         coords_x_last2,
         coords_y_last2
         ) %>%
  data.frame() ->
  pbp

# Select model data
pbp %>%
  filter(event_type %in% st.fenwick_events,
         !{game_period > 4 & session == "R"},
         !is.na(coords_x),
         !is.na(coords_y),
         game_strength_state %in% c("5v5",
                                    "4v4",
                                    "3v3",
                                    "5v4",
                                    "4v5",
                                    "5v3",
                                    "3v5",
                                    "4v3",
                                    "3v4",
                                    "5vE",
                                    "Ev5",
                                    "4vE",
                                    "Ev4",
                                    "3vE",
                                    "Ev3"
                                    )
         ) %>%
  mutate(same_team_last = 1*(event_team == event_team_last),
         same_team_last2 = 1*(event_team == event_team_last2),
         is_home_team = 1*(event_team == home_team),
         is_EN = 1*({event_team == home_team & away_goalie == 0} | {event_team == away_team & home_goalie == 0})
         ) %>%
  data.frame() ->
  model_data

# Shooting team reference
model_data$shooter_strength_state <- ifelse(model_data$is_home_team == 1,
                                            model_data$game_strength_state,
                                            str_rev(model_data$game_strength_state)
                                            )

model_data$shooter_score_adv <- ifelse(model_data$is_home_team == 1,
                                       model_data$home_score - model_data$away_score,
                                       model_data$away_score - model_data$home_score
                                       )

# Fix NA
model_data$event_detail[which(is.na(model_data$event_detail) == TRUE)] <- "Unknown"
model_data <- na.omit(model_data)

# Add distance from previous event
model_data$distance_from_last <- sqrt((model_data$coords_x - model_data$coords_x_last)^2 + (model_data$coords_y - model_data$coords_y_last)^2)
model_data$distance_from_last2 <- sqrt((model_data$coords_x - model_data$coords_x_last2)^2 + (model_data$coords_y - model_data$coords_y_last2)^2)

# Add board shots
model_data$along_boards_2 <- 1*(abs(model_data$coords_y) > 40)
model_data$along_boards_3 <- 1*(abs(model_data$coords_y) > 39)
model_data$along_boards_4 <- 1*(abs(model_data$coords_y) > 38)
model_data$along_boards_5 <- 1*(abs(model_data$coords_y) > 37)

# Save
save(list = c("pbp", "model_data"), file = "/srv/shiny-server/models/development/xg_testing_data.RData")


## Averages
# On goal, combined
model_data %>%
  summarise(goals = sum(event_type == "GOAL"),
            shots = sum(event_type %in% st.shot_events),
            unblocked = n()
            ) %>%
  mutate(shooting_percentage = goals/shots,
         fenwick_percentage = goals/unblocked,
         on_goal_percentage = shots/unblocked
         ) %>%
  data.frame()
# Sh% = 0.08918131
# FSh% = 0.06410028
# OG% = 0.7188997388

# Baseline log loss
log_loss(as.matrix(1*(model_data$event_type == "GOAL")), as.matrix(rep(0.06410028, nrow(model_data))))
# LL1 = 0.2378588727
log_loss(as.matrix(1*(model_data$event_type %in% st.shot_events)), as.matrix(rep(0.7188997388, nrow(model_data))))
# LL2 = 0.5939894719


## Model testing
# Response variables
model_data$is_goal <- 1*(model_data$event_type == "GOAL")
model_data$is_save <- 1*(model_data$event_type == "SHOT")

# Feature list
vars <- c("event_distance",
          "event_angle",
          #"coords_x",
          #"coords_y",
          "seconds_since_last",
          "event_type_last",
          #"coords_x_last",
          #"coords_y_last",
          "same_team_last",
          "is_home_team",
          "is_EN",
          "shooter_strength_state",
          "shooter_score_adv",
          "distance_from_last"
          )

# Logistic regression
foreach(i = 1:4, .combine = "rbind") %do% {
  
  seed <- sample(1:nrow(model_data), nrow(model_data)/10, replace = FALSE)
  
  train_data <- model_data[-seed, ]
  test_data <- model_data[seed, ]
  
  model_mat <- data.frame(outcome = as.factor(1*(model_data$is_save) + 2*(model_data$is_goal) + 1),
                          model.matrix(is_goal ~ 
                                       poly(event_distance, 3, raw = TRUE) + poly(event_angle, 3, raw = TRUE) + 
                                       event_type_last*same_team_last*(seconds_since_last + distance_from_last) + 
                                       is_home_team + is_EN + shooter_strength_state + shooter_score_adv,
                                       data = model_data[, c("is_goal", vars)]
                                       )
                          )
  
  train_mat <- model_mat[-seed, ]
  test_mat <- model_mat[seed, ]
  
  glm <- cv.glmnet(x = as.matrix(train_mat[, -1]),
                   y = as.factor(train_mat[, 1]),
                   family = "multinomial",
                   nfolds = 4,
                   nlambda = 100,
                   alpha = 1
                   )
  
  predicted <- predict.cv.glmnet(glm,
                                 newx = as.matrix(test_mat[, -1]),
                                 s = "lambda.min",
                                 type = "response"
                                 )
  
  ftable2df(predicted) %>%
    rename(val = `1`) %>%
    group_by(Var2) %>%
    summarise(prob_miss = sum(val*(Var1 == 1)),
              prob_save = sum(val*(Var1 == 2)),
              prob_goal = sum(val*(Var1 == 3))
              ) %>%
    data.frame() ->
    prob_df
  
  test_data$prob_goal <- as.numeric(prob_df$prob_goal)
  test_data$prob_save <- as.numeric(prob_df$prob_save)
  
  test_data_5 <- filter(test_data, shooter_strength_state == "5v5")
  test_data_nen <- filter(test_data, is_EN == 0)
  
  LL1_all <- log_loss(as.matrix(test_data$is_goal), as.matrix(test_data$prob_goal))
  LL2_all <- log_loss(as.matrix(1*(test_data$is_save + test_data$is_goal)), as.matrix(test_data$prob_save + test_data$prob_goal))
  cat("LL1 (All Situations): ",
      LL1_all,
      " || LL2 (All Situations): ",
      LL2_all,
      "\n",
      sep = ""
      )
  
  LL1_5 <- log_loss(as.matrix(test_data_5$is_goal), as.matrix(test_data_5$prob_goal))
  LL2_5 <- log_loss(as.matrix(1*(test_data_5$is_save + test_data_5$is_goal)), as.matrix(test_data_5$prob_save + test_data_5$prob_goal))
  cat("LL1 (5-on-5): ",
      LL1_5,
      " || LL2 (5-on-5): ",
      LL2_5,
      "\n",
      sep = ""
      )
  
  LL1_nen <- log_loss(as.matrix(test_data_nen$is_goal), as.matrix(test_data_nen$prob_goal))
  LL2_nen <- log_loss(as.matrix(1*(test_data_nen$is_save + test_data_nen$is_goal)), as.matrix(test_data_nen$prob_save + test_data_nen$prob_goal))
  cat("LL1 (Non-EN): ",
      LL1_nen,
      " || LL2 (Non-EN): ",
      LL2_nen,
      "\n",
      sep = ""
      )
  
  test_data %>%
    summarise(goals = sum(is_goal),
              saves = sum(is_save),
              unblocked = n(),
              xgoals = sum(prob_goal),
              xsaves = sum(prob_save)
              ) %>%
    mutate(sh = goals/(goals + saves),
           fsh = goals/unblocked,
           og = (goals + saves)/unblocked,
           xsh = xgoals/(xgoals + xsaves),
           xfsh = xgoals/unblocked,
           xog = (xgoals + xsaves)/unblocked
           ) %>%
    data.frame() ->
    results
    
  data.frame(ll1_all = LL1_all,
             ll2_all = LL2_all,
             ll1_5 = LL1_5,
             ll2_5 = LL2_5,
             ll1_nen = LL1_nen,
             ll2_nen = LL2_nen,
             sh = results$sh,
             fsh = results$fsh,
             og = results$og,
             xsh = results$xsh,
             xfsh = results$xfsh,
             xog = results$xog
             )
  
} ->
  cv_mat

# Baseline
# LL1 (ALL) = 0.2378588727 || LL2 (ALL) = 0.6052891319
# Model V4 (folds = 4, nlambda = 100, alpha = 1, vars = 10, interactions = 3, degree = 3/2)
# LL1 (ALL) = 0.2109291 || LL2 (ALL) = 0.585791 || LL1 (5v5) = 0.1877815 || LL2 (5v5) = 0.586289 || LL1 (NEN) = 0.2058594 || LL2 (NEN) = 0.5851544


# XGBoost
model_data$is_goal <- 1*(model_data$event_type == "GOAL")
model_data$is_save <- 1*(model_data$event_type == "SHOT")
model_data$is_miss <- 1*(model_data$event_type == "MISSED_SHOT")

# Feature list
vars <- c("event_distance",
          "event_angle",
          "coords_x",
          "coords_y",
          "seconds_since_last",
          "event_type_last",
          "coords_x_last",
          "coords_y_last",
          "same_team_last",
          "is_home_team",
          "is_EN",
          "shooter_strength_state",
          "shooter_score_adv",
          "home_skaters",
          "away_skaters",
          "home_score",
          "away_score",
          "session",
          "game_seconds",
          "distance_from_last"
          #"event_type_last2",
          #"coords_x_last2",
          #"coords_y_last2",
          #"same_team_last2",
          #"distance_from_last2"
          )

foreach(i = 1:10, .combine = rbind) %do% {
  
  cat(i, "\n")
  
  seed <- sample(1:nrow(model_data), nrow(model_data)/10, replace = FALSE)
  
  train_data <- model_data[-seed, ]
  testing_data <- model_data[seed, ]
  
  model_mat <- data.frame(outcome = 1*(model_data$is_save) + 2*(model_data$is_goal),
                          model.matrix(~ .,
                                       data = model_data[, vars]
                                       ) %>%
                            normalize()
                          )
  
  train_model_mat <- as.matrix(model_mat[-seed, ])
  
  inseed <- sample(1:nrow(train_model_mat), nrow(train_model_mat)/10, replace = FALSE)
  train_mat <- xgb.DMatrix(as.matrix(train_model_mat[-inseed, -1]), label = train_model_mat[-inseed, 1])
  eval_mat <- xgb.DMatrix(as.matrix(train_model_mat[inseed, -1]), label = train_model_mat[inseed, 1])
  
  test_mat <- xgb.DMatrix(as.matrix(model_mat[seed, -1]), label = model_mat$outcome[seed])
  
  # Train
  xg <- xgb.train(data = train_mat,
                  params = list(eta = 0.05,
                                max_depth = 6,
                                subsample = 0.01,
                                colsample = 0.5,
                                num_parallel_tree = 2,
                                objective = "multi:softprob",
                                eval_metric = "mlogloss",
                                num_class = 3
                                ),
                  nrounds = 500,
                  watchlist = list(train_error = train_mat,
                                   test_error = eval_mat
                                   ),
                  verbose = 1,
                  print_every_n = 5,
                  early_stopping_rounds = 3,
                  save_period = NULL
                  )
  
  # Predict
  probs <- predict(xg,
                   test_mat,
                   reshape = TRUE
                   )
  
  cbind(testing_data,
        probs
        ) %>%
    data.frame() ->
    newdata

  newdata %>%
    rename(prob_goal = X3,
           prob_save = X2
           ) %>%
    data.frame() ->
    test_data
  
  test_data_5 <- filter(test_data, shooter_strength_state == "5v5")
  test_data_nen <- filter(test_data, is_EN == 0)
  
  LL1_all <- log_loss(as.matrix(test_data$is_goal), as.matrix(test_data$prob_goal))
  LL2_all <- log_loss(as.matrix(1*(test_data$is_save + test_data$is_goal)), as.matrix(test_data$prob_save + test_data$prob_goal))
  cat("LL1 (All Situations): ",
      LL1_all,
      " || LL2 (All Situations): ",
      LL2_all,
      "\n",
      sep = ""
      )
  
  LL1_5 <- log_loss(as.matrix(test_data_5$is_goal), as.matrix(test_data_5$prob_goal))
  LL2_5 <- log_loss(as.matrix(1*(test_data_5$is_save + test_data_5$is_goal)), as.matrix(test_data_5$prob_save + test_data_5$prob_goal))
  cat("LL1 (5-on-5): ",
      LL1_5,
      " || LL2 (5-on-5): ",
      LL2_5,
      "\n",
      sep = ""
      )
  
  LL1_nen <- log_loss(as.matrix(test_data_nen$is_goal), as.matrix(test_data_nen$prob_goal))
  LL2_nen <- log_loss(as.matrix(1*(test_data_nen$is_save + test_data_nen$is_goal)), as.matrix(test_data_nen$prob_save + test_data_nen$prob_goal))
  cat("LL1 (Non-EN): ",
      LL1_nen,
      " || LL2 (Non-EN): ",
      LL2_nen,
      "\n",
      sep = ""
      )
  
  test_data %>%
    summarise(goals = sum(is_goal),
              saves = sum(is_save),
              unblocked = n(),
              xgoals = sum(prob_goal),
              xsaves = sum(prob_save)
              ) %>%
    mutate(sh = goals/(goals + saves),
           fsh = goals/unblocked,
           og = (goals + saves)/unblocked,
           xsh = xgoals/(xgoals + xsaves),
           xfsh = xgoals/unblocked,
           xog = (xgoals + xsaves)/unblocked
           ) %>%
    data.frame() ->
    results
    
  data.frame(ll1_all = LL1_all,
             ll2_all = LL2_all,
             ll1_5 = LL1_5,
             ll2_5 = LL2_5,
             ll1_nen = LL1_nen,
             ll2_nen = LL2_nen,
             sh = results$sh,
             fsh = results$fsh,
             og = results$og,
             xsh = results$xsh,
             xfsh = results$xfsh,
             xog = results$xog
             )
  
} -> cv_mat

cv_mat %>%
  colMeans()
# Model parameters: (eta = 0.05, nrounds = 500, subsample = 0.01, colsample = 0.5, max_depth = 6, patience = 3)
# LL1 (ALL) = 0.21094538 || LL2 (ALL) = 0.58473009 || LL1 (5v5) = 0.19100468 || LL2 (5v5) = 0.58523646 || LL1 (NEN) = 0.20817551 || LL2 (NEN) = 0.58432983


# TensorFlow
model_data$is_goal <- 1*(model_data$event_type == "GOAL")
model_data$is_save <- 1*(model_data$event_type == "SHOT")
model_data$is_miss <- 1*(model_data$event_type == "MISSED_SHOT")

# Feature list
vars <- c("event_distance",
          "event_angle",
          "coords_x",
          "coords_y",
          "seconds_since_last",
          "event_type_last",
          "coords_x_last",
          "coords_y_last",
          "same_team_last",
          "is_home_team",
          "is_EN",
          "shooter_strength_state",
          "shooter_score_adv",
          "home_skaters",
          "away_skaters",
          "home_score",
          "away_score",
          "session",
          "game_seconds",
          "distance_from_last"
          #"event_type_last2",
          #"coords_x_last2",
          #"coords_y_last2",
          #"same_team_last2",
          #"distance_from_last2"
          )

foreach(i = 1:10, .combine = rbind) %do% {
  
  cat(i, "\n")
  
  seed <- sample(1:nrow(model_data), nrow(model_data)/10, replace = FALSE)
  
  train_data <- model_data[-seed, ]
  testing_data <- model_data[seed, ]
  
  model_mat <- data.frame(is_goal = model_data$is_goal,
                          is_save = model_data$is_save,
                          is_miss = model_data$is_miss,
                          model.matrix(~ .,
                                       data = model_data[, vars]
                                       ) %>%
                            normalize()
                          )
  
  train_mat <- model_mat[-seed, ]
  test_mat <- model_mat[seed, ]
  
  keras_model_sequential() %>%
    layer_dense(input_shape = ncol(train_mat) - 3, 
                units = 60
                ) %>%
    layer_activation(activation = "relu") %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 60) %>%
    layer_activation(activation = "relu") %>%
    layer_dense(units = 3) %>%
    layer_activity_regularization() %>%
    layer_activation(activation = "softmax") ->
    model_seq
    
  model_seq %>%
    compile(loss = "categorical_crossentropy",
            optimizer = optimizer_sgd(lr = 0.001,
                                      momentum = 0.9,
                                      decay = 0.00000001,
                                      nesterov = TRUE
                                      ),
            metrics = "accuracy"
            )
    
  # Train
  model_seq %>%
    fit(as.matrix(train_mat[, -c(1:3)]),
        as.matrix(train_mat[, c(1:3)]),
        epochs = 500,
        verbose = 2,
        batch_size = 100,
        validation_split = 0.1,
        callbacks = list(callback_early_stopping(monitor = "val_loss",
                                                 patience = 30
                                                 )
                         )
        )
  
  # Evaluate
  model_seq %>%
    evaluate(as.matrix(train_mat[, -c(1:3)]),
             as.matrix(train_mat[, c(1:3)]),
             batch_size = 100
             ) ->
    eval_metrics
  
  cat("\n")
  
  # Predict
  model_seq %>%
    predict(as.matrix(test_mat[, -c(1:3)]),
            batch_size = 100
            ) ->
    prob_mat
  
  cbind(testing_data,
        prob_mat
        ) %>%
    data.frame() ->
    newdata

  newdata %>%
    rename(prob_goal = X1,
           prob_save = X2
           ) %>%
    data.frame() ->
    test_data
  
  test_data_5 <- filter(test_data, shooter_strength_state == "5v5")
  test_data_nen <- filter(test_data, is_EN == 0)
  
  LL1_all <- log_loss(as.matrix(test_data$is_goal), as.matrix(test_data$prob_goal))
  LL2_all <- log_loss(as.matrix(1*(test_data$is_save + test_data$is_goal)), as.matrix(test_data$prob_save + test_data$prob_goal))
  cat("LL1 (All Situations): ",
      LL1_all,
      " || LL2 (All Situations): ",
      LL2_all,
      "\n",
      sep = ""
      )
  
  LL1_5 <- log_loss(as.matrix(test_data_5$is_goal), as.matrix(test_data_5$prob_goal))
  LL2_5 <- log_loss(as.matrix(1*(test_data_5$is_save + test_data_5$is_goal)), as.matrix(test_data_5$prob_save + test_data_5$prob_goal))
  cat("LL1 (5-on-5): ",
      LL1_5,
      " || LL2 (5-on-5): ",
      LL2_5,
      "\n",
      sep = ""
      )
  
  LL1_nen <- log_loss(as.matrix(test_data_nen$is_goal), as.matrix(test_data_nen$prob_goal))
  LL2_nen <- log_loss(as.matrix(1*(test_data_nen$is_save + test_data_nen$is_goal)), as.matrix(test_data_nen$prob_save + test_data_nen$prob_goal))
  cat("LL1 (Non-EN): ",
      LL1_nen,
      " || LL2 (Non-EN): ",
      LL2_nen,
      "\n",
      sep = ""
      )
  
  test_data %>%
    summarise(goals = sum(is_goal),
              saves = sum(is_save),
              unblocked = n(),
              xgoals = sum(prob_goal),
              xsaves = sum(prob_save)
              ) %>%
    mutate(sh = goals/(goals + saves),
           fsh = goals/unblocked,
           og = (goals + saves)/unblocked,
           xsh = xgoals/(xgoals + xsaves),
           xfsh = xgoals/unblocked,
           xog = (xgoals + xsaves)/unblocked
           ) %>%
    data.frame() ->
    results
    
  data.frame(min_loss = eval_metrics[[1]],
             ll1_all = LL1_all,
             ll2_all = LL2_all,
             ll1_5 = LL1_5,
             ll2_5 = LL2_5,
             ll1_nen = LL1_nen,
             ll2_nen = LL2_nen,
             sh = results$sh,
             fsh = results$fsh,
             og = results$og,
             xsh = results$xsh,
             xfsh = results$xfsh,
             xog = results$xog
             )
  
} -> cv_mat

cv_mat %>%
  colMeans()
# Model parameters: (Hidden = 720/120, Activation: relu/relu, dropout: 0.5/0.0, LR: 0.005, Decay = 0.000001, Batch: 250, Patience: 20, Nesterov)
# LL1 (ALL) = 0.21299467 || LL2 (ALL) = 0.58018038 || LL1 (5v5) = 0.18897504 || LL2 (5v5) = 0.58006057 || LL1 (NEN) = 0.20640512 || LL2 (NEN) = 0.57933122


## Test Results
# Load model
load("~/Documents/github/corsica/models/xg_model.RData")

# Response variables
model_data$is_goal <- 1*(model_data$event_type == "GOAL")
model_data$is_save <- 1*(model_data$event_type == "SHOT")

# Feature list
vars <- c("event_distance",
          "event_angle",
          "seconds_since_last",
          "event_type_last",
          "same_team_last",
          "is_home_team",
          "is_EN",
          "shooter_strength_state",
          "shooter_score_adv",
          "distance_from_last"
          )

# Build model matrix
model_mat <- data.frame(outcome = as.factor(1*(model_data$is_save) + 2*(model_data$is_goal) + 1),
                        model.matrix(is_goal ~ 
                                     poly(event_distance, 3) + poly(event_angle, 3) + 
                                     event_type_last*same_team_last*(seconds_since_last + distance_from_last) + 
                                     is_home_team + is_EN + shooter_strength_state + shooter_score_adv,
                                     data = model_data[, c("is_goal", vars)]
                                     )
                        )

# Predict xG
predicted <- predict.cv.glmnet(xg_glm,
                               newx = as.matrix(model_mat[, -1]),
                               s = "lambda.min",
                               type = "response"
                               )
  
# Convert to data frame
ftable2df(predicted) %>%
  rename(val = `1`) %>%
  group_by(Var2) %>%
  summarise(prob_miss = sum(val*(Var1 == 1)),
            prob_save = sum(val*(Var1 == 2)),
            prob_goal = sum(val*(Var1 == 3))
            ) %>%
  data.frame() ->
  prob_df
  
# Append xG
model_data$prob_goal <- as.numeric(prob_df$prob_goal)
model_data$prob_save <- as.numeric(prob_df$prob_save)

# Test averages
model_data %>%
  summarise(goals = sum(event_type == "GOAL"),
            xgoals = sum(na.omit(prob_goal)),
            shots = sum(event_type %in% st.shot_events),
            xshots = sum(na.omit(prob_goal + prob_save)),
            unblocked = sum(event_type %in% st.fenwick_events)
            ) %>%
  mutate(sh. = goals/shots,
         xsh. = xgoals/xshots,
         fsh. = goals/unblocked,
         xfsh. = xgoals/unblocked
         ) %>%
  data.frame()

# xG bins
model_data %>%
  mutate(bin = cut(prob_goal, seq(0, 1, 0.05))) %>%
  group_by(bin) %>%
  summarise(goals = sum(event_type == "GOAL"),
            xgoals = sum(na.omit(prob_goal)),
            shots = sum(event_type %in% st.shot_events),
            xshots = sum(na.omit(prob_goal + prob_save)),
            unblocked = sum(event_type %in% st.fenwick_events)
            ) %>%
  mutate(sh. = goals/shots,
         xsh. = xgoals/xshots,
         fsh. = goals/unblocked,
         xfsh. = xgoals/unblocked
         ) %>%
  data.frame() ->
  bin_data

# Game results
model_data %>%
  group_by(game_id) %>%
  summarise(home_gf = sum(event_type == "GOAL" & event_team == home_team),
            away_gf = sum(event_type == "GOAL" & event_team == away_team),
            home_xgf = sum(na.omit(prob_goal*(event_team == home_team))),
            away_xgf = sum(na.omit(prob_goal*(event_team == away_team)))
            ) %>%
  data.frame() ->
  games

games %>%
  mutate(home_win = 1*(home_gf > away_gf),
         home_edge = cut(home_xgf - away_xgf, seq(-10, 10, 1))
         ) %>%
  group_by(home_edge) %>%
  summarise(win_pct = sum(home_win)/n(),
            games = n()
            )
