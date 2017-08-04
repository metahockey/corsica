### DEPRECATED FUNCTIONS ###
# Last edit: Manny (2017-07-01)


# Get PBP
ds.get_pbp <- function(year, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_pbp() imports the PBP page corresponding to a given year and game ID and returns a JSON list object
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
               as.character(year),
               "0",
               as.character(game_id),
               "/feed/live?site=en_nhl",
               sep = ""
               )
  
  raw_text <- NULL
  json_check <- NULL
  
  while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
    
    try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = FALSE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
    raw_text
    
    json_check <- try(fromJSON(raw_text), silent = TRUE)
    
    try_tolerance <- try_tolerance - 1
    
  }
  
  raw_json <- try(fromJSON(raw_text), silent = TRUE)
  
  if(class(raw_json) == "try-error") {raw_json <- NULL}
  
  return(raw_json)
  
}

# Get Shifts
ds.get_shifts <- function(year, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_shifts() imports the shift report page corresponding to a given year and game ID and returns a JSON list object
  
  url <- paste("http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=",
               as.character(year),
               "0",
               as.character(game_id),
               sep = ""
               )
  
  raw_text <- NULL
  json_check <- NULL
  
  while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
    
    try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = FALSE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
    raw_text
    
    json_check <- try(fromJSON(raw_text), silent = TRUE)
    
    try_tolerance <- try_tolerance - 1
    
  }
  
  raw_json <- try(fromJSON(raw_text), silent = TRUE)
  
  if(class(raw_json) == "try-error") {raw_json <- NULL}
  
  return(raw_json)
  
}

# Get Media
ds.get_media <- function(year, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_media() imports the media page corresponding to a given year and game ID and returns a JSON list object
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
               as.character(year),
               "0",
               as.character(game_id),
               "/content?site=en_nhl",
               sep = ""
               )
  
  raw_text <- NULL
  json_check <- NULL
  
  while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
    
    try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = FALSE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
    raw_text
    
    json_check <- try(fromJSON(raw_text), silent = TRUE)
    
    try_tolerance <- try_tolerance - 1
    
  }
  
  raw_json <- try(fromJSON(raw_text), silent = TRUE)
  
  if(class(raw_json) == "try-error") {raw_json <- NULL}
  
  return(raw_json)
  
}

# Parse PBP Event
ds.parse_event <- function(x) {
  
  ## Description
  # parse_event() parses a single event from the PBP JSON object and returns a data frame
  
  x$players %>%
    sapply(function(p) as.character(p$player$id)) %>%
    unlist() %>%
    c(rep(NA, 
          times = (4 - length(x$players))
          )
      ) ->
    player_ids
  
  data.frame(game_date = NA,
             game_id = NA,
             season = NA,
             session = NA,
             event_id = na_if_null(nabs(x$about$eventIdx)),
             event_code = na_if_null(as.character(x$result$eventCode)),
             event_type = na_if_null(as.character(x$result$eventTypeId)),
             event_description = na_if_null(as.character(x$result$description)),
             event_detail = na_if_null(as.character(x$result$secondaryType)),
             datetime = na_if_null(as.character(parse_date_time(x$about$dateTime, "y-m-d.H:M:S."))),
             game_period = na_if_null(nabs(x$about$period)),
             period_time_elapsed = na_if_null(as.character(x$about$periodTime)),
             period_time_remaining = na_if_null(as.character(x$about$periodTimeRemaining)),
             event_team = na_if_null(as.character(x$team$id)),
             event_player_1 = na_if_null(player_ids[1]),
             event_player_2 = na_if_null(player_ids[2]),
             event_player_3 = na_if_null(player_ids[3]),
             event_player_4 = na_if_null(player_ids[4]),
             coords_x = na_if_null(x$coordinates$x),
             coords_y = na_if_null(x$coordinates$y),
             highlight_id = na_if_null(nabs(x$about$eventId))
             ) ->
    event_df
  
  return(event_df)
  
}

# Parse Shift
ds.parse_shift <- function(x) {
  
  ## Description
  # parse_shift() parses a single shift from the Shifts JSON object and returns a data frame
  
  data.frame(game_date = NA,
             game_id = na_if_null(nabs(x$gameId)),
             season = NA,
             session = NA,
             shift_number = na_if_null(nabs(x$eventNumber)),
             shift_period = na_if_null(nabs(x$period)),
             shift_start = na_if_null(as.character(x$startTime)),
             shift_end = na_if_null(as.character(x$endTime)),
             shift_duration = na_if_null(as.character(x$duration)),
             team_id = na_if_null(as.character(x$teamId)),
             player_id = na_if_null(as.character(x$playerId))
             ) ->
    shift_df
  
  return(shift_df)
  
}

# Parse Media
ds.parse_media <- function(x) {
  
  ## Description
  # parse_media() parses a single highlight from the Media JSON object and returns a data frame
  
  data.frame(highlight_id = na_if_null(as.character(x$id)),
             highlight_title = na_if_null(as.character(x$title)),
             highlight_blurb = na_if_null(as.character(x$blurb)),
             highlight_description = na_if_null(as.character(x$description)),
             highlight_image_url = na_if_null(as.character(x$image$cuts$`1136x640`$src))
             ) ->
    media_df
  
  return(media_df)
  
}

# Parse Roster
ds.parse_roster <- function(x) {
  
  ## Description
  # parse_roster() parses a single player from the PBP JSON object and returns a data frame
  
  data.frame(player_id = na_if_null(x$id),
             player_name_first = na_if_null(x$firstName),
             player_name_last = na_if_null(x$lastName),
             player_name_full = na_if_null(x$fullName),
             player_jerseynum = na_if_null(x$primaryNumber),
             player_position = na_if_null(x$primaryPosition$code)
             ) ->
    roster_df
  
  return(roster_df)
  
}

# Parse Line Score
ds.parse_linescore <- function(x) {
  
  ## Description
  # parse_linescore() parses a single period from the PBP >> LineScore JSON object and returns a data frame
  
  data.frame(game_period = na_if_null(nabs(x$num)),
             home_side = na_if_null(toupper(substr(as.character(x$home$rinkSide), 0, 1))),
             away_side = na_if_null(toupper(substr(as.character(x$away$rinkSide), 0, 1)))
             ) ->
    linescore_df
  
  return(linescore_df)
  
}

# Is On
ds.is_on <- function(player_id, pbp, venue) {
  
  ## Description
  # is_on() returns a numeric vector indicating 1 if a given player is on ice during the event corresponding to the \
  # row index in the given PBP pbject
  
  if(venue == "Home") {
    
    data.frame(cumsum(1*(grepl(player_id, pbp$players_substituted) == TRUE & pbp$event_type == "ON" & pbp$event_team == pbp$home_team) - 
                      1*(grepl(player_id, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == pbp$home_team)
                      )
              ) ->
      is_on
    
  } else if(venue == "Away") {
    
    data.frame(cumsum(1*(grepl(player_id, pbp$players_substituted) == TRUE & pbp$event_type == "ON" & pbp$event_team == pbp$away_team) - 
                      1*(grepl(player_id, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == pbp$away_team)
                      )
              ) ->
      is_on
    
  }
  
  colnames(is_on) <- player_id
  
  return(is_on)
  
}

# Scrape
ds.scrape_game <- function(game_id, season, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape_game() collects and parses the data for a game corresponsing to a given season and game ID
  # A list object containing c([[1]] = PBP, [[2]] = Shifts, [[3]] = Highlights, [[4]] = Media) is returned
  
  season_ <- as.character(season); game_id_ <- as.character(game_id)
  
  year <- substr(season_, 0, 4)
  
  pbp <- ds.get_pbp(year, game_id_, try_tolerance, agents)
  shifts <- ds.get_shifts(year, game_id_, try_tolerance, agents)
  media <- ds.get_media(year, game_id_, try_tolerance, agents)
  highlights <- ds.get_highlights(season_, game_id_, try_tolerance, agents)
  
  pbp_df <- dcapply(pbp$liveData$plays$allPlays, 
                    ds.parse_event, 
                    "rbind", 
                    cores = 1
                    )
  
  roster_df <- dcapply(pbp$gameData$players,
                       ds.parse_roster,
                       "rbind",
                       cores = 1
                       )

  shift_df <- dcapply(shifts$data,
                      ds.parse_shift,
                      "rbind",
                      cores = 1
                      )
  
  highlight_df <- dcapply(highlights$video$events,
                          ds.parse_highlight,
                          "rbind",
                          cores = 1
                          )
  
  media_df <- dcapply(media$highlights$gameCenter$items,
                      ds.parse_media,
                      "rbind",
                      cores = 1
                      )
  
  rinkside_df <- dcapply(pbp$liveData$linescore$periods,
                         ds.parse_linescore,
                         "rbind",
                         cores = 1
                         )
  
  game_date_ <- as.character(as.Date(na_if_null(pbp$gameData$datetime$dateTime)))
  session_ <- na_if_null(as.character(pbp$gameData$game$type))
  game_id_unique <- na_if_null(nabs(pbp$gameData$game$pk))
  game_venue_ <- na_if_null(as.character(pbp$gameData$venue$name))
  home_team_ <- na_if_null(nabs(pbp$gameData$teams$home$id))
  away_team_ <- na_if_null(nabs(pbp$gameData$teams$away$id))
  
  winning_goalie_ <- na_if_null(nabs(pbp$liveData$decisions$winner$id))
  first_star_ <- na_if_null(nabs(pbp$liveData$decisions$firstStar$id))
  second_star_ <- na_if_null(nabs(pbp$liveData$decisions$secondStar$id))
  third_star_ <- na_if_null(nabs(pbp$liveData$decisions$thirdStar$id))
  
  if(sum(is.na(rinkside_df)) > 0) {
    
    pbp_df %>%
      group_by(game_period) %>%
      summarise(home_right = sum(event_type %in% st.fenwick_events & coords_x > 25 & event_team == home_team_),
                home_left = sum(event_type %in% st.fenwick_events & coords_x < -25 & event_team == home_team_)
                ) %>%
      mutate(home_side = 1*(home_right > home_left) + 2*(home_left > home_right),
             away_side = 3 - nabs(home_side)
             ) %>%
      select(game_period, home_side, away_side) %>%
      data.frame() ->
      rinkside_df
    
    rinkside_df$home_side[which(rinkside_df$home_side == 1)] <- "L"
    rinkside_df$home_side[which(rinkside_df$home_side == 2)] <- "R"
    rinkside_df$away_side[which(rinkside_df$away_side == 1)] <- "L"
    rinkside_df$away_side[which(rinkside_df$away_side == 2)] <- "R"
    
  }
  
  if(!is.null(pbp_df)) {
    
    pbp_df %>%
      mutate(game_date = game_date_,
             game_id = game_id_unique,
             season = as.character(season_),
             session = session_,
             home_team = home_team_,
             away_team = away_team_,
             game_venue = game_venue_,
             home_rinkside = rinkside_df$home_side[match(game_period, rinkside_df$game_period)],
             away_rinkside = rinkside_df$away_side[match(game_period, rinkside_df$game_period)]
             ) %>%
      data.frame() ->
      pbp_df
  
  }

  if(!is.null(shift_df)) {
  
    shift_df %>%
      mutate(game_date = game_date_,
             season = as.character(season_),
             session = session_,
             home_team = home_team_,
             away_team = away_team_,
             game_venue = game_venue_
             ) %>%
      data.frame() ->
      shift_df
    
  }
  
  if(!is.null(highlight_df) & !is.null(media_df)) {
  
    highlight_df %>%
      mutate(game_date = game_date_,
             game_id = game_id_unique,
             season = as.character(season_),
             session = session_,
             home_team = home_team_,
             away_team = away_team_,
             game_venue = game_venue_
             ) %>%
      data.frame() ->
      highlight_df
    
    full_highlight <- merge(highlight_df,
                            media_df,
                            by.x = "highlight_id",
                            by.y =  "highlight_id"
                            )
  
  } else {
    
    full_highlight <- NULL
    
  }
    
  if(length(media$editorial$preview$items) > 0) {
  
    media_preview_headline <- media$editorial$preview$items[[1]]$headline
    media_preview_subhead <- media$editorial$preview$items[[1]]$subhead
    media_preview_description <- media$editorial$preview$items[[1]]$seoDescription
    
  } else {
    
    media_preview_headline <- NULL
    media_preview_subhead <- NULL
    media_preview_description <- NULL
    
  }
  
  if(length(media$editorial$recap$items) > 0) {
    
    media_recap_headline <- na_if_null(media$editorial$recap$items[[1]]$headline)
    media_recap_subhead <- na_if_null(media$editorial$recap$items[[1]]$subhead)
    media_recap_description <- na_if_null(media$editorial$recap$items[[1]]$seoDescription)
    
  } else {
    
    media_recap_headline <- NA
    media_recap_subhead <- NA
    media_recap_description <- NA
    
  }
  
  full_media <- data.frame(game_date = game_date_,
                           game_id = game_id_unique,
                           season = as.character(season_),
                           session = session_,
                           home_team = home_team_,
                           away_team = away_team_,
                           preview_headline = na_if_null(media_preview_headline),
                           preview_subhead = na_if_null(media_preview_subhead),
                           preview_description = na_if_null(media_preview_description),
                           recap_headline = na_if_null(media_recap_headline),
                           recap_subhead = na_if_null(media_recap_subhead),
                           recap_description = na_if_null(media_recap_description),
                           winning_goalie = winning_goalie_,
                           first_star = first_star_,
                           second_star = second_star_,
                           third_star = third_star_
                           )
  
  game_list <- list(pbp_df,
                    shift_df,
                    roster_df,
                    full_highlight,
                    full_media
                    )
  
  return(game_list)
  
}

# Compile Games
ds.compile_games <- function(games, season, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # compile_games() collects, parses and compiles all game data corresponding to a given vector of game IDs and season
  # A list object containing c([[1]] = PBP, [[2]] = Shifts, [[3]] = Highlights, [[4]] = Media) is returned
  
  foreach(g = as.character(games)) %do% {
    
    cat(g,
        "...",
        "\n",
        sep = ""
        )
    
    ds.scrape_game(g, season, try_tolerance, agents)
    
  } -> nested_games
  
  unpacked <- do.call(Map, c(rbind, nested_games))
  
  pbp <- unpacked[[1]]
  shifts <- unpacked[[2]]
  roster <- unpacked[[3]]
  highlights <- unpacked[[4]]
  media <- unpacked[[5]]
  
  pbp$game_seconds = 1200*(nabs(pbp$game_period) - 1) + ds.seconds_from_ms(pbp$period_time_elapsed)
  
  shifts$start_seconds = 1200*(nabs(shifts$shift_period) - 1) + ds.seconds_from_ms(shifts$shift_start)
  shifts$end_seconds = 1200*(nabs(shifts$shift_period) - 1) + ds.seconds_from_ms(shifts$shift_end)
  
  shifts$end_seconds[which(shifts$end_seconds < shifts$start_seconds)] <- shifts$start_seconds[which(shifts$end_seconds < shifts$start_seconds)] + ds.seconds_from_ms(shifts$shift_duration[which(shifts$end_seconds < shifts$start_seconds)])
  
  bind_rows(
    shifts %>%
      filter(!is.na(shift_duration)) %>%
      group_by(game_id,
               game_date,
               season,
               session,
               home_team,
               away_team,
               game_venue,
               team_id,
               shift_number,
               start_seconds
               ) %>%
      rename(game_seconds = start_seconds, event_team = team_id) %>%
      summarise(event_type = "ON",
                game_period = nabs(first(shift_period)),
                players_substituted = paste(unique(player_id), collapse = ", ")
                ) %>%
      data.frame(),
   
     shifts %>%
       filter(!is.na(shift_duration)) %>%
       group_by(game_id,
                game_date,
                season,
                session,
                home_team,
                away_team,
                game_venue,
                team_id,
                shift_number,
                end_seconds
                ) %>%
       rename(game_seconds = end_seconds, event_team = team_id) %>%
       summarise(event_type = "OFF",
                 game_period = nabs(first(shift_period)),
                 players_substituted = paste(unique(player_id), collapse = ", ")
                 ) %>%
       data.frame()
  ) -> shift_summary
  
  if(!is.null(highlights)) {
    
    merge(pbp,
          highlights %>%
            rename(highlight_code = highlight_id) %>%
            select(game_id, event_id, highlight_code, highlight_title:highlight_image_url) %>%
            group_by(event_id) %>%
            slice(1) %>%
            data.frame(),
          by.x = c("game_id", "highlight_id"),
          by.y = c("game_id", "event_id"),
          all.x = TRUE
          ) %>%
      data.frame() ->
      new_pbp
    
  } else {
    
    pbp %>%
      mutate(highlight_code = NA,
             highlight_title = NA,
             highlight_blurb = NA,
             highlight_description = NA,
             highlight_image_url = NA
             ) %>%
      data.frame() ->
      new_pbp
    
  }
  
  bind_rows(new_pbp,
            shift_summary
            ) %>%
    mutate(priority = 1*(event_type %in% c("TAKEAWAY", "GIVEAWAY", "MISSED_SHOT", "HIT", "SHOT", "BLOCKED_SHOT")) +
                      2*(event_type == "GOAL") +
                      3*(event_type == "STOP") +
                      4*(event_type == "PENALTY") +
                      5*(event_type == "OFF") +
                      6*(event_type == "ON") +
                      7*(event_type == "FACEOFF")
           ) %>% 
    group_by(game_id) %>%
    arrange(game_period,
            game_seconds,
            priority
            ) %>%
    mutate(event_index = cumsum(!is.na(game_id))) %>%
    data.frame() ->
    new_pbp
  
  home_on_mat <- dcapply(as.list(unique(shifts$player_id)),
                         ds.is_on,
                         "cbind",
                         cores = 1,
                         pbp = arrange(new_pbp,
                                       game_id,
                                       event_index
                                       ),
                         venue = "Home"
                         )
  
  away_on_mat <- dcapply(as.list(unique(shifts$player_id)),
                         ds.is_on,
                         "cbind",
                         cores = 1,
                         pbp = arrange(new_pbp,
                                       game_id,
                                       event_index
                                       ),
                         venue = "Away"
                         )
  
  which(home_on_mat == 1, 
        arr.ind = TRUE
        ) %>%
    data.frame() %>%
    group_by(row) %>%
    summarise(home_on_1 = colnames(home_on_mat)[unique(col)[1]],
              home_on_2 = colnames(home_on_mat)[unique(col)[2]],
              home_on_3 = colnames(home_on_mat)[unique(col)[3]],
              home_on_4 = colnames(home_on_mat)[unique(col)[4]],
              home_on_5 = colnames(home_on_mat)[unique(col)[5]],
              home_on_6 = colnames(home_on_mat)[unique(col)[6]]
              ) %>%
    data.frame() ->
    home_on_df
  
  which(away_on_mat == 1, 
        arr.ind = TRUE
        ) %>%
    data.frame() %>%
    group_by(row) %>%
    summarise(away_on_1 = colnames(away_on_mat)[unique(col)[1]],
              away_on_2 = colnames(away_on_mat)[unique(col)[2]],
              away_on_3 = colnames(away_on_mat)[unique(col)[3]],
              away_on_4 = colnames(away_on_mat)[unique(col)[4]],
              away_on_5 = colnames(away_on_mat)[unique(col)[5]],
              away_on_6 = colnames(away_on_mat)[unique(col)[6]]
              ) %>%
    data.frame() ->
    away_on_df
  
  home_on_df %>%
    mutate_each(funs(na_as_zero)) %>%
    transmute(home_goalie = nabs(home_on_1)*(home_on_1 %in% roster$player_id[which(roster$player_position == "G")]) +
                            nabs(home_on_2)*(home_on_2 %in% roster$player_id[which(roster$player_position == "G")]) +
                            nabs(home_on_3)*(home_on_3 %in% roster$player_id[which(roster$player_position == "G")]) +
                            nabs(home_on_4)*(home_on_4 %in% roster$player_id[which(roster$player_position == "G")]) +
                            nabs(home_on_5)*(home_on_5 %in% roster$player_id[which(roster$player_position == "G")]) +
                            nabs(home_on_6)*(home_on_6 %in% roster$player_id[which(roster$player_position == "G")])
                            ) ->
    home_goalie
  
  away_on_df %>%
    mutate_each(funs(na_as_zero)) %>%
    transmute(away_goalie = nabs(away_on_1)*(away_on_1 %in% roster$player_id[which(roster$player_position == "G")]) +
                            nabs(away_on_2)*(away_on_2 %in% roster$player_id[which(roster$player_position == "G")]) +
                            nabs(away_on_3)*(away_on_3 %in% roster$player_id[which(roster$player_position == "G")]) +
                            nabs(away_on_4)*(away_on_4 %in% roster$player_id[which(roster$player_position == "G")]) +
                            nabs(away_on_5)*(away_on_5 %in% roster$player_id[which(roster$player_position == "G")]) +
                            nabs(away_on_6)*(away_on_6 %in% roster$player_id[which(roster$player_position == "G")])
                            ) ->
    away_goalie
  
  new_pbp %>%
    arrange(game_id,
            event_index
            ) %>%
    mutate(home_on_1 = NA,
           home_on_2 = NA,
           home_on_3 = NA,
           home_on_4 = NA,
           home_on_5 = NA,
           home_on_6 = NA,
           home_goalie = NA,
           away_on_1 = NA,
           away_on_2 = NA,
           away_on_3 = NA,
           away_on_4 = NA,
           away_on_5 = NA,
           away_on_6 = NA,
           away_goalie = NA
           ) ->
    full_pbp
  
  full_pbp$home_on_1[home_on_df$row] <- home_on_df$home_on_1
  full_pbp$home_on_2[home_on_df$row] <- home_on_df$home_on_2
  full_pbp$home_on_3[home_on_df$row] <- home_on_df$home_on_3
  full_pbp$home_on_4[home_on_df$row] <- home_on_df$home_on_4
  full_pbp$home_on_5[home_on_df$row] <- home_on_df$home_on_5
  full_pbp$home_on_6[home_on_df$row] <- home_on_df$home_on_6
  full_pbp$home_goalie[home_on_df$row] <- home_goalie$home_goalie
  
  full_pbp$away_on_1[away_on_df$row] <- away_on_df$away_on_1
  full_pbp$away_on_2[away_on_df$row] <- away_on_df$away_on_2
  full_pbp$away_on_3[away_on_df$row] <- away_on_df$away_on_3
  full_pbp$away_on_4[away_on_df$row] <- away_on_df$away_on_4
  full_pbp$away_on_5[away_on_df$row] <- away_on_df$away_on_5
  full_pbp$away_on_6[away_on_df$row] <- away_on_df$away_on_6
  full_pbp$away_goalie[away_on_df$row] <- away_goalie$away_goalie
  
  full_pbp %>%
    group_by(game_id) %>%
    arrange(event_index) %>%
    mutate(home_skaters = 6 - 1*(is.na(home_on_1) == TRUE) -
                              1*(is.na(home_on_2) == TRUE) -
                              1*(is.na(home_on_3) == TRUE) -
                              1*(is.na(home_on_4) == TRUE) -
                              1*(is.na(home_on_5) == TRUE) -
                              1*(is.na(home_on_6) == TRUE) -
                              1*(!is.na(home_goalie)),
           away_skaters = 6 - 1*(is.na(away_on_1) == TRUE) -
                              1*(is.na(away_on_2) == TRUE) -
                              1*(is.na(away_on_3) == TRUE) -
                              1*(is.na(away_on_4) == TRUE) -
                              1*(is.na(away_on_5) == TRUE) -
                              1*(is.na(away_on_6) == TRUE) -
                              1*(!is.na(away_goalie)),
           home_score = cumsum(event_type == "GOAL" & event_team == home_team) - 1*(event_type == "GOAL" & event_team == home_team),
           away_score = cumsum(event_type == "GOAL" & event_team == away_team) - 1*(event_type == "GOAL" & event_team == away_team),
           event_length = nabs(lead(game_seconds, 1) - game_seconds)
           ) %>%
    ungroup() %>%
    mutate(game_strength_state = paste(ifelse(is.na(home_goalie) == TRUE | home_goalie == 0,
                                              "E",
                                              home_skaters
                                              ),
                                       ifelse(is.na(away_goalie) == TRUE | away_goalie == 0,
                                              "E",
                                              away_skaters
                                              ),
                                       sep = "v"
                                       ),
           game_score_state = paste(home_score,
                                    away_score,
                                    sep = "v"
                                    )
           ) %>%
    select(one_of(ds.pbp_colnames)) %>%
    arrange(game_id,
            event_index
            ) %>%
    data.frame() ->
    full_pbp
  
  new_game_list <- list(full_pbp,
                        shifts,
                        highlights,
                        media
                        )
  
  return(new_game_list)
  
}

# Old Scrape
ds.old_scrape <- function(games, season, names = TRUE, pause) {
  
  pbp.list <- NULL
  roster.list <- NULL
  
  agents <- c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
              "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36",
              "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.5.2171.95 Safari/537.36",
              "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36")
  
  for (i in 1:length(games)) {
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE PLAY-BY-PLAY / ACQUÉRIR RÉSUMÉ DU MATCH ##############################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URL / Definir URL
    ID <- as.character(games[i])
    cat(ID)
    url <- paste("http://www.nhl.com/scores/htmlreports/", season, "/PL0", ID, ".HTM", sep = "")
    
    url.text <- try(getURL(url, header = FALSE,
                           .opts = curlOptions(
                             referer = 'nhl.com',
                             verbose = TRUE,
                             header = TRUE,
                             followLocation = TRUE,
                             useragent = agents[sample(1:4, 1)]))
    )
    
    if(class(url.text) == "try-error") {
      url.text <- getURL(url, header = FALSE,
                         .opts = curlOptions(
                           referer = 'nhl.com',
                           verbose = TRUE,
                           header = TRUE,
                           followLocation = TRUE,
                           useragent = agents[sample(1:4, 1)]))
    }
    
    # Create HTML object / Créer objet HTML
    html <- read_html(url.text)
    
    # Scrape text / Acquérir texte 
    all <- html_nodes(html, "td")
    body <- html_nodes(html, ".bborder")
    full.text <- html_text(all)
    body.text <- html_text(body)
    
    # Skip game if file is broken / Proceder au prochain match si le fichier est incomplet 
    if (length(full.text) < 500) {next}
    
    pbp.raw <- matrix(body.text, byrow = TRUE, ncol = 8) %>% data.frame() %>% filter(X2 != "Per")
    
    # Team list / Liste d'équipes 
    teamlist <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
                  "CGY", "CHI", "COL", "DAL", "DET", "EDM",
                  "FLA", "L.A", "MIN", "MTL", "N.J", "NSH",
                  "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
                  "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
                  "PHX", "ATL")
    
    # Get teams / Obtenir équipes
    hometeam <- gsub(" On Ice", "", body.text[8])
    awayteam <- gsub(" On Ice", "", body.text[7])
    
    hometeam[which(hometeam == "PHX")] <- "ARI"; awayteam[which(awayteam == "PHX")] <- "ARI"
    
    teams <- c(awayteam, hometeam)
    
    # Date, game and etc. data / Date, match, etc.
    date <- gsub("^[a-zA-Z]*, ", "", full.text[grep("^[a-zA-Z]*, ", full.text)]) %>% as.Date(format = "%B %d, %Y") %>% first() %>% as.character()
    Date <- rep(date, time = length(pbp.raw$X1))
    
    Game.ID <- rep(ID, times = length(pbp.raw$X1))
    
    Home.Team <- rep(hometeam, times = length(pbp.raw$X1))
    Away.Team <- rep(awayteam, times = length(pbp.raw$X1))
    
    Duration <- rep(NA, times = length(pbp.raw$X1))
    
    # Parse time / Traiter temps
    timemat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(pbp.raw$X4), ":"))), byrow = TRUE, ncol = 3))
    
    Seconds <- 1200*(as.numeric(pbp.raw$X2) - 1) + timemat$X1*60 + (timemat$X3 > 0)*(60 - timemat$X3)
    Seconds[which(as.numeric(pbp.raw$X2) == 5)] <- 3900.001
    
    ## Parse on-ice / Traiter joueurs sur glace
    stretch <- function(x) {
      t <- as.character(unlist(x))
      t2 <- list(c(t, rep(c(0, NA), times = (12 - (length(t)/2)))))
      return(t2)
    }
    
    # Away / étrangère 
    a.match <- regmatches(as.character(pbp.raw$X7), gregexpr("[0-9|A-Z]+", as.character(pbp.raw$X7)))
    a.new <- lapply(a.match, stretch)
    Away.On <- data.frame(matrix(unlist(a.new), byrow = TRUE, ncol = 24))
    colnames(Away.On) <- c("a1.num", "a1.pos", "a2.num", "a2.pos", "a3.num", "a3.pos", "a4.num", "a4.pos", "a5.num", "a5.pos", "a6.num", "a6.pos",
                           "a7.num", "a7.pos", "a8.num", "a8.pos", "a9.num", "a9.pos", "a10.num", "a10.pos", "a11.num", "a11.pos", "a12.num", "a12.pos")
    
    # Home / Domicile 
    h.match <- regmatches(as.character(pbp.raw$X8), gregexpr("[0-9|A-Z]+", as.character(pbp.raw$X8)))
    h.new <- lapply(h.match, stretch)
    Home.On <- data.frame(matrix(unlist(h.new), byrow = TRUE, ncol = 24))
    colnames(Home.On) <- c("h1.num", "h1.pos", "h2.num", "h2.pos", "h3.num", "h3.pos", "h4.num", "h4.pos", "h5.num", "h5.pos", "h6.num", "h6.pos", 
                           "h7.num", "h7.pos", "h8.num", "h8.pos", "h9.num", "h9.pos", "h10.num", "h10.pos","h11.num", "h11.pos", "h12.num", "h12.pos")
    
    ## Parse description / Traiter déscription 
    clean.nums <- function(x) {
      t <- gsub("#|ONGOAL - ", "", as.character(unlist(x)))
      t2 <- list(c(t, rep(NA, times = (3 - length(t)))))
      return(t2)
    }
    
    dummy.team <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- x
      } else {
        t <- NA
      }
      return(t)
    }
    
    dummy.zone <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- x
      } else {
        t <- NA
      }
      return(t)
    }
    
    dummy.detail <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- paste(unlist(x), collapse = "")
      } else {
        t <- NA
      }
      return(t)
    }
    
    # Event team / équipe du jeu
    t.match <- regmatches(as.character(pbp.raw$X6), gregexpr(paste("(^", paste(teamlist, collapse = "|^"), ")", sep = ""), as.character(pbp.raw$X6)))
    t.new <- lapply(t.match, dummy.team)
    ev.team <- gsub(" ", "", as.character(unlist(t.new)))
    ev.team[which(ev.team == "PHX")] <- "ARI"
    
    # Event players / Joueurs du jeu
    d.match <- regmatches(as.character(pbp.raw$X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(pbp.raw$X6)))
    d.new <- lapply(d.match, clean.nums)
    ev.players <- data.frame(matrix(unlist(d.new), byrow = TRUE, ncol = 3))
    colnames(ev.players) <- c("p1", "p2", "p3")
    
    # Event zone / Zone du jeu
    z.match <- regmatches(as.character(pbp.raw$X6), gregexpr("[a-zA-Z]{3}. [zZ]one", as.character(pbp.raw$X6)))
    z.new <- lapply(z.match, dummy.zone)
    ev.zone <- gsub(". [zZ]one", "", as.character(unlist(z.new)))
    
    # Event details / Détails du jeu
    e.match <- regmatches(as.character(pbp.raw$X6), gregexpr(", [a-zA-Z|-]+,|[A-Z] .+[(].{4,}[)],|[A-Z] .+[(][a-zA-Z]{3,}[)],", as.character(pbp.raw$X6)))
    e.new <- lapply(e.match, dummy.detail)
    Detail <- gsub(",|, |[A-Z]+ |#[0-9]+ |[A-Z]{2,}.", "", as.character(unlist(e.new)))
    
    # On-ice goalies / Gardiens sur glace
    Home.Goalie <- (Home.On$h12.pos == "G" & !is.na(Home.On$h12.pos))*as.numeric(as.character(Home.On$h12.num)) + (Home.On$h11.pos == "G" & !is.na(Home.On$h11.pos))*as.numeric(as.character(Home.On$h11.num)) +
      (Home.On$h10.pos == "G" & !is.na(Home.On$h10.pos))*as.numeric(as.character(Home.On$h10.num)) + (Home.On$h9.pos == "G" & !is.na(Home.On$h9.pos))*as.numeric(as.character(Home.On$h9.num)) +
      (Home.On$h8.pos == "G" & !is.na(Home.On$h8.pos))*as.numeric(as.character(Home.On$h8.num)) + (Home.On$h7.pos == "G" & !is.na(Home.On$h7.pos))*as.numeric(as.character(Home.On$h7.num)) +
      (Home.On$h6.pos == "G" & !is.na(Home.On$h6.pos))*as.numeric(as.character(Home.On$h6.num)) + (Home.On$h5.pos == "G" & !is.na(Home.On$h5.pos))*as.numeric(as.character(Home.On$h5.num)) +
      (Home.On$h4.pos == "G" & !is.na(Home.On$h4.pos))*as.numeric(as.character(Home.On$h4.num)) + (Home.On$h3.pos == "G" & !is.na(Home.On$h3.pos))*as.numeric(as.character(Home.On$h3.num)) +
      (Home.On$h2.pos == "G" & !is.na(Home.On$h2.pos))*as.numeric(as.character(Home.On$h2.num)) + (Home.On$h1.pos == "G" & !is.na(Home.On$h1.pos))*as.numeric(as.character(Home.On$h1.num))
    
    Away.Goalie <- (Away.On$a12.pos == "G" & !is.na(Away.On$a12.pos))*as.numeric(as.character(Away.On$a12.num)) + (Away.On$a11.pos == "G" & !is.na(Away.On$a11.pos))*as.numeric(as.character(Away.On$a11.num)) +
      (Away.On$a10.pos == "G" & !is.na(Away.On$a10.pos))*as.numeric(as.character(Away.On$a10.num)) + (Away.On$a9.pos == "G" & !is.na(Away.On$a9.pos))*as.numeric(as.character(Away.On$a9.num)) +
      (Away.On$a8.pos == "G" & !is.na(Away.On$a8.pos))*as.numeric(as.character(Away.On$a8.num)) + (Away.On$a7.pos == "G" & !is.na(Away.On$a7.pos))*as.numeric(as.character(Away.On$a7.num)) +
      (Away.On$a6.pos == "G" & !is.na(Away.On$a6.pos))*as.numeric(as.character(Away.On$a6.num)) + (Away.On$a5.pos == "G" & !is.na(Away.On$a5.pos))*as.numeric(as.character(Away.On$a5.num)) +
      (Away.On$a4.pos == "G" & !is.na(Away.On$a4.pos))*as.numeric(as.character(Away.On$a4.num)) + (Away.On$a3.pos == "G" & !is.na(Away.On$a3.pos))*as.numeric(as.character(Away.On$a3.num)) +
      (Away.On$a2.pos == "G" & !is.na(Away.On$a2.pos))*as.numeric(as.character(Away.On$a2.num)) + (Away.On$a1.pos == "G" & !is.na(Away.On$a1.pos))*as.numeric(as.character(Away.On$a1.num))
    
    # Create PBP / Créer résumé
    pbp.new <- pbp.raw %>% select(-c(X1, X3, X4, X7, X8)) %>% cbind(Duration, Date, Game.ID, ev.team, ev.players, ev.zone, Detail, Seconds, Away.On[, 1:12], Home.On[, 1:12], Away.Team, Home.Team, Away.Goalie, Home.Goalie)
    
    ## Replace with teamnum ID / Remplacer avec code équipenum
    pbp.new <- rbind_list(
      filter(pbp.new, X5 == "FAC") %>% 
        mutate(p1 = paste(awayteam, p1, sep = ""), p2 = paste(hometeam, p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "HIT") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), p2 = paste(teams[which(teams != first(ev.team))], p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "SHOT") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "GIVE") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "MISS") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "GOAL") %>% 
        mutate(p1 = paste(ev.team, p1, sep = ""), 
               p2 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(ev.team, p2, sep = "")),
               p3 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(ev.team, p3, sep = ""))) %>% data.frame(),
      filter(pbp.new, X5 == "BLOCK") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), p2 = paste(teams[which(teams != first(ev.team))], p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "PENL") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), 
               p2 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(teams[which(teams != first(ev.team))], p2, sep = ""))) %>% data.frame(),
      filter(pbp.new, X5 == "TAKE") %>% 
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 %in% c("FAC", "HIT", "SHOT", "GIVE", "MISS", "GOAL", "BLOCK", "PENL", "TAKE") == FALSE) %>% data.frame()
    ) %>% data.frame() %>% 
      mutate(a1.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a1.num, sep = "")),
             a2.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a2.num, sep = "")),
             a3.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a3.num, sep = "")),
             a4.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a4.num, sep = "")),
             a5.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a5.num, sep = "")),
             a6.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a6.num, sep = "")),
             h1.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h1.num, sep = "")),
             h2.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h2.num, sep = "")),
             h3.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h3.num, sep = "")),
             h4.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h4.num, sep = "")),
             h5.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h5.num, sep = "")),
             h6.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h6.num, sep = "")),
             Home.Goalie = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, Home.Goalie, sep = "")),
             Away.Goalie = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, Away.Goalie, sep = "")),
             Home.Skaters = 6 - (is.na(h1.num)) - (is.na(h2.num)) - (is.na(h3.num)) - (is.na(h4.num)) - (is.na(h5.num)) - (is.na(h6.num)) - (!is.na(Home.Goalie)),
             Away.Skaters = 6 - (is.na(a1.num)) - (is.na(a2.num)) - (is.na(a3.num)) - (is.na(a4.num)) - (is.na(a5.num)) - (is.na(a6.num)) - (!is.na(Away.Goalie)),
             Seconds = Seconds - 0.01*(X5 %in% c("STOP", "PENL", "GOAL", "PEND")) + 0.01*(X5 == "FAC")) %>%
      rename(Period = X2, Event = X5, Description = X6) %>% arrange(Seconds) %>%
      mutate(Home.Score = cumsum(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Home.Team)) - 1*(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Home.Team)),
             Away.Score = cumsum(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Away.Team)) - 1*(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Away.Team))) %>%
      data.frame()
    
    # Re-assign event zone for blocked shots to perspective of shooting team / Re-attribuer zone du jeu pour tirs bloqués au point de vue de l'équipe tireur
    pbp.new$ev.zone[which(pbp.new$Event == "BLOCK" & pbp.new$ev.zone == "Def")] <- "Off"
    
    # Append strength and score states / Attacher états de forces et de score
    pbp.new$Strength.State <- paste(pbp.new$Home.Skaters, pbp.new$Away.Skaters, sep = "v"); pbp.new$Score.State <- paste(pbp.new$Home.Score, pbp.new$Away.Score, sep = "-")
    pbp.new$Score.Cat <- 1*(pbp.new$Home.Score - pbp.new$Away.Score == 1) + 2*(pbp.new$Home.Score - pbp.new$Away.Score == 2) + 3*(pbp.new$Home.Score - pbp.new$Away.Score >= 3) -
      1*(pbp.new$Home.Score - pbp.new$Away.Score == -1) - 2*(pbp.new$Home.Score - pbp.new$Away.Score == -2) - 3*(pbp.new$Home.Score - pbp.new$Away.Score <= -3)
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE SHIFT REPORTS / ACQUÉRIR RAPPORTS DE PRÉSENCES #######################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URLs / Définir URLs
    url1 <- paste("http://www.nhl.com/scores/htmlreports/", season, "/TH0", ID, ".HTM", sep = "") # Home / Domicile
    url2 <- paste("http://www.nhl.com/scores/htmlreports/", season, "/TV0", ID, ".HTM", sep = "") # Away / Étrangère
    
    url1.text <- try(getURL(url1, header = FALSE,
                            .opts = curlOptions(
                              referer = 'nhl.com',
                              verbose = TRUE,
                              header = TRUE,
                              followLocation = TRUE,
                              useragent = agents[sample(1:4, 1)]))
    )
    
    url2.text <- try(getURL(url2, header = FALSE,
                            .opts = curlOptions(
                              referer = 'nhl.com',
                              verbose = TRUE,
                              header = TRUE,
                              followLocation = TRUE,
                              useragent = agents[sample(1:4, 1)]))
    )
    
    if(class(url1.text) == "try-error" | class(url2.text) == "try-error") {
      url1.text <- getURL(url1, header = FALSE,
                          .opts = curlOptions(
                            referer = 'nhl.com',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
      
      url2.text <- getURL(url2, header = FALSE,
                          .opts = curlOptions(
                            referer = 'nhl.com',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
    }
    
    # Create HTML objects / Créer objets HTML
    html1 <- read_html(url1.text) # Home / Domicile
    html2 <- read_html(url2.text) # Away / Étrangère
    
    # Scrape tables / Acquérir tables
    home.text.1 <- html_nodes(html1, ".border")
    away.text.1 <- html_nodes(html2, ".border")
    home.text.2 <- html_nodes(html1, ".bborder")
    away.text.2 <- html_nodes(html2, ".bborder")
    
    home.outer <- html_text(home.text.1)
    away.outer <- html_text(away.text.1)
    home.inner <- html_text(home.text.2)
    away.inner <- html_text(away.text.2)
    
    # Skip game if file is broken / Proceder au prochain match si le fichier est incomplet 
    if (length(home.inner) < 1 | length(away.inner) < 1) {next}
    
    hometeam.full <- home.outer[1]
    home.players <- home.outer[-1]
    home.players <- home.players[which(grepl("^[0-9]+", home.players) == TRUE)] # FIX FOR 20132014-20934 / SOLUTION POUR 20132014-20934
    awayteam.full <- away.outer[1]
    away.players <- away.outer[-1]
    away.players <- away.players[which(grepl("^[0-9]+", away.players) == TRUE)] # FIX FOR 20132014-20934 / SOLUTION POUR 20132014-20934
    
    # Create roster table / Créer table de formation
    roster <- rbind_list(cbind(rep(hometeam, times = length(home.players)), home.players) %>% data.frame() %>% rename(Num.Last.First = home.players),
                         cbind(rep(awayteam, times = length(away.players)), away.players) %>% data.frame() %>% rename(Num.Last.First = away.players)) %>%
      data.frame()
    
    namemat <- data.frame(matrix(as.character(unlist(strsplit(gsub("^[0-9]+ ", "", roster$Num.Last.First), ", "))), byrow = T, ncol = 2))
    
    roster$Game.ID <- rep(ID, times = length(roster$Num.Last.First))
    roster$Date <- rep(date, times = length(roster$Num.Last.First))
    roster$Number <- unlist(regmatches(as.character(roster$Num.Last.First), gregexpr("^[0-9]+", as.character(roster$Num.Last.First))))
    roster$Last.Name <- namemat$X1
    roster$First.Name <- namemat$X2
    
    posmatch <- rbind_list(group_by(pbp.new, a1.num) %>% rename(player = a1.num) %>% 
                             summarise(C = sum(a1.pos == "C"), L = sum(a1.pos == "L"), R = sum(a1.pos == "R"), D = sum(a1.pos == "D"), G = sum(a1.pos == "G"), N = sum(a1.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a2.num) %>% rename(player = a2.num) %>% 
                             summarise(C = sum(a2.pos == "C"), L = sum(a2.pos == "L"), R = sum(a2.pos == "R"), D = sum(a2.pos == "D"), G = sum(a2.pos == "G"), N = sum(a2.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a3.num) %>% rename(player = a3.num) %>% 
                             summarise(C = sum(a3.pos == "C"), L = sum(a3.pos == "L"), R = sum(a3.pos == "R"), D = sum(a3.pos == "D"), G = sum(a3.pos == "G"), N = sum(a3.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a4.num) %>% rename(player = a4.num) %>% 
                             summarise(C = sum(a4.pos == "C"), L = sum(a4.pos == "L"), R = sum(a4.pos == "R"), D = sum(a4.pos == "D"), G = sum(a4.pos == "G"), N = sum(a4.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a5.num) %>% rename(player = a5.num) %>% 
                             summarise(C = sum(a5.pos == "C"), L = sum(a5.pos == "L"), R = sum(a5.pos == "R"), D = sum(a5.pos == "D"), G = sum(a5.pos == "G"), N = sum(a5.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a6.num) %>% rename(player = a6.num) %>% 
                             summarise(C = sum(a6.pos == "C"), L = sum(a6.pos == "L"), R = sum(a6.pos == "R"), D = sum(a6.pos == "D"), G = sum(a6.pos == "G"), N = sum(a6.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h1.num) %>% rename(player = h1.num) %>% 
                             summarise(C = sum(h1.pos == "C"), L = sum(h1.pos == "L"), R = sum(h1.pos == "R"), D = sum(h1.pos == "D"), G = sum(h1.pos == "G"), N = sum(h1.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h2.num) %>% rename(player = h2.num) %>% 
                             summarise(C = sum(h2.pos == "C"), L = sum(h2.pos == "L"), R = sum(h2.pos == "R"), D = sum(h2.pos == "D"), G = sum(h2.pos == "G"), N = sum(h2.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h3.num) %>% rename(player = h3.num) %>% 
                             summarise(C = sum(h3.pos == "C"), L = sum(h3.pos == "L"), R = sum(h3.pos == "R"), D = sum(h3.pos == "D"), G = sum(h3.pos == "G"), N = sum(h3.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h4.num) %>% rename(player = h4.num) %>% 
                             summarise(C = sum(h4.pos == "C"), L = sum(h4.pos == "L"), R = sum(h4.pos == "R"), D = sum(h4.pos == "D"), G = sum(h4.pos == "G"), N = sum(h4.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h5.num) %>% rename(player = h5.num) %>% 
                             summarise(C = sum(h5.pos == "C"), L = sum(h5.pos == "L"), R = sum(h5.pos == "R"), D = sum(h5.pos == "D"), G = sum(h5.pos == "G"), N = sum(h5.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h6.num) %>% rename(player = h6.num) %>% 
                             summarise(C = sum(h6.pos == "C"), L = sum(h6.pos == "L"), R = sum(h6.pos == "R"), D = sum(h6.pos == "D"), G = sum(h6.pos == "G"), N = sum(h6.pos %in% c("C", "L", "R", "D", "G") == F))) %>%
      data.frame() %>% group_by(player) %>%
      summarise(C = sum(C), L = sum(L), R = sum(R), D = sum(D), G = sum(G), N = sum(N)) %>% 
      mutate(Pos.Num = 1*(C > L & C > R & C > D & C > G & C > N) +
               2*(L > C & L > R & L > D & L > G & L > N) +
               3*(R > L & R > C & R > D & R > G & R > N) +
               4*(D > L & D > R & D > C & D > G & D > N) +
               5*(G > C & G > L & G > R & G > D & G > N) +
               6*(N > C & N > L & N > R & N > D & N > G)) %>%
      data.frame()
    
    posmatch$Pos <- colnames(posmatch)[-1][posmatch$Pos.Num[1:nrow(posmatch)]]
    
    roster <- roster %>% mutate(Team.Num = paste(V1, Number, sep = ""),
                                Full.Name = paste(First.Name, Last.Name, sep = "."),
                                Position = posmatch$Pos[match(Team.Num, posmatch$player)]) %>%
      rename(Team = V1) %>% data.frame()
    
    # Create shift tables / Créer tables de présences
    shiftlist.home <- NULL
    shiftlist.away <- NULL
    
    for (j in 1:(length(home.outer)-1)) {
      shiftlist.home[[j]] <- home.inner[which(home.inner == "Shift #" | home.inner == "Présence #Shift #")[j]:(which(home.inner == "SHF" | home.inner == "PR/SHF")[j]-3)]
    }
    
    for (j in 1:(length(away.outer)-1)) {
      shiftlist.away[[j]] <- away.inner[which(away.inner == "Shift #" | away.inner == "Présence #Shift #")[j]:(which(away.inner == "SHF" | away.inner == "PR/SHF")[j]-3)]
    }
    
    htoi.raw <- matrix(unlist(shiftlist.home), byrow = TRUE, ncol = 6) %>% data.frame()
    atoi.raw <- matrix(unlist(shiftlist.away), byrow = TRUE, ncol = 6) %>% data.frame()
    
    htoi.raw$p.match <- cumsum(htoi.raw$X2 == "Per")
    htoi.raw$Player <- home.players[htoi.raw$p.match[1:nrow(htoi.raw)]]
    htoi.raw <- filter(htoi.raw, X2 != "Per")
    
    atoi.raw$p.match <- cumsum(atoi.raw$X2 == "Per")
    atoi.raw$Player <- away.players[atoi.raw$p.match[1:nrow(atoi.raw)]]
    atoi.raw <- filter(atoi.raw, X2 != "Per")
    
    startmat.home <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(htoi.raw$X3), " ")), ":"))), byrow = TRUE, ncol = 5))
    endmat.home <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(htoi.raw$X4), " ")), ":"))), byrow = TRUE, ncol = 5))
    startmat.away <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(atoi.raw$X3), " ")), ":"))), byrow = TRUE, ncol = 5))
    endmat.away <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(atoi.raw$X4), " ")), ":"))), byrow = TRUE, ncol = 5))
    
    startsec.home <- 1200*(as.numeric(htoi.raw$X2) - 1) + startmat.home$X1*60 + startmat.home$X2
    endsec.home <- 1200*(as.numeric(htoi.raw$X2) - 1) + endmat.home$X1*60 + endmat.home$X2
    startsec.away <- 1200*(as.numeric(atoi.raw$X2) - 1) + startmat.away$X1*60 + startmat.away$X2
    endsec.away <- 1200*(as.numeric(atoi.raw$X2) - 1) + endmat.away$X1*60 + endmat.away$X2
    
    htoi.new <- htoi.raw %>% select(-c(X1, X3:X6, p.match)) %>% cbind(roster[match(htoi.raw$Player, roster$Num.Last.First), c(3,4,1,5,8,9)], startsec.home, endsec.home) %>%
      mutate(Duration = endsec.home - startsec.home) %>% data.frame()
    atoi.new <- atoi.raw %>% select(-c(X1, X3:X6, p.match)) %>% cbind(roster[match(atoi.raw$Player, roster$Num.Last.First), c(3,4,1,5,8,9)], startsec.away, endsec.away) %>% 
      mutate(Duration = endsec.away - startsec.away) %>% data.frame()
    
    colnames(htoi.new) <- c("Period", "Num.Last.First", "Game.ID", "Date", "Team", "Num", "Team.Num", "Full.Name", "Start.Seconds", "End.Seconds", "Duration")
    colnames(atoi.new) <- c("Period", "Num.Last.First", "Game.ID", "Date", "Team", "Num", "Team.Num", "Full.Name", "Start.Seconds", "End.Seconds", "Duration")
    
    shift.on <- rbind_list(htoi.new %>% select(Period, Start.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = Start.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                             mutate(Event = "ON", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                    Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA),
                           atoi.new %>% select(Period, Start.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = Start.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                             mutate(Event = "ON", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                    Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA)) %>% data.frame()
    
    shift.off <- rbind_list(htoi.new %>% select(Period, End.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = End.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                              mutate(Event = "OFF", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                     Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA),
                            atoi.new %>% select(Period, End.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = End.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                              mutate(Event = "OFF", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                     Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA)) %>% data.frame()
    
    who.on.1 <- function(x) {
      n <- htoi.new$Team.Num[which(as.numeric(htoi.new$Start.Seconds) <= as.numeric(x) & as.numeric(htoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      on.home <- c(n2, p)
      return(on.home)
    }
    
    who.off.1 <- function(x) {
      n <- htoi.new$Team.Num[which(as.numeric(htoi.new$Start.Seconds) < as.numeric(x) & as.numeric(htoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      off.home <- c(n2, p)
      return(off.home)
    }
    
    who.on.2 <- function(x) {
      n <- atoi.new$Team.Num[which(as.numeric(atoi.new$Start.Seconds) <= as.numeric(x) & as.numeric(atoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      on.away <- c(n2, p)
      return(on.away)
    }
    
    who.off.2 <- function(x) {
      n <- atoi.new$Team.Num[which(as.numeric(atoi.new$Start.Seconds) < as.numeric(x) & as.numeric(atoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      off.away <- c(n2, p)
      return(off.away)
    }
    
    on.home <- lapply(shift.on$Seconds, who.on.1) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(h1.num = X1, h2.num = X2, h3.num = X3, h4.num = X4, h5.num = X5, h6.num = X6, h1.pos = X13, h2.pos = X14, h3.pos = X15, h4.pos = X16, h5.pos = X17, h6.pos = X18)
    off.home <- lapply(shift.off$Seconds, who.off.1) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(h1.num = X1, h2.num = X2, h3.num = X3, h4.num = X4, h5.num = X5, h6.num = X6, h1.pos = X13, h2.pos = X14, h3.pos = X15, h4.pos = X16, h5.pos = X17, h6.pos = X18)
    on.away <- lapply(shift.on$Seconds, who.on.2) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(a1.num = X1, a2.num = X2, a3.num = X3, a4.num = X4, a5.num = X5, a6.num = X6, a1.pos = X13, a2.pos = X14, a3.pos = X15, a4.pos = X16, a5.pos = X17, a6.pos = X18)
    off.away <- lapply(shift.off$Seconds, who.off.2) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(a1.num = X1, a2.num = X2, a3.num = X3, a4.num = X4, a5.num = X5, a6.num = X6, a1.pos = X13, a2.pos = X14, a3.pos = X15, a4.pos = X16, a5.pos = X17, a6.pos = X18)
    
    shift.on <- cbind(shift.on, on.home[,c(1:6, 13:18)], on.away[,c(1:6, 13:18)]) %>% data.frame()
    shift.off <- cbind(shift.off, off.home[,c(1:6, 13:18)], off.away[,c(1:6, 13:18)]) %>% data.frame()
    
    check <- pbp.new %>% filter(Event == "FAC") %>% mutate(Event = "CHECK", Seconds = Seconds - 0.011, Description = "Checkpoint") %>% data.frame()
    
    pbp.new <- rbind_list(pbp.new, shift.on, shift.off, check) %>% arrange(Seconds) %>% 
      mutate(event.ref = cumsum(Event %in% c("ON", "OFF") == F)) %>% group_by(event.ref) %>%
      mutate(Away.Team = first(Away.Team), Home.Team = first(Home.Team), Away.Goalie = first(Away.Goalie), Home.Goalie = first(Home.Goalie),
             Home.Skaters = first(Home.Skaters), Away.Skaters = first(Away.Skaters), Strength.State = first(Strength.State), 
             Home.Score = first(Home.Score), Away.Score = first(Away.Score), Score.State = first(Score.State), Score.Cat = first(Score.Cat),
             Seconds = Seconds - 0.001*(Event == "OFF") + 0.001*(Event == "ON")) %>% 
      filter(Event != "CHECK") %>% arrange(Seconds) %>% data.frame() %>% select(-c(event.ref)) %>% data.frame() 
    
    pbp.new$Event.Length <- c((pbp.new$Seconds[2:nrow(pbp.new)] - pbp.new$Seconds[1:(nrow(pbp.new) - 1)]), 0)
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE HIGHLIGHTS JSON / ACQUÉRIR JSON DE FAITS-SAILLANTS ###################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URL / Définir URL
    year <- substr(season, start = 1, stop = 4)
    url3 <- paste("http://live.nhle.com/GameData/", season, "/", year, "0", ID, "/gc/gcgm.jsonp", sep = "")
    
    full.text.3 <- try(getURL(url3, header = FALSE,
                              .opts = curlOptions(
                                referer = 'nhl.com',
                                verbose = TRUE,
                                header = TRUE,
                                followLocation = TRUE,
                                useragent = agents[sample(1:4, 1)]))
    )
    
    if (class(full.text.3) == "try-error") {
      full.text.3 <- getURL(url3, header = FALSE,
                            .opts = curlOptions(
                              referer = 'nhl.com',
                              verbose = TRUE,
                              header = TRUE,
                              followLocation = TRUE,
                              useragent = agents[sample(1:4, 1)]))
    }
    
    text.3 <- unlist(strsplit(full.text.3, ","))
    
    hl.presecs <- gsub("sip[\":]*", "", unlist(regmatches(text.3, gregexpr("sip[\":]*[0-9]*", text.3))))
    hl.period <- gsub("[^a-z]+p[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+p[\":]+[0-9]*", text.3))))
    hl.Team1 <- gsub("[^a-z]+t1[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+t1[\":]+[A-Z]*", text.3))))
    hl.Team2 <- gsub("[^a-z]+t2[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+t2[\":]+[A-Z]*", text.3))))
    hl.seconds <- 1200*(as.numeric(hl.period) - 1) + as.numeric(hl.presecs)
    if(as.numeric(season) >= 20152016) {
      
      urls <- paste("https://www.nhl.com/video/c-", gsub("neulionId\\\":", "", unlist(regmatches(text.3, gregexpr("neulionId\\\":[0-9]+", text.3)))), sep = "")
      
    } else {
      
      urls <- NA
      
    }
    
    hl.mat <- cbind(hl.seconds, urls) %>% data.frame()
    
    pbp.new$URL <- hl.mat$urls[match((pbp.new$Event %in% c("SHOT", "GOAL"))*round(as.numeric(as.character(pbp.new$Seconds)), 0), hl.mat$hl.seconds)]
    pbp.new$Highlight <- 1*(!is.na(pbp.new$URL))
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE SPORTSNET / ACQUÉRIR DONÉES SPORTSNET ################################################################################################################################
    ########################################################################################################################################################################################################
    
    if (as.numeric(season) >= 20152016) {
      
      # Scrape main page / Acquérir page primaire 
      url <- paste("https://statsapi.web.nhl.com/api/v1/game/", year, "0", ID, "/feed/live?site=en_nhl", sep = "")
      glist <- try(getURL(url, header = FALSE,
                          .opts = curlOptions(
                            referer = 'nhl.com',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(glist) == "try-error") {
        glist <- getURL(url, header = FALSE,
                        .opts = curlOptions(
                          referer = 'nhl.com',
                          verbose = TRUE,
                          header = TRUE,
                          followLocation = TRUE,
                          useragent = agents[sample(1:4, 1)]))
      }
      
      jason <- fromJSON(glist)
      plays <- jason$liveData$plays$allPlays
      
      lapply(plays,
             do_plays
             ) %>%
        unlist() %>%
        matrix(ncol = 6,
               byrow = TRUE
               ) %>%
        data.frame() ->
        coordmat
      
      coordmat$X2 <- as.character(coordmat$X2)
      coordmat$X4 <- as.character(coordmat$X4)
      
      # Rename events
      coordmat$X2[which(coordmat$X2 == "FACEOFF")] <- "FAC"
      coordmat$X2[which(coordmat$X2 == "PENALTY")] <- "PENL"
      coordmat$X2[which(coordmat$X2 == "GIVEAWAY")] <- "GIVE"
      coordmat$X2[which(coordmat$X2 == "TAKEAWAY")] <- "TAKE"
      coordmat$X2[which(coordmat$X2 == "MISSED_SHOT")] <- "MISS"
      coordmat$X2[which(coordmat$X2 == "BLOCKED_SHOT")] <- "BLOCK"
      
      # Parse time
      timemat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(coordmat$X4), ":"))), byrow = TRUE, ncol = 2))
    
      coordmat$seconds <- 1200*(as.numeric(coordmat$X3) - 1) + timemat$X1*60 + timemat$X2
      
      # Match with PBP / Associer au résumé
      merge(pbp.new %>%
              mutate(rounded = round(nabs(Seconds), 0)) %>%
              data.frame(),
            coordmat %>%
              select(X2, X5, X6, seconds) %>%
              data.frame(),
            by.x = c("rounded", "Event"),
            by.y = c("seconds", "X2"),
            all.x = TRUE
            ) %>%
        data.frame() %>%
        select(-c(rounded)) %>%
        arrange(Seconds) %>%
        data.frame() ->
        pbp.new
      
    } else {
      
      # Provide date / Fournir date
      day <- gsub("-", "", as.character(date))
      
      # Scrape main page / Acquérir page primaire 
      url <- paste("http://scores.espn.go.com/nhl/scoreboard?date=", day, sep = "")
      glist <- try(getURL(url, header = FALSE,
                          .opts = curlOptions(
                            referer = 'sports.espn.go.com',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(glist) == "try-error") {
        glist <- getURL(url, header = FALSE,
                        .opts = curlOptions(
                          referer = 'sports.espn.go.com',
                          verbose = TRUE,
                          header = TRUE,
                          followLocation = TRUE,
                          useragent = agents[sample(1:4, 1)]))
      }
      
      gameids <- unique(unlist(regmatches(glist, gregexpr("gameId=[0-9]+", glist))))
      teams <- toupper(gsub("team/_/name/|>|</div>", "", unique(unlist(regmatches(glist, gregexpr("team/_/name/[a-zA-Z]+|>(Coyotes|Thrashers)</div>", glist)))))) # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      
      # Format team names / Changer noms d'équipes 
      teams[which(teams == "PHX")] <- "ARI"
      teams[which(teams == "TB")] <- "T.B"
      teams[which(teams == "NJ")] <- "N.J"
      teams[which(teams == "SJ")] <- "S.J"
      teams[which(teams == "LA")] <- "L.A"
      teams[which(teams == "COYOTES")] <- "ARI" # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      teams[which(teams == "THRASHERS")] <- "ATL" # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      
      if (as.numeric(season) < 20110000) {
        teams[which(teams == "WPG")] <- "ATL"
      } # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      
      teammat <- matrix(unique(teams), byrow = TRUE, ncol = 2) %>% data.frame()
      
      # Create URL directory / Créer annuaire de URLs
      url.match <- cbind(gameids, teammat) %>% data.frame() %>% rename(awayteam = X1, hometeam = X2)
      
      # Match URL / Associer URL
      urlt <- first(as.character(url.match$gameids[which(as.character(url.match$awayteam) == as.character(awayteam) | as.character(url.match$hometeam) == as.character(awayteam))]))
      
      ####################################################################################################################################################################################
      ####################################################################################################################################################################################
      
      # Scrape game page / Acquérir page du match
      url2 <- paste("http://sports.espn.go.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&", urlt, sep = "")
      gamepage <- try(getURL(url2, header = FALSE,
                             .opts = curlOptions(
                               referer = 'sports.espn.go.com',
                               verbose = TRUE,
                               header = TRUE,
                               followLocation = TRUE,
                               useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(gamepage) == "try-error") {
        gamepage <- getURL(url2, header = FALSE,
                           .opts = curlOptions(
                             referer = 'sports.espn.go.com',
                             verbose = TRUE,
                             header = TRUE,
                             followLocation = TRUE,
                             useragent = agents[sample(1:4, 1)]))
      }
      
      enames = c("FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL",
                 "STOP", "PRDY", "PSTR", "PEND", "PERD", "SOC", "GEnd", "SOut",
                 "error", "TAKE", "GIVE", "early intermission", "nothing", "nothing")
      ecodes = as.character(c(502, 503, 504, 505, 506, 507, 508, 509,
                              516, 517, 518, 519, 520, 521, 522, 0, 
                              9999, 1401, 1402, -2147483648, 1, 5))
      
      etext <- unlist(regmatches(gamepage, gregexpr("<Play.*?/Play>", gamepage)))
      
      if (length(etext) > 1) {
        esplit <- t(do.call(cbind, strsplit(etext, "[\\[~]")))
        esplit <- esplit[,c(5,3,4,6,7,11)]
        colnames(esplit) <- c("etype","xc","yc","time","period","event.description")
        esplit <- esplit[,1:5] %>% as.data.frame(stringsAsFactors = FALSE)
        
        esplit$etype <- enames[match(esplit$etype, ecodes)]
        
        timesplits <- do.call(rbind, strsplit(esplit$time, ":"))
        seconds <- 1200*(as.numeric(esplit$period) - 1) + as.numeric(timesplits[,1])*60 + as.numeric(timesplits[,2])
        esplit$seconds <- seconds
        
        # Match with PBP / Associer au résumé
        pbp.new <- group_by(pbp.new, Seconds, Event) %>% 
          mutate(XC = first(esplit$xc[which(esplit$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(esplit$etype) == as.character(Event))]),
                 YC = first(esplit$yc[which(esplit$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(esplit$etype) == as.character(Event))]))
      } else {
        
        pbp.new$XC <- NA; pbp.new$YC <- NA
        
      }
    }
    
    ########################################################################################################################################################################################################
    ########################################################################################################################################################################################################
    ########################################################################################################################################################################################################
    
    # Fix duplicate names / Réparer noms doubles
    roster$Full.Name[which(roster$Full.Name == "ERIK.KARLSSON" & roster$Team == "CAR")] <- "ERIK.KARLSSON.2"
    roster$Full.Name[which(roster$Full.Name == "ERIK.GUSTAFSSON" & roster$Team == "PHI")] <- "ERIK.GUSTAFSSON.2"
    roster$Full.Name[which(roster$Full.Name == "PK.SUBBAN" | roster$Full.Name == "P.K.SUBBAN")] <- "P.K..SUBBAN"
    roster$Full.Name[which(roster$Full.Name == "TJ.OSHIE" | roster$Full.Name == "T.J.OSHIE")] <- "T.J..OSHIE"
    roster$Full.Name[which(roster$Full.Name == "BJ.CROMBEEN" | roster$Full.Name == "B.J.CROMBEEN" | roster$Full.Name == "BRANDON.CROMBEEN")] <- "B.J..CROMBEEN"
    roster$Full.Name[which(roster$Full.Name == "ILJA.BRYZGALOV")] <- "ILYA.BRYZGALOV"
    roster$Full.Name[which(roster$Full.Name == "CAMERON.BARKER")] <- "CAM.BARKER"
    roster$Full.Name[which(roster$Full.Name == "CHRIS.VANDE VELDE")] <- "CHRIS.VANDEVELDE"
    roster$Full.Name[which(roster$Full.Name == "DANIEL.CARCILLO")] <- "DAN.CARCILLO"
    roster$Full.Name[which(roster$Full.Name == "DANIEL.CLEARY")] <- "DAN.CLEARY"
    roster$Full.Name[which(roster$Full.Name == "DAVID JOHNNY.ODUYA")] <- "JOHNNY.ODUYA"
    roster$Full.Name[which(roster$Full.Name == "DAVID.BOLLAND")] <- "DAVE.BOLLAND"
    roster$Full.Name[which(roster$Full.Name == "DWAYNE.KING")] <- "DJ.KING"
    roster$Full.Name[which(roster$Full.Name == "EVGENII.DADONOV")] <- "EVGENY.DADONOV"
    roster$Full.Name[which(roster$Full.Name == "FREDDY.MODIN")] <- "FREDRIK.MODIN"
    roster$Full.Name[which(roster$Full.Name == "HARRISON.ZOLNIERCZYK")] <- "HARRY.ZOLNIERCZYK"
    roster$Full.Name[which(roster$Full.Name == "J P.DUMONT" | roster$Full.Name == "JEAN-PIERRE.DUMONT")] <- "J-P.DUMONT"
    roster$Full.Name[which(roster$Full.Name == "JEAN-FRANCOIS.JACQUES")] <- "J-F.JACQUES"
    roster$Full.Name[which(roster$Full.Name == "JONATHAN.AUDY-MARCHESSAULT")] <- "JONATHAN.MARCHESSAULT"
    roster$Full.Name[which(roster$Full.Name == "JOSHUA.HENNESSY")] <- "JOSH.HENNESSY"
    roster$Full.Name[which(roster$Full.Name == "KRISTOPHER.LETANG")] <- "KRIS.LETANG"
    roster$Full.Name[which(roster$Full.Name == "KRYSTOFER.BARCH")] <- "KRYS.BARCH"
    roster$Full.Name[which(roster$Full.Name == "MARTIN.ST LOUIS")] <- "MARTIN.ST. LOUIS"
    roster$Full.Name[which(roster$Full.Name == "MATTHEW.CARLE")] <- "MATT.CARLE"
    roster$Full.Name[which(roster$Full.Name == "MATTHEW.DUMBA")] <- "MATT.DUMBA"
    roster$Full.Name[which(roster$Full.Name == "JOSEPH.CORVO")] <- "JOE.CORVO"
    roster$Full.Name[which(roster$Full.Name == "TOBY.ENSTROM")] <- "TOBIAS.ENSTROM"
    roster$Full.Name[which(roster$Full.Name == "MICHAEL.SANTORELLI")] <- "MIKE.SANTORELLI"
    roster$Full.Name[which(roster$Full.Name == "MICHAEL.CAMMALLERI")] <- "MIKE.CAMMALLERI"
    roster$Full.Name[which(roster$Full.Name == "PIERRE.PARENTEAU" | roster$Full.Name == "PIERRE-ALEX.PARENTEAU")] <- "PA.PARENTEAU"
    roster$Full.Name <- gsub("ALEXANDER.|ALEXANDRE.", "ALEX.", roster$Full.Name)
    roster$Full.Name <- gsub("CHRISTOPHER.", "CHRIS.", roster$Full.Name)
    roster$Full.Name[which(roster$Full.Name == "NICOLAS.PETAN")] <- "NIC.PETAN"
    roster$Full.Name[which(roster$Full.Name == "NIKOLAI.KULEMIN")] <- "NIKOLAY.KULEMIN"
    
    # Add roster match code / Ajouter code d'association
    roster$Player.Code <- gsub("[^A-Za-z]", "", roster$Full.Name)
    
    # Replace teamnum / Remplacer code équipenum
    if(names == TRUE) {
      pbp.new$p1 <- roster$Full.Name[match(pbp.new$p1, roster$Team.Num)]; pbp.new$p2 <- roster$Full.Name[match(pbp.new$p2, roster$Team.Num)]; pbp.new$p3 <- roster$Full.Name[match(pbp.new$p3, roster$Team.Num)]
      pbp.new$a1.num <- roster$Full.Name[match(pbp.new$a1.num, roster$Team.Num)]; pbp.new$a2.num <- roster$Full.Name[match(pbp.new$a2.num, roster$Team.Num)]; pbp.new$a3.num <- roster$Full.Name[match(pbp.new$a3.num, roster$Team.Num)]
      pbp.new$a4.num <- roster$Full.Name[match(pbp.new$a4.num, roster$Team.Num)]; pbp.new$a5.num <- roster$Full.Name[match(pbp.new$a5.num, roster$Team.Num)]; pbp.new$a6.num <- roster$Full.Name[match(pbp.new$a6.num, roster$Team.Num)]
      pbp.new$h1.num <- roster$Full.Name[match(pbp.new$h1.num, roster$Team.Num)]; pbp.new$h2.num <- roster$Full.Name[match(pbp.new$h2.num, roster$Team.Num)]; pbp.new$h3.num <- roster$Full.Name[match(pbp.new$h3.num, roster$Team.Num)]
      pbp.new$h4.num <- roster$Full.Name[match(pbp.new$h4.num, roster$Team.Num)]; pbp.new$h5.num <- roster$Full.Name[match(pbp.new$h5.num, roster$Team.Num)]; pbp.new$h6.num <- roster$Full.Name[match(pbp.new$h6.num, roster$Team.Num)]
      pbp.new$Away.Goalie <- roster$Full.Name[match(pbp.new$Away.Goalie, roster$Team.Num)]; pbp.new$Home.Goalie <- roster$Full.Name[match(pbp.new$Home.Goalie, roster$Team.Num)]
    }
    
    # Populate lists / Peupler listes
    pbp.list[[i]] <- c(t(pbp.new))
    cnames1 <- colnames(pbp.new)
    
    roster.list[[i]] <- c(t(roster))
    cnames2 <- colnames(roster)
    
    cat(paste("Pausing", pause, "seconds...\n"))
    Sys.sleep(pause)
    
  }
  
  # Unlist into tables / Remplir tables
  pbp.full <- matrix(unlist(pbp.list), byrow = T, ncol = 53) %>% as.data.frame(stringsAsFactors = FALSE)
  colnames(pbp.full) <- cnames1
  pbp.full$Season <- season
  pbp.full$Season.Type <- "Regular"
  pbp.full$Season.Type[which(as.numeric(as.character(pbp.full$Game.ID)) >= 30000)] <- "Playoffs"
  
  roster.full <- matrix(unlist(roster.list), byrow = T, ncol = 11) %>% data.frame()
  colnames(roster.full) <- cnames2
  roster.full$Season <- season
  roster.full$Season.Type <- "Regular"
  roster.full$Season.Type[which(as.numeric(as.character(roster.full$Game.ID)) >= 30000)] <- "Playoffs"
  
  # Print record of missing games / Imprimer un record de matchs manquants
  missing <- games[which(games %in% unique(pbp.full$Game.ID) == FALSE)]
  
  if (length(missing) > 0) {
    cat(paste("Oops! Games missing:", paste(missing, collapse = ", "), "\n"))
  } else {
    cat("Success! All games were scraped.\n")
  }
  
  return(pbp.full)
  
}

# Enhance PBP
st.pbp_enhance <- function(pbp) {
  
  ## Description
  # pbp_enhance() performs some preliminary operations on a given PBP data frame object and returns \
  # the enhanced version
  
  pbp %>%
    mutate_each(funs(nabs), coords_x, coords_y, game_period, game_seconds) %>%
    data.frame() -> 
    pbp
  
  pbp %>%
    mutate(event_distance = st.distance_from_net(coords_x, coords_y),
           event_angle = st.angle_from_centre(coords_x, coords_y),
           event_rinkside = st.which_zone(coords_x),
           event_circle = st.which_circle(coords_x, coords_y)
           ) %>%
    data.frame() ->
    enhanced_pbp
  
  enhanced_pbp %>%
  filter(event_type %in% c("FAC",
                           "TAKE", 
                           "BLOCK", 
                           "SHOT", 
                           "MISS", 
                           "HIT", 
                           "GIVE",
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
         coords_y_last = lag(coords_y, 1)
         ) %>%
  ungroup() %>%
  arrange(season, game_id, event_index) %>%
  select(season,
         session,
         game_id,
         game_date,
         game_period,
         game_seconds,
         event_index,
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
         coords_y_last
         ) %>%
  data.frame() ->
  xg_pbp

  xg_pbp$game_strength_state[which(xg_pbp$game_strength_state %in% st.strength_states == FALSE)] <- "5v5"
  
  xg_pbp %>%
  filter(event_type %in% st.fenwick_events,
         !{game_period > 4 & session == "R"},
         !is.na(coords_x),
         !is.na(coords_y)
         ) %>%
  mutate(same_team_last = 1*(event_team == event_team_last),
         is_home_team = 1*(event_team == home_team),
         is_EN = 1*({event_team == home_team & away_goalie == 0} | {event_team == away_team & home_goalie == 0})
         ) %>%
  select(-c(home_goalie, away_goalie)) %>%
  data.frame() ->
  model_data
  
  model_data$shooter_strength_state <- ifelse(model_data$is_home_team == 1,
                                              model_data$game_strength_state,
                                              str_rev(model_data$game_strength_state)
                                              )

  model_data$shooter_score_adv <- ifelse(model_data$is_home_team == 1,
                                         model_data$home_score - model_data$away_score,
                                         model_data$away_score - model_data$home_score
                                         )
  
  model_data$event_detail <- as.character(model_data$event_detail)
  model_data$event_detail[which(is.na(model_data$event_detail) == TRUE)] <- "Unknown"
  model_data <- na.omit(model_data)
  
  model_data$distance_from_last <- sqrt((model_data$coords_x - model_data$coords_x_last)^2 + (model_data$coords_y - model_data$coords_y_last)^2)

  model_data$is_goal <- 1*(model_data$event_type == "GOAL")
  model_data$is_save <- 1*(model_data$event_type == "SHOT")
  
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
  
  model_data$event_type_last <- as.factor(model_data$event_type_last)
  model_data$shooter_strength_state <- as.factor(model_data$shooter_strength_state)
  
  model_mat <- cbind(outcome = as.factor(1*(model_data$is_save) + 2*(model_data$is_goal) + 1),
                     predict(xg_process,
                             model_data[, c("is_goal", vars)]
                             ) %>%
                       data.frame()
                     )
  
  predicted <- glmnet::predict.cv.glmnet(xg_glm,
                                         newx = as.matrix(model_mat[, -1]),
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
  
  model_data$prob_goal <- as.numeric(prob_df$prob_goal)
  model_data$prob_save <- as.numeric(prob_df$prob_save)
  
  merge(enhanced_pbp,
        model_data %>%
          select(game_id, event_index, prob_goal, prob_save) %>%
          data.frame(),
        by.x = c("game_id", "event_index"),
        by.y = c("game_id", "event_index"),
        all.x = TRUE
        ) %>%
    data.frame() ->
    enhanced_pbp
  
  enhanced_pbp %>%
  filter(event_type %in% c(st.corsi_events, "FACEOFF"),
         !{session == "R" & game_period > 4},
         !is.na(home_rinkside),
         !is.na(away_rinkside)
         ) %>%
  arrange(game_id, event_index) %>%
  mutate(faceoff_ref = cumsum(event_type == "FACEOFF")) %>%
  group_by(game_id, faceoff_ref) %>%
  arrange(event_index) %>%
  mutate(seconds_since = nabs(game_seconds) - min(nabs(game_seconds)),
         rinkside_start = first(event_rinkside),
         is_home_team = 1*({event_type %in% st.fenwick_events & event_team == home_team} |
                           {event_type == "BLOCKED_SHOT" & event_team == away_team}),
         home_zonestart = 1*(rinkside_start == home_rinkside) + 2*(rinkside_start == "N") + 3*(rinkside_start != home_rinkside & rinkside_start != "N"),
         home_score_adv = home_score - away_score
         ) %>%
  ungroup() %>%
  arrange(game_id, event_index) %>%
  data.frame() ->
  adj_pbp
  
  adj_pbp$game_strength_state[which(adj_pbp$game_strength_state %in% st.strength_states == FALSE)] <- "5v5"
  
  adj_pbp %>%
  filter(game_strength_state %in% st.strength_states) %>%
  group_by(game_id) %>%
  arrange(event_index) %>%
  mutate(elapsed = game_seconds - lag(game_seconds, 1),
         hazard_goal = 1*(event_type == "GOAL"),
         hazard_shot = 1*(event_type %in% st.shot_events),
         hazard_fenwick = 1*(event_type %in% st.fenwick_events),
         hazard_corsi = 1*(event_type %in% st.corsi_events)
         ) %>%
  ungroup() %>%
  select(game_id,
         event_index,
         event_type,
         elapsed,
         hazard_goal:hazard_corsi,
         is_home_team,
         seconds_since,
         rinkside_start,
         home_zonestart,
         game_strength_state,
         home_score_adv,
         game_seconds
         ) %>%
  data.frame() ->
  model_data

  model_data$elapsed[which(is.na(model_data$elapsed) == TRUE)] <- 0
  model_data$elapsed[which(model_data$elapsed == 0)] <- 0.01

  model_data$home_zonestart <- as.factor(model_data$home_zonestart)
  model_data$game_strength_state <- as.factor(model_data$game_strength_state)
  
  model_mat <- predict(home_corsi_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_home_corsi <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_home_corsi, 
                                                                          as.matrix(model_mat), 
                                                                          s = "lambda.min"
                                                                          )
                                                )
                                     )
  
  model_mat <- predict(home_fenwick_process,
                       model_data
                       ) %>%
                 data.frame()

  model_data$adj_home_fenwick <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_home_fenwick, 
                                                                            as.matrix(model_mat), 
                                                                            s = "lambda.min"
                                                                            )
                                                  )
                                       )  

  model_mat <- predict(home_shot_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_home_shot <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_home_shot, 
                                                                         as.matrix(model_mat), 
                                                                         s = "lambda.min"
                                                                         )
                                               )
                                    )
  
  model_mat <- predict(home_goal_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_home_goal <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_home_goal, 
                                                                         as.matrix(model_mat), 
                                                                         s = "lambda.min"
                                                                         )
                                               )
                                    )
  
  model_mat <- predict(away_corsi_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_away_corsi <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_away_corsi, 
                                                                          as.matrix(model_mat), 
                                                                          s = "lambda.min"
                                                                          )
                                                )
                                     )
  
  model_mat <- predict(away_fenwick_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_away_fenwick <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_away_fenwick, 
                                                                            as.matrix(model_mat), 
                                                                            s = "lambda.min"
                                                                            )
                                                  )
                                       )
  
  model_mat <- predict(away_shot_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_away_shot <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_away_shot, 
                                                                         as.matrix(model_mat), 
                                                                         s = "lambda.min"
                                                                         )
                                               )
                                    )
  
  model_mat <- predict(away_goal_process,
                       model_data
                       ) %>%
                 data.frame()
  
  model_data$adj_away_goal <- 1/exp(as.numeric(glmnet::predict.cv.glmnet(cox_away_goal, 
                                                                         as.matrix(model_mat), 
                                                                         s = "lambda.min"
                                                                         )
                                               )
                                    )
  
  merge(enhanced_pbp,
        model_data %>%
          select(game_id, event_index, adj_home_corsi:adj_away_goal) %>%
          data.frame(),
        by.x = c("game_id", "event_index"),
        by.y = c("game_id", "event_index"),
        all.x = TRUE
        ) %>%
    data.frame() ->
    enhanced_pbp
  
  return(enhanced_pbp)
  
}

