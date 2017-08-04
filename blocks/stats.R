### STATS ###
# Last edit: Manny (2017-03-30)


# Game Score Formula: 0.750*G + 0.700*A1 + 0.550*A2 + 0.075*iSF + 0.050*iBLK + 0.150*iPEND + 0.010*iFOW + 0.050*CF + 0.150*GF - 0.150*iPENT - 0.010*iFOL - 0.050*CA - 0.150*GA

## Full Team Stats
{
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_team, game_score_state, game_strength_state) %>%
    st.sum_team("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_team, game_score_state, game_strength_state) %>%
    st.sum_team("Away")
) %>%
  data.frame() ->
  team_stats
}

## Full Skater Stats
{
bind_rows(
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_1, game_strength_state) %>%
    rename(player = home_on_1) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_2, game_strength_state) %>%
    rename(player = home_on_2) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_3, game_strength_state) %>%
    rename(player = home_on_3) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_4, game_strength_state) %>%
    rename(player = home_on_4) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_5, game_strength_state) %>%
    rename(player = home_on_5) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, home_on_6, game_strength_state) %>%
    rename(player = home_on_6) %>%
    st.sum_skater("Home"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_1, game_strength_state) %>%
    rename(player = away_on_1) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_2, game_strength_state) %>%
    rename(player = away_on_2) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_3, game_strength_state) %>%
    rename(player = away_on_3) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_4, game_strength_state) %>%
    rename(player = away_on_4) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_5, game_strength_state) %>%
    rename(player = away_on_5) %>%
    st.sum_skater("Away"),
  
  pbp %>%
    filter(!{game_period > 4 & session == "R"}) %>%
    group_by(season, session, game_id, game_date, away_on_6, game_strength_state) %>%
    rename(player = away_on_6) %>%
    st.sum_skater("Away")
) %>%
  data.frame() ->
  skater_stats
}

## Full Goalie Stats
{
  bind_rows(
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, home_goalie, game_strength_state) %>%
      st.old_sum_goalie("Home"),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, away_goalie, game_strength_state) %>%
      st.old_sum_goalie("Away")
  ) %>%
    data.frame() ->
    goalie_stats_full
}

## 2-Player Combos
{
  bind_rows(
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h2.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h2.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h3.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, h3.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = h3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, h4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = h4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, h5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = h5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, h4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = h4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, h5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = h5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h4.num, h5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h4.num, player_2 = h5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h4.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h4.num, player_2 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h5.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h5.num, player_2 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a2.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a2.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a3.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a2.num, a3.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a2.num, player_2 = a3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a2.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a2.num, player_2 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a2.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a2.num, player_2 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a2.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a2.num, player_2 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a3.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a3.num, player_2 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a3.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a3.num, player_2 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a3.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a3.num, player_2 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a4.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a4.num, player_2 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a4.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a4.num, player_2 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a5.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a5.num, player_2 = a6.num)
  ) %>%
    data.frame() %>%
    filter(!is.na(player_1),
           !is.na(player_2)
    ) %>%
    group_by(player_1, player_2) %>%
    mutate(combo_code = st.combo_code(first(player_1), first(player_2), "0000000")) %>%
    group_by(season, session, game_id, game_date, combo_code, game_strength_state, venue) %>%
    summarise_each(funs(sum), -c(GP, player_1, player_2)) %>%
    data.frame() ->
    combo_stats_2
}

## 3-Player Combos
{
  bind_rows(
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h2.num, h3.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h2.num, player_3 = h3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h2.num, h4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h2.num, player_3 = h4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h2.num, h5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h2.num, player_3 = h5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h2.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h2.num, player_3 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h3.num, h4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h3.num, player_3 = h4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h3.num, h5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h3.num, player_3 = h5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h3.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h3.num, player_3 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h4.num, h5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h4.num, player_3 = h5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h4.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h4.num, player_3 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, h5.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = h5.num, player_3 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, h3.num, h4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = h3.num, player_3 = h4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, h3.num, h5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = h3.num, player_3 = h5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, h3.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = h3.num, player_3 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, h4.num, h5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = h4.num, player_3 = h5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, h4.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = h4.num, player_3 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, h5.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = h5.num, player_3 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, h4.num, h5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = h4.num, player_3 = h5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, h4.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = h4.num, player_3 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, h5.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = h5.num, player_3 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h4.num, h5.num, h6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h4.num, player_2 = h5.num, player_3 = h6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a2.num, a3.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a2.num, player_3 = a3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a2.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a2.num, player_3 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a2.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a2.num, player_3 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a2.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a2.num, player_3 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a3.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a3.num, player_3 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a3.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a3.num, player_3 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a3.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a3.num, player_3 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a4.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a4.num, player_3 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a4.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a4.num, player_3 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a1.num, a5.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a1.num, player_2 = a5.num, player_3 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a2.num, a3.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a2.num, player_2 = a3.num, player_3 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a2.num, a3.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a2.num, player_2 = a3.num, player_3 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a2.num, a3.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a2.num, player_2 = a3.num, player_3 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a2.num, a4.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a2.num, player_2 = a4.num, player_3 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a2.num, a4.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a2.num, player_2 = a4.num, player_3 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a2.num, a5.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a2.num, player_2 = a5.num, player_3 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a3.num, a4.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a3.num, player_2 = a4.num, player_3 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a3.num, a4.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a3.num, player_2 = a4.num, player_3 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a3.num, a5.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a3.num, player_2 = a5.num, player_3 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, a4.num, a5.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Away") %>%
      rename(player_1 = a4.num, player_2 = a5.num, player_3 = a6.num)
  ) %>%
    data.frame() %>%
    filter(!is.na(player_1),
           !is.na(player_2),
           !is.na(player_3)
    ) %>%
    group_by(player_1, player_2, player_3) %>%
    mutate(combo_code = st.combo_code(first(player_1), first(player_2), first(player_3))) %>%
    group_by(season, session, game_id, game_date, combo_code, game_strength_state, venue) %>%
    summarise_each(funs(sum), -c(GP, player_1, player_2, player_3)) %>%
    data.frame() ->
    combo_stats_3
}

## Matchups
{
  bind_rows(
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, a1.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = a1.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, a2.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = a2.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, a3.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = a3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h1.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h1.num, player_2 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, a1.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = a1.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, a2.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = a2.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, a3.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = a3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h2.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h2.num, player_2 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, a1.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = a1.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, a2.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = a2.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, a3.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = a3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h3.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h3.num, player_2 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h4.num, a1.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h4.num, player_2 = a1.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h4.num, a2.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h4.num, player_2 = a2.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h4.num, a3.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h4.num, player_2 = a3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h4.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h4.num, player_2 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h4.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h4.num, player_2 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h4.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h4.num, player_2 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h5.num, a1.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h5.num, player_2 = a1.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h5.num, a2.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h5.num, player_2 = a2.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h5.num, a3.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h5.num, player_2 = a3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h5.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h5.num, player_2 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h5.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h5.num, player_2 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h5.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h5.num, player_2 = a6.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h6.num, a1.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h6.num, player_2 = a1.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h6.num, a2.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h6.num, player_2 = a2.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h6.num, a3.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h6.num, player_2 = a3.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h6.num, a4.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h6.num, player_2 = a4.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h6.num, a5.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h6.num, player_2 = a5.num),
    
    pbp %>%
      filter(!{game_period > 4 & session == "R"}) %>%
      group_by(season, session, game_id, game_date, h6.num, a6.num, game_strength_state) %>%
      st.old_sum_team("Home") %>%
      rename(player_1 = h6.num, player_2 = a6.num)
  ) %>%
    data.frame() %>%
    filter(!is.na(player_1),
           !is.na(player_2)
    ) %>%
    group_by(player_1, player_2) %>%
    mutate(combo_code = st.combo_code(first(player_1), first(player_2), "0000000")) %>%
    group_by(season, session, game_id, game_date, combo_code, game_strength_state, venue) %>%
    summarise_each(funs(sum), -c(GP, player_1, player_2)) %>%
    data.frame() ->
    matchup_stats
}
