### COMPILE GAMES ###
# Last edit: Manny (2017-07-03)


## Dependencies
require(RCurl); require(rjson); require(dplyr); require(lubridate); 
require(doMC); require(Kmisc); require(RSQLite); require(rvest)
load("/srv/shiny-server/modules/user_functions.RData")
load("/srv/shiny-server/modules/dryscrape.RData")
load("/srv/shiny-server/modules/stats.RData")


## Compile games
# Scrape
game_list <- ds.compile_games(games = as.character(20301:20600),
                              season = "20162017",
                              pause = 2,
                              try_tolerance = 5,
                              agents = ds.user_agents
                              )

# Unpack
pbp <- game_list[[1]]
roster <- game_list[[2]]
shifts <- game_list[[3]]


## Write to database
# Connect
conn <- dbConnect(SQLite(), "~/corsica_data/raw.sqlite")

# Check for duplicates
pbp_contains <- sqliteQuickColumn(conn,
                                  "pbp",
                                  "game_id"
                                  )

pbp %>%
  filter(game_id %in% unique(pbp_contains) == FALSE) %>%
  data.frame() ->
  pbp

roster_contains <- sqliteQuickColumn(conn,
                                     "roster",
                                     "game_id"
                                     )

roster %>%
  filter(game_id %in% unique(roster_contains) == FALSE) %>%
  data.frame() ->
  roster

shifts_contains <- sqliteQuickColumn(conn,
                                     "shifts",
                                     "game_id"
                                     )

shifts %>%
  filter(game_id %in% unique(shifts_contains) == FALSE) %>%
  data.frame() ->
  shifts

# Write
dbWriteTable(conn,
             "pbp",
             pbp,
             overwrite = FALSE,
             append = TRUE
             )

dbWriteTable(conn,
             "roster",
             roster,
             overwrite = FALSE,
             append = TRUE
             )

dbWriteTable(conn,
             "shifts",
             shifts,
             overwrite = FALSE,
             append = TRUE
             )


# Disconnect
dbDisconnect(conn)

