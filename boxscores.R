#######################################################################################################################################################
################################ LIBRERIAS   ##########################################################################################################
#######################################################################################################################################################


library(tidyverse)
library(jsonlite)
library(httr)
library(httr2)


ronda_fn <- function(ronda) {
  df <- paste0("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/E/seasons/E2025/games?teamCode=&phaseTypeCode=RS&roundNumber=", ronda) %>%
    httr::GET(query = list()) %>%
    httr::content() %>%
    purrr::pluck("data") %>%
    dplyr::tibble(value = .) %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(round) %>%
    tidyr::unnest_wider(confirmedDate, names_sep = "_") %>%
    transmute(jornada = round, gamecode = code, date = lubridate::with_tz(lubridate::ymd_hms(date), "Europe/Madrid"))
}

ronda_df <- map_df(1:38, ronda_fn)

write_csv(ronda_df, here::here("substack/boxScoreEuroligue/csv/gamecodes_2025-26.csv"))


#######################################################################################################################################################
################################ GAMESCODES  ##########################################################################################################
#######################################################################################################################################################

gamecode <- read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/Euroleague-boxscores/refs/heads/main/gamecodes/gamecodes_2025-26.csv",
  show_col_types = FALSE,
  progress = FALSE
) %>%
  arrange(round, gamecode, date) %>%
  filter(date < today(tzone = "Europe/Madrid")) %>%
  pull(gamecode)

#######################################################################################################################################################
################################ FUNCIÓN BOXSCORE #####################################################################################################
#######################################################################################################################################################



boxscores_fn <- function(gamecode) {
  url <- paste0("https://live.euroleague.net/api/Boxscore?gamecode=", gamecode, "&seasoncode=E2025")

  round <- ronda_df %>%
    rename(codigo = gamecode) %>%
    arrange(round, codigo) %>%
    filter(codigo == gamecode) %>%
    pull(jornada)


  raw_teams <- httr::GET(url, query = list()) %>%
    httr::content()



  tm <- purrr::pluck(raw_teams, "Stats") %>%
    dplyr::tibble(value = .) %>%
    tidyr::unnest_wider(value) %>%
    select(Team)


  df <- purrr::pluck(raw_teams, "Stats", 1, "PlayersStats") %>%
    dplyr::tibble(value = .) %>%
    tidyr::unnest_wider(value) %>%
    mutate(
      team_name = tm$Team[1],
      opp_team_name = tm$Team[2],
      id_match = gamecode
    )


  df1 <- purrr::pluck(teams_enbruto, "Stats", 2, "PlayersStats") %>%
    dplyr::tibble(value = .) %>%
    tidyr::unnest_wider(value) %>%
    mutate(
      team_name = tm$Team[2],
      opp_team_name = tm$Team[1],
      id_match = gamecode
    )

  df2 <- rbind(df, df1) %>%
    select(id_match, Player_ID:opp_team_name) %>%
    janitor::clean_names() %>%
    mutate(
      isLeague = "euroleague",
      player_id = str_squish(player_id),
      ronda = round, .before = id_match
    )
}

boxscores_df <- map_df(gamecode, boxscores_fn)

glimpse(boxscores_df)

#######################################################################################################################################################
################################ libreria euroliga ####################################################################################################
#######################################################################################################################################################


library(euroleaguer)

bx <- getGameBoxScore(season_code = "E2025", game_code = 4)
players <- bx$PlayerStats # boxscore por jugador
teams <- bx$TeamStats # boxscore por equipo


# obtener los códigos de partido de esa jornada
gms <- getCompetitionGames(season_code = "E2025", round = 1)
codes <- gms$GameCode

# descargar boxscores y apilar
bxs <- map(codes, ~ getGameBoxScore("E2025", .x))

players_round <- map2_dfr(codes, bxs, ~ .y$PlayerStats %>% mutate(game_code = .x))
teams_round <- map2_dfr(codes, bxs, ~ .y$TeamStats %>% mutate(game_code = .x))
