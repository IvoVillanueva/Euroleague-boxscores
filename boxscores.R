#######################################################################################################################################################
################################ LIBRERIAS   ##########################################################################################################
#######################################################################################################################################################


library(tidyverse)
library(jsonlite)
library(httr)

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

