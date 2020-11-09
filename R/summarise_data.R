source("R/packages.R", local = TRUE)


#' Summarise PBP file into key metrics per player per game style
#'
#' @param tib Raw pbp tibble or dataframe
#'
#' @return Summarised tibble
#'
#' @examples
summarise_pbp <- function(tib) {
  
  break_points <- c("0-40", "15-40", "30-40", "40-A")
  short_rally <- c("4", "3", "2")
  long_rally <- c("9+", "20+")
  
  return(
    tib %>% filter(is.na(`Player Game Style`) == FALSE) %>% group_by(
      Player, 
      `Player Game Style`
    ) %>% summarise(
      matches = length(unique(`Match ID`)),
      serve_total = sum(if_else(Serving == 1, 1, 0)),
      serve_1_in = sum(if_else(Serving == 1 & Serve == "First Serve In", 1, 0)),
      serve_1_win = sum(if_else(Serving == 1 & Serve == "First Serve In" & PointWon == 1, 1, 0)),
      serve_2_win = sum(if_else(Serving == 1 & Serve == "Second Serve In" & PointWon == 1, 1, 0)),
      return_total = sum(if_else(Serving == 0, 1, 0)),
      return_1_win = sum(if_else(Serving == 0 & Serve == "First Serve In" & PointWon == 1, 1, 0)),
      return_2_win = sum(if_else(Serving == 0 & Serve == "Second Serve In" & PointWon == 1, 1, 0)),
      break_point_total = sum(if_else(Serving == 0 & Scoreline %in% break_points, 1, 0)),
      break_point_win = sum(if_else(Serving == 0 & Scoreline %in% break_points & PointWon == 1, 1, 0)),
      shots_total = sum(na.omit(if_else(Serving == 1, ceiling(`Shot Count` / 2) - 1, floor(`Shot Count` / 2)))),
      winners_total = sum(if_else(FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                    str_detect(finish, "Winner"), 1, 0)),
      forced_errors_total = sum(if_else(FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                          finish == "Forced Error", 1, 0)),
      unforced_errors_total = sum(if_else(FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                            finish == "Unforced Error", 1, 0)),
      return_errors_total = sum(if_else(FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                          finish == "Return Error", 1, 0)),
      short_rally_total = sum(if_else(`Rally Length` %in% short_rally | is.na(`Rally Length`), 1, 0)),
      medium_rally_total = sum(if_else(`Rally Length` == "5 to 8", 1, 0), na.rm = TRUE),
      long_rally_total = sum(if_else(`Rally Length` %in% long_rally, 1, 0)),
      short_rally_win = sum(if_else((`Rally Length` %in% short_rally | is.na(`Rally Length`)) & 
                                      PointWon == 1, 1, 0)),
      medium_rally_win = sum(if_else(`Rally Length` == "5 to 8" & PointWon == 1, 1, 0), na.rm = TRUE),
      long_rally_win = sum(if_else(`Rally Length` %in% long_rally & PointWon == 1, 1, 0)),
      approaches_total = sum(if_else(is.na(`Player Net Approach`) == FALSE, 1, 0)),
      intentional_approaches_total = sum(if_else(str_detect(`Player Net Approach`, "Intentional"), 1, 0), 
                                         na.rm = TRUE),
      reactive_approaches_total = sum(if_else(str_detect(`Player Net Approach`, "Reactive"), 1, 0), 
                                      na.rm = TRUE),
      approaches_win = sum(if_else(is.na(`Player Net Approach`) == FALSE & PointWon == 1, 1, 0))
    ) %>% mutate(
      serve_1_in_pct = serve_1_in / serve_total,
      serve_1_win_pct = serve_1_win / serve_1_in,
      serve_2_win_pct = serve_2_win / (serve_total - serve_1_in)
      ## TODO: Calculate rest of proportions
    )
  )
  
}

#' Summarise SBS file into key metrics per player per game style
#'
#' @param tib Raw sbs tibble or dataframe
#'
#' @return Summarised tibble
#'
#' @examples
summarise_sbs <- function(tib) {
  
  ## TODO
  
}