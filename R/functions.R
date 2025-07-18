rich_weighted_average <- function(tbbl) {
  tbbl %>%
    mutate(weighted_score = composite_score * prop) %>%
    summarize(weighted_average = sum(weighted_score, na.rm = TRUE)) %>%
    pull(weighted_average)
}
rich_cagr <- function(tbbl) {
  start_value <- tbbl$weighted_average[1]
  end_value <- tbbl$weighted_average[nrow(tbbl)]
  years <- nrow(tbbl) - 1
  cagr_value <- ((end_value / start_value)^(1 / years)) - 1
  return(cagr_value)
}
rich_last <- function(tbbl) {
  tbbl$weighted_average[tbbl$year == max(tbbl$year)]
}
