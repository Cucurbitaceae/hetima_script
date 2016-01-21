#' Google Analytics daily report.
#' 
#' @importFrom dplyr bind_cols
#' @importFrom dplyr filter
#' @importFrom lubridate today
#' @importFrom RGA authorize
#' @importFrom RGA get_ga
#' @importFrom RGA list_profiles
#' @param track_url character. ex. http://qiita.com
#' @param auto_save logical.
#' @param ... other function arguments. 
#' @name ga_report
#' @export
ga_report <- function(track_url = NULL, auto_save = TRUE) {
  
  RGA::authorize()
  df_analytics <- RGA::list_profiles() %>%
    dplyr::filter(website.url == track_url) %$%
    RGA::get_ga(
      profile.id = id,
      start.date = lubridate::today(tz = "Asia/Tokyo") - 6,
      end.date   = lubridate::today(tz = "Asia/Tokyo"),
      metrics    = "ga:sessions",
      dimensions = "ga:day"
    ) %>%
    dplyr::bind_cols(data.frame(date = seq(
      lubridate::today(tz = "Asia/Tokyo") - 6,
      lubridate::today(tz = "Asia/Tokyo"),
      by = 1
    )))
  p <- df_analytics %>% ggplot(aes(date, sessions, group = 1)) +
    geom_line() +
    geom_point(size = 2, colour = "magenta")
  
  if (!auto_save == TRUE) {
    print(p)
  } else {
    temp_dir <- tempdir()
    print(p)
  }
  ggsave(paste(
    temp_dir,
    paste0("google_analytics_report", lubridate::today(tz = "Asia/Tokyo"), ".png"),
    sep = "/"
  ), width = 7, height = 3)
  rdrop2::drop_upload(file = paste(
    temp_dir,
    paste0("google_analytics_report", lubridate::today(tz = "Asia/Tokyo"), ".png"),
    sep = "/"
  ),
  overwrite = TRUE)
  closeAllConnections()
}
