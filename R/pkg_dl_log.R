#' CRAN Mirror daily download log.
#'
#' @import magrittr
#' @import ggplot2
#' @import googlesheets
#' @importFrom lubridate today
#' @importFrom readr read_csv
#' @importFrom slackr slackr_setup
#' @importFrom slackr slackr_upload
#' @param path2token character.
#' @param path2conf character.
#' @examples 
#' \dontrun{
#' pkg_dl_log("googlesheets_token.rds", "slackr_setup")
#' }
pkg_dl_log <- function(path2token = "googlesheets_token.rds", path2conf = "slackr_setup") {
    if (!file.exists(path2token)) {
      stop("Can't find token file.", call. = FALSE)
    } else {
      if (!file.exists(path2conf)) {
        stop("Can't find slackr configure file.", call. = FALSE)
      } else {
        temp_dir <- tempdir()
        file.copy(from = path2token,
                  to   = paste(temp_dir,
                               path2token %>% gsub(".+/|.+/.+%", "", .),
                               sep = "/"))
        suppressMessages(googlesheets::gs_auth(
          token = paste(temp_dir,
                        path2token %>% gsub(".+/|.+/.+%", "", .),
                        sep = "/"),
          verbose = FALSE
        ))
        log_d <- lubridate::today(tzone = "UTC") - 1
        
        df_today <-
          readr::read_csv(
            paste0(
              "http://cran-logs.rstudio.com/",
              year(log_d),
              "/",
              log_d,
              ".csv.gz"
            ),
            col_types = cols_only(
              date      = col_date(),
              r_os      = col_character(),
              package   = col_character(),
              country   = col_character()
            )
          ) %>%
          dplyr::group_by(date, package) %>%
          dplyr::summarise(download = n()) %>%
          dplyr::arrange(-download) %>%
          dplyr::slice(1:100)
        
        googlesheets::gs_title("cran_pkg_dl_log") %>% googlesheets::gs_edit_cells(input = df_today)
        
        file.copy(from = path2conf,
                  to   = paste(temp_dir,
                               path2conf %>% gsub(".+/|.+/.+%", "", .),
                               sep = "/"))
        slackr::slackr_setup(config_file = paste(temp_dir,
                                                 path2conf %>% gsub(".+/|.+/.+%", "", .),
                                                 sep = "/"))
        
        df_today %>% head(10) %>%
          ggplot(aes(reorder(package, download), download, fill = factor(package))) +
          geom_bar(stat = "identity", alpha = 0.8) +
          guides(fill = FALSE) +
          labs(x = "Package") +
          coord_flip()
        ggsave(paste(
          temp_dir,
          paste0("todays_cran_package_dl_", log_d, ".png"),
          sep = "/"
        ))
        slackr::slackr_upload(filename = paste(
          temp_dir,
          paste0("todays_cran_package_dl_", log_d, ".png"),
          sep = "/"
        ),
        title    = log_d)
      }
    }
  }
