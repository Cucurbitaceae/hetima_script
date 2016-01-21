#' Notice new released r-packages.
#'
#' @import Hmisc
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom gmailr gmail_auth
#' @importFrom gmailr mime
#' @importFrom gmailr send_message
#' @importFrom knitr kable
#' @importFrom lubridate today
#' @importFrom rvest html_node
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
#' @param path2auth path to .httr-oauth 
#' @param path2gmailr_json path to google auth json file
#' @param address2sent character. 
#' @param address_from character. ex. Uryu Shinya <suika1127@gmail.com>
#' @name notice_new_rpkg
#' @examples 
#' \dontrun{
#' notice_new_rpkg(".httr-oauth", 
#'                 "auth_gmailr.json", 
#'                 "u_suika.30700@m.evernote.com", "
#'                  Uryu Shinya <suika1127@gmail.com>")
#' }
#' @export
notice_new_rpkg <- function(path2auth        = ".httr-oauth",
                            path2gmailr_json = "auth_gmailr.json",
                            address2sent     = "<address_to_sent>",
                            address_from     = "<address_from>") {
  # Flag) Exit authorize files?
  if (!file.exists(path2auth)) {
    stop("Try gmail_auth() to ciliate your .httr-oauth file.", call. = FALSE)
  } else {
    if (!file.exists(path2gmailr_json)) {
      stop("Can't find secret_file. Please, check secret_file path.",
           call. = FALSE)
      
    } else {
      temp_dir <- tempdir()
      file.copy(from = path2gmailr_json,
                to   = paste(
                  temp_dir,
                  path2gmailr_json %>% gsub(".+/|.+/.+%", "", .),
                  sep = "/"
                ))
      file.copy(from = path2auth,
                to = paste(temp_dir, path2auth %>% gsub(".+/|.+/.+%", "", .), sep = "/"))
      gmailr::gmail_auth(
        secret_file = paste(
          temp_dir,
          path2gmailr_json %>% gsub(".+/|.+/.+%", "", .),
          sep = "/"
        ),
        scope = "compose"
      )
      
      available_list <-
        installed.packages() %>% as.data.frame(stringsAsFactors = FALSE) %$% Package
      df_new_pkg <-
        xml2::read_html(
          "https://cran.r-project.org/web/packages/available_packages_by_date.html"
        ) %>%
        rvest::html_node(., css = "table") %>%
        rvest::html_table() %>%
        dplyr::filter(
          Date == lubridate::today(tzone = "Asia/Tokyo") - 1,
          Hmisc::`%nin%`(Package, available_list)
        )
      
      if (!nrow(df_new_pkg) == 0) {
        gmailr::mime(
          To      = address2sent,
          From    = address_from,
          attr    = list(
            content_type = "text/html",
            charset = "utf-8",
            encoding = "base64"
          ),
          Subject = paste0(
            "There are new released R Packages! (",
            lubridate::today(tzone = "Asia/Tokyo"),
            ") #R: package"
          ),
          body    = paste(
            knitr::kable(df_new_pkg, format = "html"),
            paste0(
              "install.packages(pkgs = ",
              deparse(df_new_pkg$Package, control = "all", width.cutoff = 500L) ,
              ")"
            ),
            sep = "<br>"
          )
        ) %>%
          gmailr::send_message()
        install.packages(pkgs = df_new_pkg$Package)
        message("Success :)")
      } else {
        message("There are not find will be install packages.")
      }
    }
  }
}
