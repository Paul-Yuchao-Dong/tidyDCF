## code to prepare `prospectus` dataset goes here

#' Download Helper - Creates a Data folder
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
download.f <- function(url) {
  data.folder = 'data'  # setup temp folder
  if (!dir.exists(data.folder)){dir.create(data.folder, F)}
  filename = file.path(data.folder, basename(url))
  if(!file.exists(filename))
    tryCatch({ download.file(url, filename, mode='wb') },
             error = function(ex) cat('', file=filename))
  message(paste0('File located at: ', filename))
  filename
}

url <- 'https://www.saudiaramco.com/-/media/images/investors/saudi-aramco-prospectus-en.pdf'

prospectus.pdf <- download.f(url)

# usethis::use_data(prospectus, overwrite = TRUE)
