
#' Automated Data Extraction Functions for Saudi Aramco Prospectus
#' Tidy PDF Scraping and Fuzzy Joining Helper
#' @param pdf.file
#' @param page
#' @param names
#'
#' @return
#' @export
#'
#' @examples
extract.values.f <- function(pdf.file, page, names){
  require(tabulizer)
  require(fuzzyjoin) # regex_inner_join()

  # PDF: https://www.saudiaramco.com/-/media/images/investors/saudi-aramco-prospectus-en.pdf
  # Locate table areas
  area = case_when(
    page == 220  ~ c(459.77, 69.76, 601, 427.98), # Table 42 (pg 131)
    page == 221  ~ c(168.03, 69.76, 394.53, 404.59), # Table 43 (pg 132)
    page == 222  ~ c(180.11, 68.38, 413.04, 412.05), # Table 45 (pg 133)
    page == 233  ~ c(181.57, 70.99, 673.96, 448.91) # Table 52 (pg 144)
  )

  # Extract the tables
  tmp <- extract_tables(
    pdf.file,
    pages  = page,
    area   = list(area),
    guess  = FALSE,
    output = "data.frame"
  )

  tmp %>%
    purrr::pluck(1) %>%
    map_dfc(~trimws(gsub("\\.|[[:punct:]]", "", .x))) %>%
    set_names( c("Heading", paste0("X", if(page==233){1:4}else{0:4})) ) %>%
    regex_inner_join(
      data.frame(regex_name = names, stringsAsFactors = FALSE),
      by = c(Heading = "regex_name")
    ) %>%
    select(X4) %>%
    pull() %>%
    as.numeric()
}
