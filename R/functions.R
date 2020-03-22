#' Read files of different format
#'
#' Technically this returns the data frame of the uploaded file of various format(csv, xlsx, txt).
#' To be noted that this function was firstly created by Krzysztof Bajdzienko(https://github.com/kbajdzienko).
#' 
#' @return A data frame
#' @export
#' @seealso [readxl::reax_xlsx],[readr::read_csv] which this function wraps.
#' @examples
#' read_delimKB(path)

read_delimKB <- function(path){
  
  require(stringr)
  require(dplyr)
  require(readxl)
  require(readr)
  
  type = str_extract(path, 'csv$|xlsx$|txt$')
  
  df_raw <- switch(type,
                   txt = read_delim(path, col_names = FALSE, delim = '\t'),
                   csv = read_delim(path, col_names = FALSE, delim = ','),
                   xlsx = read_xlsx(path, col_names = FALSE))
  
  
  x <- slice(df_raw, 1) %>% 
    str_to_lower() %>%
    enc2utf8() %>%
    str_replace_all('[:space:]','_') %>%
    str_replace_all('[:punct:]','_') %>%
    str_replace_all('__{1,}','_') %>%
    str_replace_all('_{1,}$','')
  
  df <- slice(df_raw, -1) %>% as_tibble()
  colnames(df) <- x
  # browser()
  df = replace(df, is.na(df),"NA")
  return(df)
}