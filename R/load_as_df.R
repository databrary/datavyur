#' Load A Raw Datavyu File as a Data Frame
#' 
#' @param df_fn A character string. The full file name for the Datavyu file to
#' load. Default is the value returned by `datavyur_example()`.
#' @param vb A logical value. Show verbose output. Default is FALSE.
#' 
#' 
#' @export
load_as_df <- function(dv_fn = datavyur_example(), 
                       vb = FALSE) {
  
  extracted_csv <- extract_opf(vb = vb) %>%
    to_csv(vb = vb)
    readr::read_csv(show_col_types = FALSE)
}