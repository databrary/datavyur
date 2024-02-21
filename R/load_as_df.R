#' Load A Raw Datavyu File as a Data Frame
#'
#' @param dv_fn A character string. The full file name for the Datavyu file to
#' load. Default is the value returned by `datavyur_example()`.
#' @param vb A logical value. Show verbose output. Default is FALSE.
#'
#'
#' @export
load_as_df <- function(dv_fn = datavyur_example(),
                       vb = FALSE) {
  assertthat::assert_that(length(dv_fn) == 1)
  assertthat::is.string(dv_fn)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  extract_opf(dv_fn, vb = vb) %>%
    to_csv(vb = vb) %>%
    readr::read_csv(show_col_types = FALSE,
                    col_types = readr::cols(.default = "c")) %>%
    dplyr::rename("code_value" = "code.value")
}