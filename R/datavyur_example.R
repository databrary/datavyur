#' Load Sample Datavyu File
#' 
#' @param path A character string. The file path to the sample Datavyu file
#' distributed with the datavyur package. Default is ""9807_117035.opf", a 
#' Datavyu file from volume 1, session 9807, with asset_id 117035.
#' 
#' @returns The full file path to the file.
#' 
#' @examples
#' datavyur_example() # Shows path to sample Datavyu file.
#' 
#' @export 
datavyur_example <- function(path = "9807_117035.opf") {
  if (is.null(path)) {
    dir(system.file("extdata", package = "datavyur"))
  } else {
    system.file("extdata", path, package = "datavyur", mustWork = TRUE)
  }
}