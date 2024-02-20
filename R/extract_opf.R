#' Extracts Elements From Datavyu coding file.
#'
#' @param in_dir A character string. The input directory for a 
#' Datavyu file.
#' @param in_fn File name for the Datavyu file.
#' @param out_dir Output directory to save exported files.
#' @param auto_write_over A Boolean value. If TRUE, any existing files in out_dir are overwritten.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return The output directory where the extracted files were saved.
#' @examples
#' extract_opf()
#' @export
extract_opf <- function(in_fn = NULL,
                        out_dir = dirname(in_fn),
                        create_dir = FALSE,
                        auto_write_over = TRUE,
                        vb = FALSE) {
  
  # Parameter checking -----------------------------------------------------------------
  assertthat::is.string(in_fn)
  assertthat::assert_that(length(in_fn) == 1)
  assertthat::is.readable(in_fn)
  if (!(tools::file_ext(in_fn) == "opf"))
    stop(paste0("File ", in_fn, " does not have Datavyu (.opf) extension."))
  
  assertthat::is.string(out_dir)
  assertthat::assert_that(length(out_dir) == 1)
  if (!dir.exists(out_dir)) {
    if (create_dir) {
      if (vb)
        message(paste0("Creating directory ", out_dir, "."))
      dir.create(out_dir)      
    } else {
      if (vb) message("Unable to create output directory.")
     return(NULL) 
    }
  }
  assertthat::is.writeable(out_dir)
  
  assertthat::assert_that(is.logical(auto_write_over))
  assertthat::assert_that(length(auto_write_over) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)

  # Extract file and return ----------------------------------------------------
  # Note: Raw .opf file is actually a zip file.
  if (vb)
    message(paste0("Extracting file ", in_fn, " to ", out_dir))
  utils::unzip(in_fn, exdir = out_dir)
  
  # Return out_dir for chaining ------------------------------------------------
  if (vb)
    message(paste0("Success. File extracted to ", out_dir, "/."))
  return(out_dir)
}
