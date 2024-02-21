#' Extracts Elements From Datavyu coding file.
#'

#' @param in_fn A character string string. The full file name for the Datavyu 
#' file. Default is NULL.
#' @param out_dir A character string. Output directory to save exported files.
#' @param create_dir A logical value. Create a new directory. Default is FALSE.
#' @param auto_write_over A logical value. If TRUE, any existing files in 
#' out_dir are overwritten. Default is TRUE.
#' @param vb A logical value. Provide verbose output. Default is FALSE.
#' 
#' @returns The output directory where the extracted files were saved.
#' 
#' @examples
#' \donttest{
#' extract_opf()
#' }
#' 
#' @export
extract_opf <- function(in_fn = NULL,
                        out_dir = tempdir(),
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
      if (vb) message("Copying file to new output directory.")
      file.copy(from = in_fn, to = file.path(out_dir, basename(in_fn)))
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
  
  if (!file.exists(file.path(out_dir, basename(in_fn)))) {
    if (vb) {
      message("File ", basename(in_fn), " not found in ", out_dir)
      message("Copying file.")
    }
    file.copy(from = in_fn, to = file.path(out_dir, basename(in_fn)))
  }

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
