#' Extract Codes From a Datavyu file.
#'
#' @param in_dir A character string. Input directory. Defaults to '.'
#' @param dv_fn A character string. Full Datavyu file name. Defaults to 'db'
#' @param out_dir A character string. Output directory. Defaults to `in_dir`.
#' @param out_fn A character string. Full output file name.
#' @param auto_write_over A logical value. If TRUE, new output file overwrites old.
#' @param code_regex A character string. Specifies the regular expression
#' to extract a code.
#' @param code_vals_regex A character string. Specifies the regular expression
#' to extract a code value.
#' @param code_type_regex A character string. Specifies the regular expression
#' for the code type.
#' @param vb A logical value. If TRUE, provides verbose output.
#'
#' @returns A character string with the name of the saved output file.
#'
#' @examples
#' \donttest{
#' \dontrun{
#' extract_codes()
#' }
#' }
#'
#' @export
extract_codes <- function(in_dir = '.',
                          dv_fn = 'db',
                          out_dir = in_dir,
                          out_fn = paste0(out_dir, "/", 'codes.csv'),
                          auto_write_over = TRUE,
                          code_regex = "^([a-zA-Z_]+[0-9]*[a-zA-Z_]*[0-9]*)",
                          code_vals_regex = "\\)-([a-zA-Z\\/]+)\\|",
                          code_type_regex = "([a-zA-Z]+)$",
                          vb = FALSE) {
  # Check parameters -----------------------------------------------------------------
  assertthat::assert_that(length(in_dir) == 1)
  assertthat::is.string(in_dir)
  
  assertthat::assert_that(length(dv_fn) == 1)
  assertthat::is.string(dv_fn)
  
  assertthat::assert_that(length(out_dir) == 1)
  assertthat::is.string(out_dir)
  
  assertthat::assert_that(length(out_fn) == 1)
  assertthat::is.string(out_fn)
  if (file.exists(out_fn)) {
    if (!auto_write_over) {
      replace.out <-
        readline(prompt = paste0("Output file ", out_fn, " exists. Replace (y/n)?: "))
      if (replace.out %in% c('n', 'N')) {
        stop(paste0("File ", out_fn, "not altered."))
      }
      if (vb)
        message(paste0("File ", out_fn, " will be replaced."))
    }
  }
  
  assertthat::assert_that(length(auto_write_over) == 1)
  assertthat::assert_that(is.logical(auto_write_over))
  
  assertthat::assert_that(length(code_regex) == 1)
  assertthat::is.string(code_regex)
  
  assertthat::assert_that(length(code_vals_regex) == 1)
  assertthat::is.string(code_vals_regex)
  
  assertthat::assert_that(length(code_type_regex) == 1)
  assertthat::is.string(code_type_regex)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  # Open Datavyu file ------------------------------------------------------------------
  dv_in_fn <- file.path(in_dir, dv_fn)
  assertthat::is.readable(dv_in_fn)
  con_in <- file(file.path(dv_in_fn, dv_fn), "r")
  if (!con_in) {
    stop(paste0("Unable to open file: ", dv_in_fn))
  }
  dv <- readLines(con_in)
  close(con_in)
  if (vb)
    message(paste0(length(dv), " lines read from file ", dv_in_fn))
  
  # Write output line by line ------------------------------------------------------------
  dv_fl <- list.files(in_dir, '\\.opf$', full.names = TRUE)
  if (is.null(dv_fl))
    stop("No Datavyu files in ", in_dir)
  if (length(dv_fl) > 1) {
    stop(paste0(
      'More than one Datavyu file in ',
      in_dir,
      ". Source undetermined."
    ))
  } else {
    out_fn <-
      paste0(out_dir,
             "/",
             tools::file_path_sans_ext(basename(dv_fl[1])),
             "-code-defs.csv")
  }
 
  con_out <- file(out_fn, "w")
  if (!con_out) {
    stop(paste0("Unable to open file: ", out_fn))
  }
  
  outlines <- 0
  writeLines("code,code_vals,code_type", con_out)
  for (l in 1:length(dv)) {
    if (stringr::str_detect(dv[l], code_regex)) {
      # stringr::str_match returns capture group(s) in column 2+
      code <- stringr::str_match(dv[l], code_regex)[2]
      code_vals <- stringr::str_match(dv[l], code_vals_regex)[2]
      code_type <- stringr::str_match(dv[l], code_type_regex)[2]
      writeLines(paste(code, code_vals, code_type, sep = ","), con = con_out)
      outlines <- outlines + 1
    }
  }
  
  # Cleanup -------------------------------------------------------------------------------
  close(con_out)
  if (vb)
    message(paste0(outlines, " lines written to file: ", out_fn))
  return(out_fn)
}

# #======================================================================================
# # Convenience function to return a data frame
# extract_dv_code_defs_df <- function(in_dir = '.',
#                                     dv_fn = 'db',
#                                     out_dir = in_dir,
#                                     out_fn = paste0(out_dir, "/", 'codes.csv'),
#                                     auto_write_over = TRUE,
#                                     code_regex = "^([a-zA-Z_]+[0-9]*[a-zA-Z_]*[0-9]*)",
#                                     code_vals_regex = "\\)-([a-zA-Z\\/]+)\\|",
#                                     code_type_regex = "([a-zA-Z]+)$",
#                                     vb = FALSE) {
#   dv_csv <- extract_dv_code_defs(
#     in_dir = in_dir,
#     dv_fn = dv_fn,
#     out_dir = out_dir,
#     out_fn = out_fn,
#     auto_write_over = TRUE,
#     vb = vb
#   )
#   read.csv(dv_csv)
# }
