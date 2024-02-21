#' Convert Extracted Datavyu File To CSV.
#'
#' @param dv_dir A character string. The directory to extract the Datavyu file.
#' Default value is the current directory ".".
#' @param dv_fn A character string. The name of the Datavyu code file.
#' Defaults to 'db'.
#' @param out_fn A character string. The name of the output CSV file. Default
#' 'tmp.csv' in the directory specified by `dv_dir`.
#' @param auto_write_over A logical value. If TRUE, new output file overwrites
#' the old. Default is FALSE.
#' @param code_regex A character string. A regular expression to extract codes
#' from a Datavyu file.
#' @param code_type_regex A character string. A regular expression to extract
#' code types from a Datavyu file.
#' @param time_regex A character string. A regular expression to extract
#' onset/offset times from a Datavyu file.
#' @param code_values_regex A character string. A regular expression to extract
#' code values from Datavyu file.
#' @param convert_times A logical value. If TRUE (default), converts the onset
#' and offset fields to lubridate-compatible dates and times. Default is TRUE.
#' @param vb A boolean value. If TRUE, provides verbose output. Default is
#' FALSE.
#'
#' @examples
#' \donttest{
#' to_csv()
#' }
#'
#' @export
to_csv <- function(dv_dir = ".",
                   dv_fn = "db",
                   out_fn = paste0(dv_dir, "/tmp.csv"),
                   auto_write_over = FALSE,
                   code_regex = "^([a-zA-Z_]+[0-9]*[a-zA-Z_]*[0-9]*)",
                   code_type_regex = "([a-zA-Z]+)$",
                   time_regex = "([0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{3})",
                   code_values_regex = "\\(([a-zA-Z ?,.'/0-9;!|~`]+)\\)$",
                   convert_times = TRUE,
                   vb = FALSE) {
  # Parameter checking -------------------------------------------------------------------
  assertthat::is.string(dv_dir)
  assertthat::assert_that(length(dv_dir) == 1)
  assertthat::is.readable(dv_dir)
  
  assertthat::is.string(dv_fn)
  assertthat::assert_that(length(dv_fn) == 1)
  
  assertthat::is.string(out_fn)
  assertthat::assert_that(length(out_fn) == 1)
  
  assertthat::assert_that(is.logical(auto_write_over))
  assertthat::assert_that(length(auto_write_over) == 1)
  
  assertthat::is.string(code_regex)
  assertthat::assert_that(length(code_regex) == 1)
  
  assertthat::is.string(code_type_regex)
  assertthat::assert_that(length(code_type_regex) == 1)
  
  assertthat::is.string(time_regex)
  assertthat::assert_that(length(time_regex) == 1)
  
  assertthat::is.string(code_values_regex)
  assertthat::assert_that(length(code_values_regex) == 1)
  
  assertthat::assert_that(is.logical(convert_times))
  assertthat::assert_that(length(convert_times) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  if (!is.character(dv_dir)) {
    stop("Datavyu directory must be a string.")
  }
  if (!dir.exists(dv_dir)) {
    stop(paste0("Directory ", dv_dir, " not found.\n"))
  }
  if (!is.character(dv_fn)) {
    stop("Datavyu file name must be a string.")
  }
  if (!is.character(out_fn)) {
    stop("Output file name must be a string.")
  }
  if (!is.logical(auto_write_over)) {
    stop("auto_write_over must be a logical/Boolean value.")
  }
  if (file.exists(paste0(dv_dir, "/", out_fn))) {
    if (!auto_write_over) {
      replace.out <-
        readline(prompt = paste0("Output file ", out_fn, " exists. Replace (y/n)?: "))
      if (replace.out %in% c('n', 'N')) {
        stop(paste0("File ", out_fn, "not altered."))
      }
      message(paste0("File ", out_fn, " will be replaced."))
    }
  }
  
  # Open Datavyu file and read------------------------------------------------------------
  con_in <- file(paste0(dv_dir, "/", dv_fn), "r")
  if (con_in == FALSE) {
    if (vb) message("Unable to open file ", dv_fn)
    return(NULL)
  }
  
  con_in <- file(paste0(dv_dir, "/", dv_fn), "r")
  if (!con_in) {
    stop(paste0("Unable to open file: ", dv_fn))
  }
  
  dv <- readLines(con_in)
  close(con_in)
  
  if (vb)
    message(paste0(length(dv), " lines read from file '", dv_fn, "'."))
  
  # Write output file---------------------------------------------------------------------
  opf_files <- list.files(dv_dir, pattern = "\\.opf$")
  if (identical(opf_files, character(0))) {
    message(paste0("No Datavyu file found in ", dv_dir))
    message("Creating unique filename.")
    out_fn <-
      paste0(dv_dir, "/", format(Sys.time(), "%F-%H%M-%S"), ".csv")
  } else {
    out_fn <-
      paste0(dv_dir, "/", tools::file_path_sans_ext(basename(opf_files)), ".csv")
  }
  con_out <- file(out_fn, "w")
  if (!con_out) {
    stop(paste0("Unable to open file: ", out_fn))
  }
  
  outlines <- 0
  
  writeLines("code,onset,offset,code.value", con_out)
  code <- "-"
  code_values <- "-,-"
  times <- "-,-"
  for (l in 1:length(dv)) {
    # If not a valid first column skip row
    if (!(stringr::str_detect(dv[l], code_regex)) &&
        !(stringr::str_detect(dv[l], time_regex))) {
      next
    }
    # If a valid code definition row
    if (stringr::str_detect(dv[l], code_regex)) {
      code <- stringr::str_extract(dv[l], code_regex)
      next
    }
    # If a valid code row, process
    if (stringr::str_detect(dv[l], time_regex)) {
      times <-
        stringr::str_extract(
          dv[l],
          '([0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{3},[0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{3})'
        )
      #if (vb) message(paste0('This line: ', dv[l]))
      #if (vb) message(paste0("Extracted times: ", times))
      if (convert_times) {
        # Change colon between ss:mmm to period
        times <-
          stringr::str_replace_all(times, pattern = ':([0-9]{3})',
                                   replacement = '\\.\\1')
      }
      if (stringr::str_detect(dv[l], code_values_regex)) {
        code_values <-
          paste0('"',
                 stringr::str_extract(dv[l], code_values_regex),
                 '"')
        code_values <-
          paste0('"', stringr::str_match(dv[l], code_values_regex)[2], '"')
      } else {
        code_values <- "-"
      }
    } else {
      times <- "-,-"
    }
    writeLines(paste(code, times, code_values, sep = ","), con = con_out)
    outlines <- outlines + 1
  }
  
  # Cleanup ------------------------------------------------------------------------------
  close(con_out)
  if (vb)
    message(paste0(outlines, " lines written to file: ", out_fn))
  return(out_fn)
}
