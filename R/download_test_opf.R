download_test_opf <- function(session_id = 9807, asset_id = 117035,
                              file_name = tempfile(fileext = ".opf"),
                              vb = TRUE) {

  stopifnot(as.numeric(session_id) > 0)
  stopifnot(as.numeric(asset_id) > 0)
  stopifnot(is.character(file_name))
  stopifnot(is.logical(vb))

  url <- paste0("https://nyu.databrary.org",
                         paste("/slot", session_id, "-", "asset", asset_id,
                               "download", sep="/"))

  if (vb) message("Making request to ", url)
  r <- httr::GET(url)
  if (httr::status_code(r) == 200) {
    content.type = r$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query.")
      message(paste0("Content-type is '", content.type, "'."))
    }
    if (content.type == "application/vnd.datavyu") {
      bin <- httr::content(r, 'raw')
      writeBin(bin, file_name)
      return(file_name)
    }
  } else {
    if (vb) message(paste0('Download Failed, HTTP status ', webpage$status_code))
    return(NULL)
  }
}
