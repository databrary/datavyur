#' Download Datavyu File from Databrary
#'
#' @param session_id An integer. The unique identifier for a Databrary session
#' that contains a Datavyu file. Default value is 9807.
#' @param asset_id An integer. The unique identifier for the Datavyu file in
#' the Databrary session. Default is 117035.
#' @param file_name A string. The name of the file to be downloaded. Default 
#' is NULL. If the file name is NULL a default one is generated.
#' @param auto_write_over A logical value. Overwrite existing file or directory
#' with the same name. Default is TRUE.
#' @param vb A logical value. Provide verbose information at the R console.
#' Default is FALSE.
#' @param rq An `httr2` request object. Default is NULL. This generates a new 
#' default request using `databraryr::make_default_request()`. If you are 
#' accessing Datavyu files from a volume or session accessible only to 
#' authorized users, you should authenticate to Databrary and use the httr2 
#' request associated with that authentication.
#'
#' @returns A string with the full file name of the downloaded file.
#'
#' @examples
#' \donttest{
#' \dontrun{
#' download_opf() # Downloads a test Datavyu file from Databrary volume 1.
#' }
#' }
#' 
#' @export
download_opf <- function(session_id = 9807,
                         asset_id = 117035,
                         file_name = NULL,
                         auto_write_over = TRUE,
                              vb = FALSE,
                              rq = NULL) {
  # Parameter checking ---------------------------------------------------------
  assertthat::is.number(session_id)
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(session_id > 0)
  
  assertthat::is.number(asset_id)
  assertthat::assert_that(length(asset_id) == 1)
  assertthat::assert_that(asset_id > 0)
  
  if (is.null(file_name)) {
    if (vb) message("NULL file name.")
    file_dir <- tempdir()
    full_dir <- file.path(file_dir, session_id)
    if (!dir.exists(full_dir)) {
      if (vb) message("Generating default directory: ", full_dir)
      dir.create(file.path(full_dir))
    } else {
      if (vb) message("Directory exists: ", full_dir)
      if (auto_write_over) {
        if (vb) message("Will overwrite.")       
      } else {
        if (vb) message("auto_write_over is FALSE. Exiting.")
        return(NULL)
      }
    }
    file_name <- file.path(full_dir, paste0(asset_id, ".opf"))
  }
  assertthat::is.readable(dirname(file_name))
  if (vb) message("Full file path: ", file_name)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  databraryr::download_session_asset(asset_id = asset_id,
                                     session_id = session_id,
                                     file_name = file_name,
                                     vb = vb,
                                     rq = rq)
}
