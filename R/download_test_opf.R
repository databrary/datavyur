#' Download Test Datavyu File from Databrary
#'
#' @param session_id An integer. The unique identifier for a Databrary session
#' that contains a Datavyu file. Default is 9807.
#' @param asset_id. An integer. The unique identifier for the Datavyu file in
#' the Databrary session. Default is 117035.
#' @param file_name. A string. The name of the file to be downloaded. The
#' default is to generate a temporary file name using
#' `tempfile(fileext = ".opf").
#' @param vb. A logical value. Provide verbose information at the R console.
#' Default is FALSE.
#'@param rq. An `httr2` request object. Default is NULL. This generates a new
#'default request using `databraryr::make_default_request()`. If you are
#'accessing Datavyu files from a volume or session accessible only to authorized
#'users, you should authenticate to Databrary and use the request associated
#'with that authentication.
download_test_opf <- function(session_id = 9807,
                              asset_id = 117035,
                              file_name = tempfile(pattern = paste0("databrary_", session_id, "_", asset_id, "_"),
                                                   fileext = ".opf"),
                              vb = FALSE,
                              rq = NULL) {
  # Parameter checking ---------------------------------------------------------
  assertthat::is.number(session_id)
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(session_id > 0)
  
  assertthat::is.number(asset_id)
  assertthat::assert_that(length(asset_id) == 1)
  assertthat::assert_that(asset_id > 0)
  
  assertthat::is.string(file_name)
  assertthat::assert_that(length(file_name) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(file_name) == 1)
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  databraryr::download_session_asset(asset_id,
                                     session_id,
                                     file_name,
                                     vb = vb,
                                     rq = rq)
}
