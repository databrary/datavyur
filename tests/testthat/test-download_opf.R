test_that("download_opf rejects bad input parameters", {
  expect_error(download_opf(session_id = -1))
  expect_error(download_opf(session_id = "a"))
  expect_error(download_opf(session_id = list(a = 1, b = 2)))
  expect_error(download_opf(session_id = TRUE))
  
  expect_error(download_opf(asset_id = -1))
  expect_error(download_opf(asset_id = "a"))
  expect_error(download_opf(asset_id = list(a = 1, b = 2)))
  
  expect_error(download_opf(file_name = -1))
  expect_error(download_opf(file_name = TRUE))
  expect_error(download_opf(file_name = list(a = 1, b = 2)))
  
  expect_error(download_opf(vb = -1))
  expect_error(download_opf(vb = "a"))
  expect_error(download_opf(vb = list(a = 1, b = 2)))
})

test_that("download_opf returns character or NULL", {
  expect_true("character" %in% class(download_opf()) ||
                is.null(download_opf()))
})