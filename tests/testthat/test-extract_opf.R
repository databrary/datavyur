# extract_opf ---------------------------------------------------
test_that("rejects bad input parameters", {
  expect_error(extract_opf(in_dir = -1))
  expect_error(extract_opf(in_dir = 0))
  expect_error(extract_opf(in_dir = list(a=1, b=2)))
  expect_error(extract_opf(in_dir = TRUE))

  expect_error(extract_opf(in_fn = -1))
  expect_error(extract_opf(in_fn = 0))
  expect_error(extract_opf(in_fn = list(a=1, b=2)))
  expect_error(extract_opf(in_fn = TRUE))

  expect_error(extract_opf(out_dir = -1))
  expect_error(extract_opf(out_dir = 0))
  expect_error(extract_opf(out_dir = list(a=1, b=2)))
  expect_error(extract_opf(out_dir = TRUE))

  expect_error(extract_opf(auto_write_over = -1))
  expect_error(extract_opf(auto_write_over = 3))
  expect_error(extract_opf(auto_write_over = "a"))
  expect_error(extract_opf(auto_write_over = list(a=1, b=2)))

  expect_error(extract_opf(vb = -1))
  expect_error(extract_opf(vb = 3))
  expect_error(extract_opf(vb = "a"))
  expect_error(extract_opf(vb = list(a=1, b=2)))
})

# test_that("extract_opf returns character or NULL", {
#   expect_true("character" %in% class(extract_opf()) ||
#                 is.null(extract_opf()))
# })
