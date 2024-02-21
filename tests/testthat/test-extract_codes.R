#  ---------------------------------------------------
test_that("extract_codes rejects bad input parameters", {
  expect_error(extract_codes(in_dir = -1))
  expect_error(extract_codes(in_dir = list(a=1, b=2)))
  expect_error(extract_codes(in_dir = TRUE))
  
  expect_error(extract_codes(dv_fn = -1))
  expect_error(extract_codes(dv_fn = list(a=1, b=2)))
  expect_error(extract_codes(dv_fn = TRUE))
  
  expect_error(extract_codes(out_dir = -1))
  expect_error(extract_codes(out_dir = list(a=1, b=2)))
  expect_error(extract_codes(out_dir = TRUE))
  
  expect_error(extract_codes(out_fn = -1))
  expect_error(extract_codes(out_fn = list(a=1, b=2)))
  expect_error(extract_codes(out_fn = TRUE))
  
  expect_error(extract_codes(auto_write_over = -1))
  expect_error(extract_codes(auto_write_over = "a"))
  expect_error(extract_codes(auto_write_over = list(a=1, b=2)))
  
  expect_error(extract_codes(code_regex = -1))
  expect_error(extract_codes(code_regex = list(a=1, b=2)))
  expect_error(extract_codes(code_regex = TRUE))
  
  expect_error(extract_codes(code_vals_regex = -1))
  expect_error(extract_codes(code_vals_regex = list(a=1, b=2)))
  expect_error(extract_codes(code_vals_regex = TRUE))
  
  expect_error(extract_codes(code_type_regex = -1))
  expect_error(extract_codes(code_type_regex = list(a=1, b=2)))
  expect_error(extract_codes(code_type_regex = TRUE))
  
  expect_error(extract_codes(vb = -1))
  expect_error(extract_codes(vb = "a"))
  expect_error(extract_codes(vb = list(a=1, b=2)))
})

# test_that("extract_codes returns character or NULL", {
#   expect_true("character" %in% class(extract_codes()) ||
#                 is.null(extract_codes()))
# })
