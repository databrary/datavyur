test_that("to_csv() rejects bad input parameters", {
  expect_error(to_csv(dv_dir = -1))
  expect_error(to_csv(dv_dir = list(a=1, b=2)))
  expect_error(to_csv(in_dir = TRUE))
  
  expect_error(to_csv(dv_fn = -1))
  expect_error(to_csv(dv_fn = list(a=1, b=2)))
  expect_error(to_csv(dv_fn = TRUE))
  
  expect_error(to_csv(out_fn = -1))
  expect_error(to_csv(out_fn = list(a=1, b=2)))
  expect_error(to_csv(out_fn = TRUE))
  
  expect_error(to_csv(auto_write_over = -1))
  expect_error(to_csv(auto_write_over = "a"))
  expect_error(to_csv(auto_write_over = list(a=1, b=2)))
  
  expect_error(to_csv(code_regex = -1))
  expect_error(to_csv(code_regex = list(a=1, b=2)))
  expect_error(to_csv(code_regex = TRUE))
  
  expect_error(to_csv(code_vals_regex = -1))
  expect_error(to_csv(code_vals_regex = list(a=1, b=2)))
  expect_error(to_csv(code_vals_regex = TRUE))
  
  expect_error(to_csv(code_type_regex = -1))
  expect_error(to_csv(code_type_regex = list(a=1, b=2)))
  expect_error(to_csv(code_type_regex = TRUE))

  expect_error(to_csv(time_regex = -1))
  expect_error(to_csv(time_regex = list(a=1, b=2)))
  expect_error(to_csv(time_regex = TRUE))
  
  expect_error(to_csv(convert_times = -1))
  expect_error(to_csv(convert_times = "a"))
  expect_error(to_csv(convert_times = list(a=1, b=2)))

  expect_error(to_csv(vb = -1))
  expect_error(to_csv(vb = "a"))
  expect_error(to_csv(vb = list(a=1, b=2)))
})

# Disable until better error checking
# test_that("to_csv returns character or NULL", {
#   expect_true("character" %in% class(to_csv()) ||
#                 is.null(to_csv()))
# })
