test_that("rejects bad input parameters", {
  expect_error(extract_code_defs(in_dir = -1))
  expect_error(extract_code_defs(in_dir = 0))
  #  expect_error(extract_code_defs(in_dir = list(a=1, b=2)))
  expect_error(extract_code_defs(in_dir = TRUE))

  expect_error(extract_code_defs(dv_fn = -1))
  expect_error(extract_code_defs(dv_fn = 0))
  #  expect_error(extract_code_defs(dv_fn = list(a=1, b=2)))
  expect_error(extract_code_defs(dv_fn = TRUE))

  expect_error(extract_code_defs(out_dir = -1))
  expect_error(extract_code_defs(out_dir = 0))
  #  expect_error(extract_code_defs(out_dir = list(a=1, b=2)))
  expect_error(extract_code_defs(out_dir = TRUE))

  expect_error(extract_code_defs(out_fn = -1))
  expect_error(extract_code_defs(out_fn = 0))
  #  expect_error(extract_code_defs(out_fn = list(a=1, b=2)))
  expect_error(extract_code_defs(out_fn = TRUE))

  expect_error(extract_code_defs(auto_write_over = -1))
  expect_error(extract_code_defs(auto_write_over = 3))
  expect_error(extract_code_defs(auto_write_over = "a"))
  #  expect_error(extract_code_defs(auto_write_over = list(a=1, b=2)))

  expect_error(extract_code_defs(code_regex = -1))
  expect_error(extract_code_defs(code_regex = 0))
  #  expect_error(extract_code_defs(code_regex = list(a=1, b=2)))
  expect_error(extract_code_defs(code_regex = TRUE))

  expect_error(extract_code_defs(code_vals_regex = -1))
  expect_error(extract_code_defs(code_vals_regex = 0))
  #  expect_error(extract_code_defs(code_vals_regex = list(a=1, b=2)))
  expect_error(extract_code_defs(code_vals_regex = TRUE))

  expect_error(extract_code_defs(code_type_regex = -1))
  expect_error(extract_code_defs(code_type_regex = 0))
  #  expect_error(extract_code_defs(code_type_regex = list(a=1, b=2)))
  expect_error(extract_code_defs(code_type_regex = TRUE))

  expect_error(extract_code_defs(vb = -1))
  expect_error(extract_code_defs(vb = 3))
  expect_error(extract_code_defs(vb = "a"))
  #  expect_error(extract_code_defs(vb = list(a=1, b=2)))
})
