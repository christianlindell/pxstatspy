test_that("PxAPI instantiates and exposes methods", {
  api <- PxAPI$new("http://example.com/v2")
  expect_true(inherits(api, "PxAPI"))
  expect_true(is.function(api$get_config))
  expect_true(is.function(api$get_navigation_root))
  expect_true(is.function(api$get_table_metadata))
  expect_true(is.function(api$get_table_data))
})
