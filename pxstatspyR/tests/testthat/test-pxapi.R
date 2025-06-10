test_that("PxAPI instantiates and exposes methods", {
  api <- PxAPI$new("http://example.com/v2")
  expect_true(inherits(api, "PxAPI"))
  expect_true(is.function(api$get_config))
  expect_true(is.function(api$get_navigation_root))
  expect_true(is.function(api$get_table_metadata))
  expect_true(is.function(api$get_table_data))
  expect_true(is.function(api$get_data_as_dataframe))
})

test_that("RateLimiter and NavigationExplorer instantiate", {
  rl <- RateLimiter$new(2, 1)
  expect_equal(rl$max_calls, 2)

  api <- PxAPI$new("http://example.com/v2")
  nav <- NavigationExplorer$new(api)
  expect_true(inherits(nav, "NavigationExplorer"))
})
