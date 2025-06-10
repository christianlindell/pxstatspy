test_that("PxAPI instantiates", {
  api <- PxAPI$new("http://example.com/v2")
  expect_true(inherits(api, "PxAPI"))
})
