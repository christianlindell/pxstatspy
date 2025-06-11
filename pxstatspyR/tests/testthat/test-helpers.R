test_that("PxVariables constants exist", {
  expect_equal(PxVariables$TIME, "Tid")
  expect_true("region" %in% PxVariables$REGION_ALTERNATIVES)
})

test_that("Selection matcher works", {
  codes <- as.character(2018:2023)
  expect_equal(count_matching_values("TOP(2)", codes), 2)
  expect_equal(expand_expression("RANGE(2019,2021)", codes), c("2019","2020","2021"))
  expect_equal(sort(get_matching_codes(c("2019", "202*"), codes)), c("2019","2020","2021","2022","2023"))
})
