test_that("create_randomization_package works", {
  rat <- create_randomization_package(n_participants = 8,
                                      arm_labels = c("Treatment", "Control"),
                                      block_sizes = c(2, 4, 6, 8),
                                      allocation_ratio = c(1, 1),
                                      study_id = "EHS_PILOT",
                                      seed = NULL)
  expect_equal(length(rat), 6)
})
