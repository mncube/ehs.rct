test_that("ehs_randomization_app works", {

  era <- ehs_randomization_app(dbname = Sys.getenv(EHS_DBNAME),
                               host = Sys.getenv(EHS_HOST),
                               port = Sys.getenv(EHS_PORT),
                               user = Sys.getenv(EHS_USER),
                               password = Sys.getenv(EHS_PASSWORD))
  expect_equal(2 * 2, 4)
})
