## test_srf_articles.R  |  chnews
## Samuel Meier | 2022-04-06
## ---------------------------


# testthat::test_that("Correct srf skeleton created" , {
#   srf_articles <- construct_srf_df_skeleton()
#   testthat::expect_equal(length(names(srf_articles)), 27)
# })
#
#
# testthat::test_that("Scraping srf.ch articles works" , {
#   srf_articles <- get_srf_articles()
#   testthat::expect_true(length(srf_articles$author) > 10)
# })
#
# testthat::test_that("Scraping srf.ch articles continously works" , {
#   srf_articles <- get_srf_articles_continously(srf_articles = NULL, update_in_minutes = 1/60, total_updates = 3)
#   testthat::expect_true(length(srf_articles$author) > 10)
# })
#
#
# load("data-raw/srf_articles_raw.rda")
#
# testthat::test_that("Cleaning SRF dates" , {
#   srf_articles <- clean_srf_dates(srf_articles_raw)
#   testthat::expect_true(lubridate::is.Date(srf_articles$day))
#   testthat::expect_true(lubridate::is.Date(srf_articles$month))
#   testthat::expect_true(lubridate::is.Date(srf_articles$year))
# })
#
# testthat::test_that("Cleaning SRF authors" , {
#   srf_articles <- clean_srf_authors(srf_articles_raw)
#   testthat::expect_true(!is.null(srf_articles$n_authors))
#   testthat::expect_true(!is.null(srf_articles$author_1))
#   testthat::expect_true(!is.null(srf_articles$author_2))
#
#   author_info_1 <- unique(srf_articles$author_1)
#   author_info_2 <- unique(srf_articles$author_2)
#   testthat::expect_true(length(author_info_1) > 1)
#   testthat::expect_true(length(author_info_2) > 1)
#
#   redundant_info_1 <- all(!grepl("Liveticker", srf_articles$author_1))
#   redundant_info_2 <- all(!grepl("Liveticker", srf_articles$author_2))
#   testthat::expect_true(redundant_info_1, TRUE)
#   testthat::expect_true(redundant_info_2, TRUE)
# })
#
# testthat::test_that("Cleaning SRF articles works" , {
#   srf_articles <- clean_srf_articles(srf_articles_raw)
#
#   testthat::expect_true(lubridate::is.Date(srf_articles$day))
#   testthat::expect_true(lubridate::is.Date(srf_articles$month))
#   testthat::expect_true(lubridate::is.Date(srf_articles$year))
#
#   author_info_1 <- unique(srf_articles$author_1)
#   author_info_2 <- unique(srf_articles$author_2)
#   testthat::expect_true(length(author_info_1) > 1)
#   testthat::expect_true(length(author_info_2) > 1)
#
#   redundant_info_1 <- all(!grepl("Analyse", srf_articles$author_1))
#   redundant_info_2 <- all(!grepl("Analyse", srf_articles$author_2))
#   testthat::expect_true(redundant_info_1, TRUE)
#   testthat::expect_true(redundant_info_2, TRUE)
# })


