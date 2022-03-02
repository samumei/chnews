## test_woz_articles.R  |  chnews
## Samuel Meier | 2022-04-06
## ---------------------------


# testthat::test_that("Correct WOZ skeleton created" , {
#   woz_articles <- construct_woz_df_skeleton()
#   testthat::expect_equal(length(names(woz_articles)), 13)
# })
#
# testthat::test_that("Scraping woz.ch articles works" , {
#   woz_articles <- get_woz_articles()
#   testthat::expect_true(length(woz_articles$author) > 10)
# })
#
# testthat::test_that("Scraping woz.ch articles continously works" , {
#   woz_articles <- get_woz_articles_continously(woz_articles = NULL, update_in_minutes = 1/60, total_updates = 3)
#   testthat::expect_true(length(woz_articles$author) > 10)
# })
#
# testthat::test_that("Scraping woz.ch articles from archive works (as list)" , {
#   woz_articles <- get_woz_articles_from_archive(woz_articles = NULL, years = 2022)
#   testthat::expect_true(length(woz_articles) > 50)
# })
#
# testthat::test_that("Scraping woz.ch articles from archive works (as data frame)" , {
#   woz_articles <- get_woz_articles_from_archive(woz_articles = NULL, years = 2022, df = TRUE)
#   testthat::expect_true(length(woz_articles$author) > 50)
# })
#
# data("woz_articles_raw")
#
# testthat::test_that("Cleaning WOZ dates" , {
#   woz_articles <- clean_woz_dates(woz_articles_raw)
#   testthat::expect_true(lubridate::is.Date(woz_articles$day))
#   testthat::expect_true(lubridate::is.Date(woz_articles$month))
#   testthat::expect_true(lubridate::is.Date(woz_articles$year))
# })
#
# testthat::test_that("Cleaning WOZ authors" , {
#   woz_articles <- clean_woz_authors(woz_articles_raw)
#   testthat::expect_true(!is.null(woz_articles$n_authors))
#   testthat::expect_true(!is.null(woz_articles$author_1))
#   testthat::expect_true(!is.null(woz_articles$author_2))
#
#   author_info_1 <- unique(woz_articles$author_1)
#   author_info_2 <- unique(woz_articles$author_2)
#   testthat::expect_true(length(author_info_1) > 1)
#   testthat::expect_true(length(author_info_2) > 1)
#
#   redundant_info_1 <- all(!grepl("Autor:in", woz_articles$author_1))
#   redundant_info_2 <- all(!grepl("Autor:in", woz_articles$author_2))
#   testthat::expect_true(redundant_info_1, TRUE)
#   testthat::expect_true(redundant_info_2, TRUE)
# })
#
# testthat::test_that("Cleaning WOZ articles works" , {
#   woz_articles <- clean_woz_articles(woz_articles_raw)
#
#   testthat::expect_true(lubridate::is.Date(woz_articles$day))
#   testthat::expect_true(lubridate::is.Date(woz_articles$month))
#   testthat::expect_true(lubridate::is.Date(woz_articles$year))
#
#   author_info_1 <- unique(woz_articles$author_1)
#   author_info_2 <- unique(woz_articles$author_2)
#   testthat::expect_true(length(author_info_1) > 1)
#   testthat::expect_true(length(author_info_2) > 1)
#
#   redundant_info_1 <- all(!grepl("Mail", woz_articles$author_1))
#   redundant_info_2 <- all(!grepl("Mail", woz_articles$author_2))
#   testthat::expect_true(redundant_info_1, TRUE)
#   testthat::expect_true(redundant_info_2, TRUE)
# })





