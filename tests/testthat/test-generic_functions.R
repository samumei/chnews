## test_generic_fuctions.R  |  chnews
## Samuel Meier | 2022-04-07
## ---------------------------


# Testing generic fuctions with WOZ articles ----------------

# data("woz_articles_clean")
#
# testthat::test_that("Test plotting WOZ article frequency " , {
#   plot <- plot_article_frequency(woz_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
#
# testthat::test_that("Test plotting WOZ section frequency " , {
#   plot <- plot_section_frequency(woz_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_section_frequency(woz_articles_clean, relative = T)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_section_frequency(woz_articles_clean, period = "month")
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
#
# testthat::test_that("Test plotting WOZ author frequency " , {
#   plot <- plot_author_frequency(woz_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_author_frequency(woz_articles_clean, top = 10)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_author_frequency(woz_articles_clean, period = "year")
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
#
# testthat::test_that("Test plotting WOZ main collaborators " , {
#   plot <- plot_main_collaborators(woz_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_main_collaborators(woz_articles_clean, specific_author = "Bettina Dyttrich")
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })


# Testing generic fuctions with SRF articles ----------------

# data("srf_articles_clean")
#
# testthat::test_that("Test plotting SRF article frequency " , {
#   plot <- plot_article_frequency(srf_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
#
# testthat::test_that("Test plotting SRF section frequency " , {
#   plot <- plot_section_frequency(srf_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_section_frequency(srf_articles_clean, relative = T)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_section_frequency(srf_articles_clean, period = "month")
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
#
# testthat::test_that("Test plotting SRF author frequency " , {
#   plot <- plot_author_frequency(srf_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_author_frequency(srf_articles_clean, top = 10)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_author_frequency(srf_articles_clean, period = "year")
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
#
# testthat::test_that("Test plotting SRF main collaborators " , {
#   plot <- plot_main_collaborators(srf_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_main_collaborators(srf_articles_clean, specific_author = "Christina Scheidegger")
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
#
#
# # Testing generic fuctions with NZZ articles ----------------
#
# data("nzz_articles_clean")
#
# testthat::test_that("Test plotting NZZ article frequency " , {
#   plot <- plot_article_frequency(nzz_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
#
# testthat::test_that("Test plotting NZZ section frequency " , {
#   plot <- plot_section_frequency(nzz_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_section_frequency(nzz_articles_clean, relative = T)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_section_frequency(nzz_articles_clean, period = "month")
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
#
# testthat::test_that("Test plotting NZZ author frequency " , {
#   plot <- plot_author_frequency(nzz_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_author_frequency(nzz_articles_clean, top = 10)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_author_frequency(nzz_articles_clean, period = "year")
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
#
# testthat::test_that("Test plotting NZZ main collaborators " , {
#   plot <- plot_main_collaborators(nzz_articles_clean)
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
#
#   plot <- plot_main_collaborators(nzz_articles_clean, specific_author = "Bettina Dyttrich")
#   testthat::expect_equal(class(plot)[1], "gg")
#   testthat::expect_equal(class(plot)[2], "ggplot")
# })
