% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic_functions.R
\name{plot_article_frequency}
\alias{plot_article_frequency}
\title{Plotting Article Frequency}
\usage{
plot_article_frequency(articles, period = "month", from = NULL, to = NULL)
}
\arguments{
\item{articles}{data frame with articles to be counted and plotted. Needs to contain a date (yyyy-mm-dd) column
\code{month_published}, and optionally \code{day_published} or \code{year_published},
if articles should be plotted per day or year.}

\item{period}{string indicating the time period over which the articles should be published. Either articles are counted per
"month" (default), "day", or  "year".}

\item{from}{string i.e. date of format "yyyy-mm-dd" indicating the publishing date from which on articles should be evaluated and plotted.
For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.}

\item{to}{string i.e. date of format "yyyy-mm-dd" indicating the publishing date until which articles should be evaluated and plotted.
For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.}
}
\value{
a ggplot line chart with the article frequency
}
\description{
Plots the number of articles published per day, month, or year in a line chart.
}
\examples{

## not run
# woz_articles_raw <- get_woz_articles()
# woz_articles_clean <- clean_woz_articles(woz_articles_raw)
# plot_article_frequency(woz_articles_clean)

}
