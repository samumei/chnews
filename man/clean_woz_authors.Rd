% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/woz_articles.R
\name{clean_woz_authors}
\alias{clean_woz_authors}
\title{Helper function to clean WOZ articles}
\usage{
clean_woz_authors(woz_articles)
}
\arguments{
\item{woz_articles}{a data frame containing raw WOZ articles as returned from the functions
\code{get_woz_articles}, \code{get_woz_articles_continously},
or \code{get_woz_articles_from_archive}.}
}
\value{
the cleaned data frame \code{woz_articles}.
}
\description{
Removes redundant information from WOZ author column and separates collaborating authors into two
separate author columns.
}
