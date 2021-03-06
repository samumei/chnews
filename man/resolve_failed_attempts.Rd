% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nzz_articles.R
\name{resolve_failed_attempts}
\alias{resolve_failed_attempts}
\title{Helper Function to retry failed downloading attempts}
\usage{
resolve_failed_attempts(raw_articles, article_links)
}
\arguments{
\item{raw_articles}{list of raw nzz articles as returned from \code{get_nzz_articles()}.}

\item{article_links}{string vector with links to the articles.}
}
\description{
Checks if  the download for a link failed and gives it another try.
}
