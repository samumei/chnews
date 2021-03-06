% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nzz_articles.R
\name{get_nzz_articles}
\alias{get_nzz_articles}
\title{Get NZZ articles}
\usage{
get_nzz_articles(n = 100, df = FALSE)
}
\arguments{
\item{n}{integer indicating the number of most recent articles to download. The maximum possible is 10000.}

\item{df}{Boolean, if FALSE (default) the articles are stored in a list. If TRUE, they are stored in a data frame.}
}
\value{
a list or data frame with the fetched NZZ articles.
}
\description{
Function fetches the most recent articles (up to 10000) from nzz.ch and stores them either in a data frame or list.
}
\details{
Some content is behind pay walls. In order to download the entire text, the user needs to be be logged in.
         This can be achieved for instance using the package \code{rselenium}.
}
\examples{
# not run
# get_nzz_articles()

}
