% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/woz_articles.R
\name{get_woz_articles_from_archive}
\alias{get_woz_articles_from_archive}
\title{Get all articles from the WOZ archive}
\usage{
get_woz_articles_from_archive(woz_articles = NULL, years, df = FALSE)
}
\arguments{
\item{woz_articles}{either NULL (default) or a data frame containing previously fetched articles (see details).
IF NULL, an empty skeleton is automatically provided with \code{construct_woz_df_skeleton()}.
Else, the new or updated articles are appended to exisiting data frame \code{woz_articles}.}

\item{years}{the years from which the articles should be fetched, eg. \code{seq(2017, 2022, 1)}.}

\item{df}{If FALSE (default) articles are stored as raw html-elemnts in a list, else as df with the columns described below.}
}
\value{
woz articles either as raw html-elements in a list or as data frame
}
\description{
Function fetches the all online articles of the Swiss news outlet "Die Wochenzeitung (WOZ)" from its archive.
The archive contains all articles since 2005, which can be fetched using this function and stored as df or list.
}
\details{
The following columns need to be contained in the df \code{woz_articles} if it is provided as parameter:
\code{woz_articles}:
\itemize{
 \item{"section"}
 \item{"published_time"}
 \item{"modified_time"}
 \item{"full_link}
 \item{"field_issue"}
 \item{"title"}
 \item{"subtitle"}
 \item{"author}
 \item{"image_link}
 \item{"image_caption}
 \item{"text_titles}
 \item{"text"}
 \item{"prowoz"}
}
}
\examples{
# not run
# woz_articles_2122 <- get_woz_articles_from_archive(woz_articles = NULL,
#                                                    years = seq(2021, 2022, 1),
#                                                    df = TRUE)

}
