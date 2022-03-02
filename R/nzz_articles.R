# Get NZZ Articles
# Author: Samuel Meier
# Date: 2022-04-06

# Introduction ----------------------------------

# This script contains functions to download articles from woz.ch.

# Required Packages ------------------------------------

#' @import jsonlite
NULL

# library(RSelenium)
# library(wdman)


# Functions  ------------------------------------

#' Function to get raw nzz articles
#'
#' The nzz articles according to the provided link are downloaded and stored in a list.
#'
#' @param article_links string vector of length 1 or more wi links to the nzz articles
#'
#' @return list containing the raw nzz articles
#' @export
#'
#' @examples
#'
#' ## not run
#' # links <- get_newest_nzz_article_links()
#' # get_nzz_articles_raw(links)
#'
get_nzz_articles_raw <- function(article_links) {

  raw_articles <- list()

  pb <- progress::progress_bar$new(total = length(article_links))

  for(i in 1:length(article_links)) {
    pb$tick()
    new_list_item <- list(new = try(read_html(article_links[i])))
    names(new_list_item) <- i
    raw_articles <- append(raw_articles, new_list_item)
  }

  # try again twice to resolve the failed attempts

  print("Resolving failed attempts...")
  raw_articles <- try(resolve_failed_attempts(raw_articles, article_links))
  raw_articles <- try(resolve_failed_attempts(raw_articles, article_links))
  raw_articles <- try(resolve_failed_attempts(raw_articles, article_links))

  return(raw_articles)
}


#' Helper Function to retry failed downloading attempts
#'
#' Checks if  the download for a link failed and gives it another try.
#'
#' @param raw_articles list of raw nzz articles as returned from \code{get_nzz_articles()}.
#' @param article_links string vector with links to the articles.
#'
resolve_failed_attempts <- function(raw_articles, article_links) {

  pb <- progress::progress_bar$new(total = length(article_links))

  for(i in 1:length(article_links)) {
    pb$tick()

    if(typeof(raw_articles[[i]]) != "list") {
      if(raw_articles[[i]][1] == "Error in open.connection(x, \"rb\") : HTTP error 404.\n") {
        new_list_item <- try(read_html(article_links[i+1]))
      }
      else{
        new_list_item <- try(read_html(article_links[i]))
      }
      raw_articles[[i]] <- new_list_item
    }
  }
  return(raw_articles)
}


#' Function to fetch newest NZZ article links
#'
#' @param n integer indicating the number of most recent articles to download. The maximum possible is 10000.
#'
#' @return vector of length \code{n} containing the NZZ article links as characters.
#'
#' @export
get_newest_nzz_article_links <- function(n = 1000) {
  newest_articles <- jsonlite::fromJSON(paste0("https://enrico.nzz.ch/v2/newest-articles?product=nzz&limit=", n, "&offset=0"))
  links_newest_articles <- newest_articles$data$metadata$url
  return(links_newest_articles)
}





#' Get NZZ articles
#'
#' Function fetches the most recent articles (up to 10000) from nzz.ch and stores them either in a data frame or list.
#'
#' @param n integer indicating the number of most recent articles to download. The maximum possible is 10000.
#' @param df Boolean, if FALSE (default) the articles are stored in a list. If TRUE, they are stored in a data frame.
#'
#' @details Some content is behind pay walls. In order to download the entire text, the user needs to be be logged in.
#'          This can be achieved for instance using the package \code{rselenium}.
#'
#' @return a list or data frame with the fetched NZZ articles.
#' @export
#'
#' @examples
#' # not run
#' # get_nzz_articles()
#'
get_nzz_articles <- function(n = 100, df = FALSE) {

  article_links <- get_newest_nzz_article_links(n)
  nzz_articles_raw <- get_nzz_articles_raw(article_links = article_links)

  if(df){
    nzz_articles <- construct_nzz_df_skeleton()

    # remove first empty space of strings
    pb <- progress::progress_bar$new(total = length(nzz_articles_raw))

    for(i in 1:length(nzz_articles_raw)) {
      pb$tick()

      full_link <- article_links[i]
      section <- gsub(".*nzz.ch/(.*)/.*", "\\1", full_link)
      section <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", section, perl = TRUE)

      meta_data <- get_nzz_meta_data(nzz_articles_raw[[i]])
      article_data <- get_nzz_article_data(nzz_articles_raw[[i]])

      nzz_articles <- rbind(nzz_articles, c(full_link, section, meta_data, article_data))
    }
    nzz_articles <- nzz_articles[2:length(nzz_articles$full_link),]
  }
  else nzz_articles <- nzz_articles_raw

  return(nzz_articles)
}




#' Helper Function to extract meta data of raw nzz html-document and return it as vector.
#'
#' @param raw_article list element with one raw nzz article as returned from \code{get_nzz_articles()}.
#'
get_nzz_meta_data <- function(raw_article) {

  meta_properties <- html_elements(raw_article, "meta") %>% html_attr("name")

  meta_contents <- html_elements(raw_article, "meta") %>% html_attr("content")

  description <- meta_contents[match("description", meta_properties)]
  author <- meta_contents[match("author", meta_properties)]
  published_time <- meta_contents[match("date", meta_properties)]
  modified_time <- meta_contents[match("last-modified", meta_properties)]
  news_keywords <- meta_contents[match("news_keywords", meta_properties)]

  meta_data <- c(description, author, published_time, modified_time)

  return(meta_data)
}



#' Helper Function to extract article data of raw nzz html-document and return it as vector.
#'
#' @param raw_article list element with one raw nzz article as returned from \code{get_nzz_articles()}.
#'
get_nzz_article_data <- function(raw_article) {

  title <- html_element(raw_article, "title") %>% html_text()

  headline_title <- html_element(raw_article, ".headline__title") %>%
    html_text()

  headline_lead <- html_element(raw_article, ".headline__lead") %>%
    html_text()

  image_caption <- html_element(raw_article, ".image-description__caption") %>%
    html_text()

  image_author <- html_element(raw_article, ".image-description__author-single") %>%
    html_text()

  text_without_paywall <- html_elements(raw_article, "[class = 'articlecomponent text']") %>%
    html_text()

  # you will only get that if you are logged in...
  text_with_paywall <- html_elements(raw_article, "[class = 'articlecomponent text regwalled']") %>%
    html_text()

  if(length(text_with_paywall) == 0) {
    if(length(text_without_paywall) == 0) {
      text <- ""
    }
    else text <- paste(c("<p>", text_without_paywall, "</p>"), collapse = " </p> <p>  ")
  }
  else {
    text <- paste(c("<p>", text_without_paywall, text_with_paywall, "</p>"), collapse = " </p> <p>  ")
  }

  linked_authors <- html_elements(raw_article, "[class = 'metainfo__item metainfo__item--author']") %>%
    html_text()

  linked_authors <- linked_authors[2:length(linked_authors)]

  linked_authors <- paste0(linked_authors, collapse = "; ")

  article_data <- c(title, headline_title, headline_lead, image_caption, image_author,text, linked_authors)

  return(article_data)
}


#' Constructs a data frame with the required column names for the function \code{get_nzz_articles()}
#'
#' @return a data frame with 13 named empty columns (see details).
#' @details The following empty columns are contained in the returned df:
#' \itemize{
#'  \item{"full_link"}
#'  \item{"section"}
#'  \item{"description"}
#'  \item{"author}
#'  \item{"published_time"}
#'  \item{"modified_time"}
#'  \item{"title"}
#'  \item{"headline_title"}
#'  \item{"headline_lead}
#'  \item{"image_caption}
#'  \item{"image_author"}
#'  \item{"text"}
#'  \item{"linked_authors"}
#' }
#' @export
#'
#' @examples
#' nzz_articles_empty <- construct_nzz_df_skeleton()
#'
construct_nzz_df_skeleton <- function(){
  nzz_articles <- data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

  names(nzz_articles) <- c("full_link", "section", "description", "author", "published_time", "modified_time",
                           "title", "headline_title", "headline_lead", "image_caption", "image_author", "text", "linked_authors")

  return(nzz_articles)
}




#' Cleaning raw NZZ articles
#'
#' @param nzz_articles a data frame containing raw NZZ articles as returned from the function
#'                     \code{get_nzz_articles}.
#' @param no_updates Boolean that indicates if updated versions should be removed.
#'                   If TRUE, each article kept is unique (default), else also updated versions of articles are kept.
#'
#' @return returns the cleaned data frame with the same columns.
#' @export
#'
#' @examples
#'
#' # not run
#' # nzz_articles_raw <- get_nzz_articles()
#' # nzz_articles_clean <- clean_nzz_articles(nzz_articles_raw)
#'
clean_nzz_articles <- function(nzz_articles, no_updates = TRUE) {

  if(no_updates) nzz_articles <- nzz_articles[!duplicated(nzz_articles[ , "full_link"]),]

  nzz_articles <- clean_nzz_dates(nzz_articles)
  nzz_articles <- clean_nzz_authors(nzz_articles)

  return(nzz_articles)
}


#' A helper function to clean NZZ articles
#'
#' Turns NZZ published_time into a day, month and year date column, in which the article was published.
#'
#' @param nzz_articles a data frame containing raw NZZ articles as returned from the function
#'                     \code{get_nzz_articles}.
#'
#'
#' @return the cleaned data frame \code{nzz_articles}
#'
clean_nzz_dates <- function(nzz_articles) {

  nzz_articles <- filter(nzz_articles, !is.na(published_time))
  nzz_articles$day_published <- as.Date(substr(nzz_articles$published_time, 1, 10 ))

  nzz_articles$month_published <- as.Date(cut(nzz_articles$day_published,
                                              breaks = "month"))

  nzz_articles$year_published <- as.Date(cut(nzz_articles$day_published,
                                             breaks = "year"))

  return(nzz_articles)
}


#' Helper function to clean NZZ articles
#'
#' Removes redundant information from NZZ author column and separates collaborating authors into two
#' separate author columns.
#'
#' @param nzz_articles a data frame containing raw NZZ articles as returned from the function
#'                     \code{get_nzz_articles}.
#'
#' @return the cleaned data frame \code{nzz_articles}.
#'
clean_nzz_authors <- function(nzz_articles) {

  nzz_articles$author <- trimws(nzz_articles$author, which = "both")
  nzz_articles$author <- gsub("Text", "", nzz_articles$author)
  nzz_articles$author <- gsub("[()]", "", nzz_articles$author)

  # remove info after comma
  for(i in 1:length(nzz_articles$author)) {
    if(length(gregexpr(",",  nzz_articles$author[i])[[1]]) == 1) {
      nzz_articles$author[i] <- gsub("(.*),.*", "\\1", nzz_articles$author[i])
    }
  }

  # manage collaborations (only if two authors, articles with three or more authors are ignored)
  nzz_articles$n_authors <- rep(1, length(nzz_articles$author))
  nzz_articles$author_1 <- nzz_articles$author
  nzz_articles$author_2 <- rep(NA_character_, length(nzz_articles$author))

  # separate authors into two columns -> not yet possible
  # nzz_articles <- separate_nzz_authors(nzz_articles)

  nzz_articles$author_1 <- gsub("(.*),.*", "\\1", nzz_articles$author_1)
  nzz_articles$author_2 <- gsub("(.*),.*", "\\1", nzz_articles$author_2)

  return(nzz_articles)
}


#' A helper function to clean NZZ authors
#'
#' Separates collaborating authors into two separate author columns.
#'
#' @param nzz_articles a data frame containing raw NZZ articles as returned from the function
#'                     \code{get_nzz_articles}.
#'
#' @return the cleaned data frame \code{nzz_articles}
#'
separate_nzz_authors <- function(nzz_articles) {

  undefined_articles <- 0
  for(i in 1:length(nzz_articles$author)) {
    if(grepl("und", nzz_articles$author[i])) {
      if(grepl(",", nzz_articles$author[i])) {
        undefined_articles <- undefined_articles + 1
      }
      nzz_articles$author_1[i] <- gsub("(.*) und .*", "\\1", nzz_articles$author[i])
      nzz_articles$author_2[i] <- gsub(".* und (.*)", "\\1", nzz_articles$author[i])
      nzz_articles$n_authors[i] <- 2
    }
    if(grepl(",",  nzz_articles$author[i])[[1]]) {
      nzz_articles$author[i] <- gsub("(.*),.*", "\\1", nzz_articles$author[i])
    }
  }
  print(paste("Could not separate articles due to more than two collaborators or ambigous author name."))
  return(nzz_articles)
}
