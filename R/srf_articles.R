# Get SRF Articles
# Author: Samuel Meier
# Date: 2022-04-06

# Introduction ----------------------------------

# This script contains functions to download articles from srf.ch.

# Required Packages ------------------------------------

#' @import rvest
#' @importFrom progress progress_bar
#' @importFrom assertthat assert_that
NULL

# Functions  ------------------------------------


#' Get newest SRF articles over a defined period of time
#'
#' Function regularly fetches the most recent online articles of the Swiss news institution "Schweizer Radio und Fernsehen (SRF)".
#' This function is especially useful to analyze if and how articles are being updated after they have been published online.
#'
#' @param srf_articles either NULL (default) or a data frame containing previously fetched articles (see details).
#' IF NULL, an empty skeleton is automatically provided with \code{construct_srf_df_skeleton()}.
#' Else, the new or updated articles are appended to exisiting data frame \code{srf_articles}.
#' @param update_in_minutes an integer that defines after how many minutes the function should check for new or updated articles.
#' @param total_updates an integer that defines the total amount of updates. Multiplying \code{total_updates} with \code{update_in_minutes} gives a rough approximation of total execution time.
#' @param save_backup If TRUE (default) stores the articles as csv-file after every mutation as backup.
#'
#' @details The following columns need to exist in the df \code{srf_articles}, if it is provided as parameter:
#' \code{srf_articles}:
#' \itemize{
#'  \item{"uploadDate"}
#'  \item{"description"}
#'  \item{"thumbnailUrl"}
#'  \item{"section"}
#'  \item{"published_time"}
#'  \item{"modified_time"}
#'  \item{"identifier}
#'  \item{"image_url}
#'  \item{"commentCount}
#'  \item{"full_link}
#'  \item{"section_link"}
#'  \item{"subsection"}
#'  \item{"title_overline"}
#'  \item{"title"}
#'  \item{"subtitle"}
#'  \item{"author}
#'  \item{"sharings}
#'  \item{"text_titles}
#'  \item{"text"}
#'  \item{"quote"}
#'  \item{"expandable_box"}
#'  \item{"related_articles"}
#'  \item{"related_articles_info"}
#'  \item{"reference}
#'  \item{"allows_comments}
#'  \item{"comment_author}
#'  \item{"comment_content"}
#' }
#' @return the data frame \code{srf_articles} updated with all the fetched articles.
#' @export
#'
#' @examples
#' # not run
#' # get_srf_articles_continously(srf_articles = NULL, update_in_minutes = 5, total_updates = 50)
#'
get_srf_articles_continously <- function(srf_articles = NULL,
                                            update_in_minutes = 15,
                                            total_updates = 48,
                                            save_backup = TRUE) {

  if(is.null(srf_articles)) srf_articles <- construct_srf_df_skeleton()

  pb <- progress::progress_bar$new(total = total_updates)

  for(i in 1:total_updates) {

    pb$tick()

    n_articles_old <- length(srf_articles$full_link)

    srf_articles <- try(get_newest_srf_articles(srf_articles))

    n_articles_new <- length(srf_articles$full_link)

    if(save_backup & (n_articles_old != n_articles_new)) utils::write.csv2(srf_articles, paste0("srf_articles_", i, ".csv"))

    print("Waiting...")
    Sys.sleep(60*update_in_minutes)
    print("Another round!")
  }

  return(srf_articles)
}


#' Get SRF articles once when executed
#'
#' Function fetches the most recent online articles of the Swiss news institution "Schweizer Radio und Fernsehen (SRF)".
#' The function can run daily, or even less frequently. Especially useful if executed with cronjob or manually on a daily basis.
#'
#' @param srf_articles either NULL (default) or a data frame containing previously fetched articles (see details).
#' IF NULL, an empty skeleton is automatically provided with \code{construct_srf_df_skeleton()}.
#' Else, the new or updated articles are appended to exisiting data frame \code{srf_articles}.
#'
#' @details The following columns need to be contained in the df \code{srf_articles} if it is provided as parameter:
#' \code{srf_articles}:
#' \itemize{
#'  \item{"uploadDate"}
#'  \item{"description"}
#'  \item{"thumbnailUrl"}
#'  \item{"section"}
#'  \item{"published_time"}
#'  \item{"modified_time"}
#'  \item{"identifier}
#'  \item{"image_url}
#'  \item{"commentCount}
#'  \item{"full_link}
#'  \item{"section_link"}
#'  \item{"subsection"}
#'  \item{"title_overline"}
#'  \item{"title"}
#'  \item{"subtitle"}
#'  \item{"author}
#'  \item{"sharings}
#'  \item{"text_titles}
#'  \item{"text"}
#'  \item{"quote"}
#'  \item{"expandable_box"}
#'  \item{"related_articles"}
#'  \item{"related_articles_info"}
#'  \item{"reference}
#'  \item{"allows_comments}
#'  \item{"comment_author}
#'  \item{"comment_content"}
#' }
#' @return the data frame \code{srf_articles} updated with all the fetched articles. Also stores the df as csv in the background.
#' @export
#'
#' @examples
#' # not run
#' # get_srf_articles()
#'
get_srf_articles <- function(srf_articles = NULL) {

  if(is.null(srf_articles)) srf_articles <- construct_srf_df_skeleton()

  n_articles_old <- length(srf_articles$full_link)

  srf_articles <- get_newest_srf_articles(srf_articles)

  n_articles_new <- length(srf_articles$full_link)

  if (n_articles_old != n_articles_new) {
    utils::write.csv2(srf_articles, paste0("srf_articles_", Sys.Date(), ".csv"))
    print ("Successfully downloaded SRF articles and stored to CSV!")
  }
  else {
    print("No new or updated articles found!")
  }

  return(srf_articles)
}


#' Constructs a data frame with the required column names for the functions \code{get_srf_articles()}
#'
#' @return a data frame with 27 named empty columns (see details).
#' @details The following empty columns are contained in the returned df:
#' \itemize{
#'  \item{"uploadDate"}
#'  \item{"description"}
#'  \item{"thumbnailUrl"}
#'  \item{"section"}
#'  \item{"published_time"}
#'  \item{"modified_time"}
#'  \item{"identifier}
#'  \item{"image_url}
#'  \item{"commentCount}
#'  \item{"full_link}
#'  \item{"section_link"}
#'  \item{"subsection"}
#'  \item{"title_overline"}
#'  \item{"title"}
#'  \item{"subtitle"}
#'  \item{"author}
#'  \item{"sharings}
#'  \item{"text_titles}
#'  \item{"text"}
#'  \item{"quote"}
#'  \item{"expandable_box"}
#'  \item{"related_articles"}
#'  \item{"related_articles_info"}
#'  \item{"reference}
#'  \item{"allows_comments}
#'  \item{"comment_author}
#'  \item{"comment_content"}
#' }
#' @export
#'
#' @examples
#' srf_articles_empty <- construct_srf_df_skeleton()
#'
construct_srf_df_skeleton <- function(){
  srf_articles <- data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

  names(srf_articles) <- c("uploadDate", "description", "thumbnailUrl", "section", "published_time", "modified_time",
                           "identifier", "image_url", "commentCount", "full_link", "section_link", "subsection", "title_overline",
                           "title", "subtitle", "author", "sharings", "text_titles", "text", "quote", "expandable_box",
                           "related_articles", "related_articles_info",  "reference", "allows_comments", "comment_author",
                           "comment_content")

  return(srf_articles)
}

#' Helper Function to get newest srf article links and extract article content
#'
#' @param srf_articles a data frame containing previously fetched articles (can be zero)
#'
get_newest_srf_articles <- function(srf_articles) {

  current_links <- read_html("https://www.srf.ch/news/das-neueste") %>%
    html_elements(".teaser__main") %>%
    html_attr("href")

  # remove first empty space of strings
  pb <- progress::progress_bar$new(total = length(current_links))

  for(i in 1:length(current_links)) {
    pb$tick()
    full_link <- paste0("https://www.srf.ch", current_links[i])

    link_index <- match(full_link, srf_articles$full_link)

    if(is.na(link_index)){

      meta_data <- get_meta_data(full_link)
      article_data <- c(meta_data, get_article_data(full_link))
      srf_articles <- rbind(srf_articles, article_data)

    }
    else {
      # only update if modified
      meta_data <- get_meta_data(full_link)

      if(srf_articles$modified_time[link_index] != meta_data[6]) {
        article_data <- c(meta_data, get_article_data(full_link))
        srf_articles <- rbind(srf_articles, article_data)
      }
      else {
        if(srf_articles$commentCount[link_index] != meta_data[9]) {
          # update the comment count of article
          srf_articles$commentCount[link_index] <- meta_data[9]
        }
      }
    }
  }

  return(srf_articles)
}




#' Helper Function to extract meta data of raw srf html-document and return it as vector.
#'
#' @param full_link string with link to srf article
#'
get_meta_data <- function(full_link) {

  meta_properties <- read_html(full_link) %>%
    html_elements("meta") %>%
    html_attr("itemprop")

  meta_contents <- read_html(full_link) %>%
    html_elements("meta") %>%
    html_attr("content")

  uploadDate <- meta_contents[match("uploadDate", meta_properties)]
  description <- meta_contents[match("description", meta_properties)]
  thumbnailUrl <- meta_contents[match("thumbnailUrl", meta_properties)]
  section <- meta_contents[match("articleSection", meta_properties)]
  published_time <- meta_contents[match("datePublished", meta_properties)]
  modified_time <- meta_contents[match("dateModified", meta_properties)]
  identifier <- meta_contents[match("identifier", meta_properties)]
  image_url <- meta_contents[match("url", meta_properties)]
  commentCount <- meta_contents[match("commentCount", meta_properties)]

  meta_data <- c(uploadDate, description, thumbnailUrl, section, published_time, modified_time, identifier, image_url, commentCount)

  return(meta_data)
}


#' Helper Function to extract data of raw srf html-document and return it as vector.
#'
#' @param full_link string with link to srf article
#'
get_article_data <- function(full_link) {
  raw <- read_html(full_link)

  breadcrumb_link <- html_elements(raw, ".breadcrumb__link") %>% html_attr("href") # Evtl. von hier holen und noch aufbereiten oder sonst von anderswo (z.b. aus Link)

  section_link <- breadcrumb_link[1]
  subsection <- breadcrumb_link[2]

  title_overline <- html_element(raw, ".article-title__overline") %>% html_text()

  title <- html_element(raw, ".article-title__text") %>% html_text()

  subtitle <- html_element(raw, ".article-lead") %>% html_text()

  # Author: isn't there a smoother way with one line and itemprop=author or smth.
  span_properties <- read_html(full_link) %>%
    html_elements("span") %>%
    html_attr("itemprop")

  span_contents <- read_html(full_link) %>%
    html_elements("span") %>%
    html_text()

  author <- span_contents[match("author", span_properties)]

  sharings <- html_elements(raw, ".sharing-bar__summary ") %>% html_attr("data-i18n-text-share-summary")

  text <- html_element(raw, ".article-content") %>% html_text()

  quote <-  html_element(raw, ".blockquote__text") %>% html_text()

  expandable_box <-  html_element(raw, ".expandable-box__content") %>% html_text()

  text_titles <- read_html(full_link) %>%
    html_elements("h2") %>%
    html_text()
  text_titles <- paste(c("<p>", text_titles, "</p>"), collapse = " </p> <p>  ")

  related_articles <-  html_elements(raw, ".related-items-list__content a") %>% html_attr("href")
  related_articles <- paste(related_articles, collapse = "; ")

  related_articles_info <- html_elements(raw, ".teaser-info") %>% html_text()
  related_articles_info <- paste(related_articles_info, collapse = "; ")

  reference <- html_elements(raw, ".article-reference") %>% html_text()

  comment_author <- html_elements(raw, ".comment__user") %>% html_text()
  comment_content <- html_elements(raw, ".comment__content") %>% html_text()
  comment_content <- comment_content[1:length(comment_author)]

  allows_comments <- html_elements(raw, "#socialRegistration") %>% html_text()
  allows_comments <- ifelse(identical(allows_comments, character(0)), FALSE, TRUE)

  comment_author <- paste(c("<p>", comment_author, "</p>"), collapse = " </p> <p>  ")
  comment_content <- paste(c("<p>", comment_content, "</p>"), collapse = " </p> <p>  ")

  # Hier evtl. "Mehr anzeigen" anklicken um weitere Kommentare zu scrapen.

  article_data <- c(full_link, section_link, subsection, title_overline, title, subtitle,
                    author, sharings, text_titles, text, quote, expandable_box,
                    related_articles, related_articles_info,  reference, allows_comments, comment_author, comment_content)

  return(article_data)
}




#' Cleaning raw SRF articles
#'
#' @param srf_articles a data frame containing raw SRF articles as returned from the functions
#'                     \code{get_srf_articles} or \code{get_srf_articles_continously}.
#' @param no_updates Boolean that indicates if updated versions should be removed.
#'                   If TRUE, each article kept is unique (default), else also updated versions of articles are kept.
#'
#' @return returns the cleaned data frame with the same columns.
#' @export
#'
#' @examples
#'
#' # not run
#' # srf_articles_raw <- get_srf_articles()
#' # srf_articles_clean <- clean_srf_articles(srf_articles_raw)
#'
clean_srf_articles <- function(srf_articles, no_updates = TRUE) {

  if(no_updates) srf_articles <- srf_articles[!duplicated(srf_articles[ , "full_link"]),]

  srf_articles <- clean_srf_dates(srf_articles)
  srf_articles <- clean_srf_authors(srf_articles)

  return(srf_articles)
}


#' A helper function to clean SRF articles
#'
#' Turns SRF uploadDate into a day, month and year date column, in which the article was published.
#'
#' @param srf_articles a data frame containing raw SRF articles as returned from the functions
#'                     \code{get_srf_articles} or \code{get_srf_articles_continously}.
#'
#'
#' @return the cleaned data frame \code{srf_articles}
#'
clean_srf_dates <- function(srf_articles) {

  srf_articles <- filter(srf_articles, !is.na(uploadDate))
  srf_articles$day_published <- as.Date(srf_articles$uploadDate)

  srf_articles$month_published <- as.Date(cut(srf_articles$day_published,
                                              breaks = "month"))

  srf_articles$year_published <- as.Date(cut(srf_articles$day_published,
                                             breaks = "year"))

  return(srf_articles)
}


#' Helper function to clean SRF articles
#'
#' Removes redundant information from SRF author column and separates collaborating authors into two
#' separate author columns.
#'
#' @param srf_articles a data frame containing raw SRF articles as returned from the functions
#'                     \code{get_srf_articles} or \code{get_srf_articles_continously}.
#'
#' @return the cleaned data frame \code{srf_articles}.
#'
clean_srf_authors <- function(srf_articles) {

  srf_articles$author <- trimws(srf_articles$author, which = "both")
  srf_articles$author <- gsub("Eine Analyse von ", "", srf_articles$author)
  srf_articles$author <- gsub("Analyse von ", "", srf_articles$author)
  srf_articles$author <- gsub("Am Liveticker sind ", "", srf_articles$author)
  srf_articles$author <- gsub("Am Liveticker ist ", "", srf_articles$author)
  srf_articles$author <- gsub("Am Liveticker: ", "", srf_articles$author)
  srf_articles$author <- gsub("agenturen", "Agenturen", srf_articles$author)

  # remove info after comma
  for(i in 1:length(srf_articles$author)) {
    if(length(gregexpr(",",  srf_articles$author[i])[[1]]) == 1) {
      srf_articles$author[i] <- gsub("(.*),.*", "\\1", srf_articles$author[i])
    }
  }

  # manage collaborations (only if two authors, articles with three or more authors are ignored)
  srf_articles$n_authors <- rep(1, length(srf_articles$author))
  srf_articles$author_1 <- srf_articles$author
  srf_articles$author_2 <- rep(NA_character_, length(srf_articles$author))

  # separate authors into two columns
  srf_articles <- separate_srf_authors(srf_articles)

  srf_articles$author_1 <- gsub("(.*),.*", "\\1", srf_articles$author_1)
  srf_articles$author_2 <- gsub("(.*),.*", "\\1", srf_articles$author_2)

  return(srf_articles)
}


#' A helper function to clean SRF authors
#'
#' Separates collaborating authors into two separate author columns.
#'
#' @param srf_articles a data frame containing raw SRF articles as returned from the functions
#'                     \code{get_srf_articles} or \code{get_srf_articles_continously}.
#'
#' @return the cleaned data frame \code{srf_articles}
#'
separate_srf_authors <- function(srf_articles) {
  undefined_articles <- 0
  for(i in 1:length(srf_articles$author)) {
    if(grepl("und", srf_articles$author[i])) {
      if(grepl(",", srf_articles$author[i])) {
        undefined_articles <- undefined_articles + 1
      }
      srf_articles$author_1[i] <- gsub("(.*) und .*", "\\1", srf_articles$author[i])
      srf_articles$author_2[i] <- gsub(".* und (.*)", "\\1", srf_articles$author[i])
      srf_articles$n_authors[i] <- 2
    }
    if(grepl(",",  srf_articles$author[i])[[1]]) {
      srf_articles$author[i] <- gsub("(.*),.*", "\\1", srf_articles$author[i])
    }
  }
  print(paste("Could not separate", undefined_articles, "articles due to more than two collaborators or ambigous author name."))
  return(srf_articles)
}





