# Get WOZ Articles
# Author: Samuel Meier
# Date: 2022-04-06

# Introduction ----------------------------------

# This script contains functions to download articles from woz.ch.

# Functions ----------------------------------


#' Get newest WOZ articles over a defined period of time
#'
#' Function regularly fetches the most recent online articles of the Swiss news outlet "Die Wochenzeitung (WOZ)".
#' This function is especially useful to analyze if and how articles are being updated after they have been published online.
#'
#' @param woz_articles either NULL (default) or a data frame containing previously fetched articles (see details).
#' IF NULL, an empty skeleton is automatically provided with \code{construct_woz_df_skeleton()}.
#' Else, the new or updated articles are appended to existing data frame \code{woz_articles}.
#' @param update_in_minutes an integer that defines after how many minutes the function should check for new or updated articles.
#' @param total_updates an integer that defines the total amount of updates. Multiplying \code{total_updates} with \code{update_in_minutes} gives a rough approximation of total execution time.
#' @param sections sections to scrape on woz.ch. Default is all the permanent sections currently (2022-04-06) on the website.
#' @param save_backup If TRUE (default) stores the articles as csv-file after every mutation as backup.
#'
#'
#' @details The following columns need to exist in the df \code{woz_articles}, if it is provided as parameter:
#' \code{woz_articles}:
#' \itemize{
#'  \item{"section"}
#'  \item{"published_time"}
#'  \item{"modified_time"}
#'  \item{"full_link}
#'  \item{"field_issue"}
#'  \item{"title"}
#'  \item{"subtitle"}
#'  \item{"author}
#'  \item{"image_link}
#'  \item{"image_caption}
#'  \item{"text_titles}
#'  \item{"text"}
#'  \item{"prowoz"}
#' }
#' @return the data frame \code{woz_articles} updated with all the fetched articles.
#' @export
#'
#' @examples
#' # not run
#' # get_woz_articles_continously(woz_articles = NULL, update_in_minutes = 5, total_updates = 50)
#'
get_woz_articles_continously <- function(woz_articles = NULL,
                                         update_in_minutes = 15,
                                         total_updates = 48,
                                         sections = c("schweiz", "wirtschaft", "international", "kultur-wissen"),
                                         save_backup = TRUE) {

  if(is.null(woz_articles)) woz_articles <- construct_woz_df_skeleton()

  pb <- progress::progress_bar$new(total = total_updates)

  for(i in 1:total_updates) {

    pb$tick()

    n_articles_old <- length(woz_articles$full_link)

    for(j in 1:length(sections)) {
      woz_articles <- get_section_articles(woz_articles, sections[j])
    }

    n_articles_new <- length(woz_articles$full_link)

    if(save_backup & (n_articles_old != n_articles_new)) utils::write.csv2(woz_articles, paste0("woz_articles_", i, ".csv"))

    print("Waiting...")
    Sys.sleep(60*update_in_minutes)
    print("Another round!")  }

  return(woz_articles)
}




#' Get WOZ articles once when executed
#'
#' Function fetches the most recent online articles of the Swiss news outlet "Die Wochenzeitung (WOZ)".
#' The function can be executed daily or less frequently. It is especially useful if executed with cronjob or manually on a daily basis.
#'
#' @param woz_articles either NULL (default) or a data frame containing previously fetched articles (see details).
#' IF NULL, an empty skeleton is automatically provided with \code{construct_woz_df_skeleton()}.
#' Else, the new or updated articles are appended to exisiting data frame \code{woz_articles}.
#' @param sections sections to scrape on woz.ch. Default is all the permanent sections currently (2022-04-06) on the website.
#'
#' @details The following columns need to be contained in the df \code{woz_articles} if it is provided as parameter:
#' \code{woz_articles}:
#' \itemize{
#'  \item{"section"}
#'  \item{"published_time"}
#'  \item{"modified_time"}
#'  \item{"full_link}
#'  \item{"field_issue"}
#'  \item{"title"}
#'  \item{"subtitle"}
#'  \item{"author}
#'  \item{"image_link}
#'  \item{"image_caption}
#'  \item{"text_titles}
#'  \item{"text"}
#'  \item{"prowoz"}
#' }
#' @return the data frame \code{woz_articles} updated with all the fetched articles. Also stores the df as csv in the background.
#' @export
#'
#' @examples
#' # not run
#' # get_woz_articles()
#'
get_woz_articles <- function(woz_articles = NULL, sections = c("schweiz", "wirtschaft", "international", "kultur-wissen")) {

  if(is.null(woz_articles)) woz_articles <- construct_woz_df_skeleton()

  n_articles_old <- length(woz_articles$full_link)

  for(j in 1:length(sections)) {
    woz_articles <- get_section_articles(woz_articles, sections[j])
  }

  n_articles_new <- length(woz_articles$full_link)

  if (n_articles_old != n_articles_new) {
    utils::write.csv2(woz_articles, paste0("woz_articles_", Sys.Date(), ".csv"))
    print ("Successfully downloaded WOZ articles and stored to CSV!")
  }
  else {
    print("No new or updated articles found!")
  }

  return(woz_articles)
}




#' Get all articles from the WOZ archive
#'
#' Function fetches the all online articles of the Swiss news outlet "Die Wochenzeitung (WOZ)" from its archive.
#' The archive contains all articles since 2005, which can be fetched using this function and stored as df or list.
#'
#' @param woz_articles either NULL (default) or a data frame containing previously fetched articles (see details).
#' IF NULL, an empty skeleton is automatically provided with \code{construct_woz_df_skeleton()}.
#' Else, the new or updated articles are appended to exisiting data frame \code{woz_articles}.
#' @param years the years from which the articles should be fetched, eg. \code{seq(2017, 2022, 1)}.
#' @param df If FALSE (default) articles are stored as raw html-elemnts in a list, else as df with the columns described below.
#'
#' @details The following columns need to be contained in the df \code{woz_articles} if it is provided as parameter:
#' \code{woz_articles}:
#' \itemize{
#'  \item{"section"}
#'  \item{"published_time"}
#'  \item{"modified_time"}
#'  \item{"full_link}
#'  \item{"field_issue"}
#'  \item{"title"}
#'  \item{"subtitle"}
#'  \item{"author}
#'  \item{"image_link}
#'  \item{"image_caption}
#'  \item{"text_titles}
#'  \item{"text"}
#'  \item{"prowoz"}
#' }
#' @return woz articles either as raw html-elements in a list or as data frame
#' @export
#'
#' @examples
#' # not run
#' # woz_articles_2122 <- get_woz_articles_from_archive(woz_articles = NULL,
#' #                                                    years = seq(2021, 2022, 1),
#' #                                                    df = TRUE)
#'
get_woz_articles_from_archive <- function(woz_articles = NULL, years, df = FALSE) {

  if(!df) raw_articles <- list()

  for(i in 1:length(years)) {

    # get links
    editions_of_year <- read_html(paste0("https://www.woz.ch/index/", years[i])) %>%
      html_elements("h2 a") %>%
      html_attr("href")

    for(j in 1:length(editions_of_year)) {

      article_links <- read_html(paste0("https://www.woz.ch", editions_of_year[j])) %>%
        html_elements(".block-link") %>%
        html_attr("href")

      pb <- progress::progress_bar$new(total = length(article_links))

      # get content
      if(df) {

        if(is.null(woz_articles)) woz_articles <- construct_woz_df_skeleton()

        for(k in 1:length(article_links)) {
          pb$tick()
          full_link <- paste0("https://www.woz.ch", article_links[k])

          meta_data <- try(get_woz_meta_data(full_link))
          article_data <- try(c(meta_data, get_woz_article_data(full_link)))
          woz_articles <- rbind(woz_articles, article_data)
        }
      }
      else {
        assertthat::assert_that(is.null(woz_articles), msg = "If articles are stored as list, no data frame \"woz_articles\" can be passed as parameter!")

        for(k in 1:length(article_links)) {
          pb$tick()
          new_list_item <- list(new = try(read_html(paste0("https://www.woz.ch", article_links[k]))))
          names(new_list_item) <- length(raw_articles) + 1
          raw_articles <- append(raw_articles, new_list_item)
        }
      }

      print(paste0("Successfully fetched articles of edition ", editions_of_year[j], "!"))

    }

    print(paste0("Successfully fetched articles of ", years[i], "!"))
  }

  if(!df) woz_articles <- raw_articles[-1]

  return(woz_articles)
}



#' Get the newest articles from each section on woz.ch
#'
#' @param woz_articles a data frame with 13 named columns (see \code{get_woz_articles} and \code{construct_woz_df_skeleton} )
#' @param section sections to scrape on woz.ch. Default is all the permanent sections currently (2022-04-06) on the website.
#'
#' @return the data frame \code{woz_articles} updated with all the fetched articles.
#'
get_section_articles <- function(woz_articles, section) {

  current_links <- read_html(paste0("https://www.woz.ch/t/", section)) %>%
    html_elements(".block-link") %>%
    html_attr("href")

  # remove first empty space of strings
  current_links <- substring(current_links, 1)
  pb <- progress::progress_bar$new(total = length(current_links))

  for(i in 1:length(current_links)) {
    pb$tick()
    full_link <- paste0("https://www.woz.ch", current_links[i])

    link_index <- match(full_link, woz_articles$full_link)

    if(is.na(link_index)){

      meta_data <- try(get_woz_meta_data(full_link))
      article_data <- try(c(meta_data, get_woz_article_data(full_link, section)))
      woz_articles <- rbind(woz_articles, article_data)

    }
    else{

      meta_data <- try(get_woz_meta_data(full_link))

      if(woz_articles$modified_time[link_index] != meta_data[3]) {
        article_data <- try(c(meta_data, get_woz_article_data(full_link, section)))
        woz_articles <- rbind(woz_articles, article_data)
      }
    }
  }

  return(woz_articles)
}



#' Helper Function to extract meta data of raw woz html-document and return it as vector.
#'
#' @param full_link string with link to woz article
#'
get_woz_meta_data <- function(full_link) {

  meta_properties <- read_html(full_link) %>%
    html_elements("meta") %>%
    html_attr("property")

  meta_contents <- read_html(full_link) %>%
    html_elements("meta") %>%
    html_attr("content")

  section <- meta_contents[match("article:section", meta_properties)]
  published_time <- meta_contents[match("article:published_time", meta_properties)]
  modified_time <- meta_contents[match("article:modified_time", meta_properties)]

  return(c(section, published_time, modified_time))
}


#' Helper Function to extract data of raw woz html-document and return it as vector.
#'
#' @param full_link string with link to woz article
#' @param section sections to scrape on woz.ch. Default is all the permanent sections currently (2022-04-06) on the website.
#'
get_woz_article_data <- function(full_link, section = NULL) {
  raw <- read_html(full_link)

  field_issue <- html_element(raw, ".field-issue-ref") %>% html_text() # Zurechtschneiden



  title <- html_element(raw, ".article-title") %>% html_text() # Remove Whitespace around Title

  subtitle <- html_element(raw, ".field-subhead") %>% html_text() # Remove Whitespace around Title

  author <- html_element(raw, ".field-authorline") %>% html_text() # Remove Whitespace around Title --> Sp\u00e4ter auch noch Foto und Text separieren plus Co-Autoren identifizieren

  image_links <- html_elements(raw, "img") %>% html_attr("src") # noch aufteilen, dass nur die kommen, die es braucht.. oder ev. keine (gibt es teilweise mehr als ein bild?)

  image_link <- image_links[2]

  image_caption <- html_element(raw, ".field-image-caption") %>% html_text() # Remove Whitespace around Title

  text_titles <- read_html(full_link) %>%
    html_elements("h4") %>%
    html_text()
  text_titles <- paste(c("<p>", text_titles, "</p>"), collapse = " </p> <p>  ")

  text <- read_html(full_link) %>%
    html_elements("p") %>%
    html_text()
  text <- paste(c("<p>", text, "</p>"), collapse = " </p> <p>  ")

  prowoz <- html_element(raw, ".prowoz-infobox") %>% html_text()

  ressort <- section

  article_data <- c(full_link, field_issue, title, subtitle, author, image_link, image_caption, text_titles, text, prowoz, ressort)

  return(article_data)
}


#' Constructs a data frame with the required column names for the functions \code{get_woz_articles()}
#'
#' @return a data frame with 13 named empty columns (see details).
#' @details The following empty columns are contained in the returned df:
#' \itemize{
#'  \item{"section"}
#'  \item{"published_time"}
#'  \item{"modified_time"}
#'  \item{"full_link}
#'  \item{"field_issue"}
#'  \item{"title"}
#'  \item{"subtitle"}
#'  \item{"author}
#'  \item{"image_link}
#'  \item{"image_caption}
#'  \item{"text_titles}
#'  \item{"text"}
#'  \item{"prowoz"}
#' }
#' @export
#'
#' @examples
#' woz_articles_empty <- construct_woz_df_skeleton()
#'
construct_woz_df_skeleton <- function(){
  woz_articles <- data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

  names(woz_articles) <- c("section", "published_time", "modified_time", "full_link", "field_issue", "title", "subtitle", "author",
                           "image_link", "image_caption", "text_titles", "text", "prowoz")

  return(woz_articles)
}


#' Cleaning raw WOZ articles
#'
#' @param woz_articles a data frame containing raw WOZ articles as returned from the functions
#'                     \code{get_woz_articles}, \code{get_woz_articles_continously},
#'                     or \code{get_woz_articles_from_archive}.
#' @param no_updates Boolean that indicates if updated versions should be removed.
#'                   If TRUE, each article kept is unique (default), else also updated versions of articles are kept.
#'
#' @return returns the cleaned data frame with the same columns.
#' @export
#'
#' @examples
#'
#' # not run
#' # woz_articles_raw <- get_woz_articles()
#' # woz_articles_clean <- clean_woz_articles(woz_articles_raw)
#'
clean_woz_articles <- function(woz_articles, no_updates = TRUE) {

  if(no_updates) woz_articles <- woz_articles[!duplicated(woz_articles[ , "full_link"]),]

  woz_articles <- clean_woz_dates(woz_articles)
  woz_articles <- clean_woz_authors(woz_articles)

  # rename section (remove unicode)
  woz_articles$section[woz_articles$section == "Kultur\u2009/\u2009Wissen"] <- "Kultur/Wissen"


  return(woz_articles)
}


#' A helper function to clean WOZ articles
#'
#' Turns WOZ field issue into a day, month and year date column, in which the article was published.
#'
#' @param woz_articles a data frame containing raw WOZ articles as returned from the functions
#'                     \code{get_woz_articles}, \code{get_woz_articles_continously},
#'                     or \code{get_woz_articles_from_archive}
#'
#' @return the cleaned data frame \code{woz_articles}
#'
clean_woz_dates <- function(woz_articles) {

  woz_articles$day_published <- as.Date(substr(woz_articles$field_issue, 22, 31), format = "%d.%m.%Y")

  woz_articles$month_published <- as.Date(cut(woz_articles$day_published,
                                            breaks = "month"))

  woz_articles$year_published <- as.Date(cut(woz_articles$day_published,
                                           breaks = "year"))

  return(woz_articles)
}


#' Helper function to clean WOZ articles
#'
#' Removes redundant information from WOZ author column and separates collaborating authors into two
#' separate author columns.
#'
#' @param woz_articles a data frame containing raw WOZ articles as returned from the functions
#'                     \code{get_woz_articles}, \code{get_woz_articles_continously},
#'                     or \code{get_woz_articles_from_archive}.
#'
#' @return the cleaned data frame \code{woz_articles}.
#'
clean_woz_authors <- function(woz_articles) {

  woz_articles <- woz_articles %>%
    filter(author != "\n      ")

  woz_articles$author <- trimws(woz_articles$author, which = "both")
  woz_articles$author <- gsub("Von ", "", woz_articles$author)
  woz_articles$author <- gsub("Vom ", "", woz_articles$author)
  woz_articles$author <- gsub("von ", "", woz_articles$author)
  woz_articles$author <- gsub("VON ", "", woz_articles$author)
  woz_articles$author <- gsub("Mail an Autor:in", "", woz_articles$author)
  woz_articles$author <- gsub("Twitter Profil von Autor:in", "", woz_articles$author)
  woz_articles$author <- gsub("Twitter Profil Autor:in", "", woz_articles$author)
  woz_articles$author <- gsub("Text und Fotos", "", woz_articles$author)
  woz_articles$author <- gsub("Fotos", "", woz_articles$author)
  woz_articles$author <- gsub("Foto", "", woz_articles$author)
  woz_articles$author <- gsub("foto", "", woz_articles$author)
  woz_articles$author <- gsub("Text", "", woz_articles$author)
  woz_articles$author <- gsub("text", "", woz_articles$author)
  woz_articles$author <- gsub("Moderation", "", woz_articles$author)
  woz_articles$author <- gsub("Illustration", "", woz_articles$author)
  woz_articles$author <- gsub("Interview: ", "", woz_articles$author)
  woz_articles$author <- gsub("Interview", "", woz_articles$author)
  woz_articles$author <- gsub("interview", "", woz_articles$author)
  woz_articles$author <- gsub("Intrview: ", "", woz_articles$author)
  woz_articles$author <- gsub("Aufgezeichnet von ", "", woz_articles$author)
  woz_articles$author <- gsub("Gespr\u00e4ch: ", "", woz_articles$author)
  woz_articles$author <- gsub("Gespr\u00e4chsf\u00fchrung ", "", woz_articles$author)
  woz_articles$author <- gsub("Gespr\u00e4chsleitung: ", "", woz_articles$author)
  woz_articles$author <- gsub("Zusammengestellt von ", "", woz_articles$author)
  woz_articles$author <- gsub("Zusammengestellt ", "", woz_articles$author)
  woz_articles$author <- gsub("Recherche: ", "", woz_articles$author)
  woz_articles$author <- gsub("Rezensiert von ", "", woz_articles$author)
  woz_articles$author <- gsub("\\*", "", woz_articles$author)
  woz_articles$author <- gsub("[()]", "", woz_articles$author)
  woz_articles$author <- gsub("  ", " ", woz_articles$author)
  woz_articles$author <- trimws(woz_articles$author, which = "both")

  # remove location after comma
  for(i in 1:length(woz_articles$author)) {
    if(length(gregexpr(",",  woz_articles$author[i])[[1]]) == 1) {
      woz_articles$author[i] <- gsub("(.*),.*", "\\1", woz_articles$author[i])
    }
  }

  # manage collaborations (only if two authors, articles with three or more authors are ignored)
  woz_articles$n_authors <- rep(1, length(woz_articles$author))
  woz_articles$author_1 <- woz_articles$author
  woz_articles$author_2 <- rep(NA_character_, length(woz_articles$author))

  # separate authors into two columns
  woz_articles <- separate_woz_authors(woz_articles)

  woz_articles$author_1 <- gsub("(.*),.*", "\\1", woz_articles$author_1)
  woz_articles$author_2 <- gsub("(.*),.*", "\\1", woz_articles$author_2)

  return(woz_articles)
}


#' A helper function to clen WOZ authors
#'
#' Separates collaborating authors into two separate author columns.
#'
#' @param woz_articles a data frame containing raw WOZ articles as returned from the functions
#'                     \code{get_woz_articles}, \code{get_woz_articles_continously},
#'                     or \code{get_woz_articles_from_archive}
#'
#' @return the cleaned data frame \code{woz_articles}
#'
separate_woz_authors <- function(woz_articles) {
  undefined_articles <- 0
  for(i in 1:length(woz_articles$author)) {
    if(grepl("und", woz_articles$author[i])) {
      if(grepl(",", woz_articles$author[i])) {
        undefined_articles <- undefined_articles + 1
      }
      woz_articles$author_1[i] <- gsub("(.*) und .*", "\\1", woz_articles$author[i])
      woz_articles$author_2[i] <- gsub(".* und (.*)", "\\1", woz_articles$author[i])
      woz_articles$n_authors[i] <- 2
    }
    if(grepl(",",  woz_articles$author[i])[[1]]) {
      woz_articles$author[i] <- gsub("(.*),.*", "\\1", woz_articles$author[i])
    }
  }
  print(paste("Could not separate", undefined_articles, "articles due to more than two collaborators or ambigous author name."))
  return(woz_articles)
}

