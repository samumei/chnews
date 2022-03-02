# Generic functions
# Author: Samuel Meier
# Date: 2022-04-06

# Introduction ----------------------------------

# Generic functions to analyze the articles irrespective of the media outlet.

# Required Packages ------------------------------------

#' @import dplyr
#' @import ggplot2
#' @import lubridate
NULL

# Functions ------------------------------------

#' Plotting Article Frequency
#'
#' Plots the number of articles published per day, month, or year in a line chart.
#'
#' @param articles data frame with articles to be counted and plotted. Needs to contain a date (yyyy-mm-dd) column
#'                 \code{month_published}, and optionally \code{day_published} or \code{year_published},
#'                 if articles should be plotted per day or year.
#' @param period string indicating the time period over which the articles should be published. Either articles are counted per
#'               "month" (default), "day", or  "year".
#' @param from string i.e. date of format "yyyy-mm-dd" indicating the publishing date from which on articles should be evaluated and plotted.
#'             For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.
#' @param to string i.e. date of format "yyyy-mm-dd" indicating the publishing date until which articles should be evaluated and plotted.
#'             For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.
#'
#' @return a ggplot line chart with the article frequency
#' @export
#'
#' @examples
#'
#' ## not run
#' # woz_articles_raw <- get_woz_articles()
#' # woz_articles_clean <- clean_woz_articles(woz_articles_raw)
#' # plot_article_frequency(woz_articles_clean)
#'
plot_article_frequency <- function(articles, period = "month", from = NULL, to = NULL) {
  if(!is.null(from) & !is.null(to)) {
    if(period == "day") articles <- try(filter(articles, day_published >= from & day_published <= to))
    else{
      if(period == "month") articles <- try(filter(articles, month_published >= from & month_published <= to))
      else{
        if(period == "year") articles <- try(filter(articles, year_published >= from & year_published <= to))
      }
    }
  }

  if(period == "day") {
    articles_count <- articles %>% count(day_published)
  }
  else {
    if(period == "month") {
      articles_count <- articles %>% count(month_published)
      articles_count$day_published <- articles_count$month_published
    }
    else {
      if(period == "year") {
        articles_count <- articles %>% count(year_published)
        articles_count$day_published <- articles_count$year_published
      }
      else {
        stop("Unknown parameter provided for \"period\". Choose either \"day\", \"month\", \"year\".")
      }
    }
  }

  ggplot(articles_count, aes(x = day_published, y = n)) +
    geom_line() +
    scale_x_date(date_labels = "%Y-%m-%d")
}


#' Plotting Article per Section Frequency
#'
#' Plots the number of articles published in each section (german "Ressort") either in total in a bar chart or
#' per day, month, or year in a line chart.
#'
#' @param articles data frame with articles to be counted and plotted. Needs to contain a date (yyyy-mm-dd) column
#'                 \code{month_published}, and optionally \code{day_published} or \code{year_published},
#'                 if articles should be plotted per day or year.
#' @param period string indicating the time period over which the articles should be published. Either articles are counted per
#'               overall indicated with "all" (default) or per "day", "month", or "year".
#' @param top integer indicating the which sections should be plotted. If 5 (default), the five sections with overall
#'            the most number of articles are plotted.
#' @param specific_section vector string of length 1 or more, containing the sections to be plotted. If NULL (default),
#'                         the five (or as indicated in "top") most frequent sections are being plotted.
#' @param relative IF FALSE (default) the absolute number of articles per section are plotted.
#'                 If TRUE, they are plotted relatively to all articles in the selected sections.
#' @param from string i.e. date of format "yyyy-mm-dd" indicating the publishing date from which on articles should be evaluated and plotted.
#'             For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.
#' @param to string i.e. date of format "yyyy-mm-dd" indicating the publishing date until which articles should be evaluated and plotted.
#'             For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.
#'
#' @return a ggplot bar or line chart with the article frequency per section
#' @export
#'
#' @examples
#'
#' ## not run
#' # woz_articles_raw <- get_woz_articles()
#' # woz_articles_clean <- clean_woz_articles(woz_articles_raw)
#' # plot_article_frequency(woz_articles_clean)
#'
plot_section_frequency <- function(articles,
                                   period = "all",
                                   top = 5,
                                   specific_section = NULL,
                                   relative = FALSE,
                                   from = NULL,
                                   to = NULL) {

  # keep only top sections
  if(is.null(specific_section)) {
    sections_to_keep <- articles %>%
      select(section, day_published, month_published, year_published) %>%
      filter(!is.na(section)) %>%
      group_by(section) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(top)
    sections_to_keep <- unique(sections_to_keep$section)
  }
  else {
    sections_to_keep <- specific_section
  }
  articles <- filter(articles, section %in% sections_to_keep)

  if(!is.null(from) & !is.null(to)) {
    if(period == "day") articles <- try(filter(articles, day_published >= from & day_published <= to))
    else{
      if(period == "month") articles <- try(filter(articles, month_published >= from & month_published <= to))
      else{
        if(period == "year") articles <- try(filter(articles, year_published >= from & year_published <= to))
      }
    }
  }


  if(period == "all") {
    articles_by_section <- articles %>%
      group_by(section) %>%
      summarise(n = n())
    ggplot(data = articles_by_section, aes(x = section, y = n)) + geom_col()
  }
  else {
    articles <- filter(articles, !is.na(day_published))

    if(period == "day") {
      articles_by_section <- articles %>%
        group_by(section, day_published) %>%
        summarise(n = n())
      if(relative) {
        ggplot(data = articles_by_section, aes(x = day_published, y = n, color = section)) +
          geom_area(aes(color = section, fill = section), position="fill", stat="identity")
      }
      else {
        ggplot(data = articles_by_section, aes(x = day_published, y = n, color = section)) +
          geom_line()
      }
    }
    else{
      if(period == "month") {
        articles_by_section <- articles %>%
          group_by(section, month_published) %>%
          summarise(n = n())
        if(relative) {
          ggplot(data = articles_by_section, aes(x = month_published, y = n, color = section)) +
            geom_area(aes(color = section, fill = section), position="fill", stat="identity")
        }
        else {
          ggplot(data = articles_by_section, aes(x = month_published, y = n, color = section)) +
            geom_line()
        }
      }
      else {
        if(period == "year") {
          articles_by_section <- articles %>%
            group_by(section, year_published) %>%
            summarise(n = n())
          if(relative) {
            ggplot(data = articles_by_section, aes(x = year_published, y = n, color = section)) +
              geom_area(aes(color = section, fill = section), position="fill", stat="identity")
          }
          else {
            ggplot(data = articles_by_section, aes(x = year_published, y = n, color = section)) +
              geom_line()
          }
        }
      }
    }
  }
}


#' Plotting Article per Author Frequency
#'
#' Plots the number of articles published by each author either in total in a bar chart or
#' per day, month, or year in a line chart.
#'
#' @param articles data frame with articles to be counted and plotted. Needs to contain a date (yyyy-mm-dd) column
#'                 \code{month_published}, and optionally \code{day_published} or \code{year_published},
#'                 if articles should be plotted per day or year. Also needs columns \code{author_1} and \code{author_2}
#'                 to count the articles per author.
#' @param period string indicating the time period over which the articles should be published. Either articles are counted per
#'               overall indicated with "all" (default) or per "day", "month", or "year".
#' @param top integer indicating which authors should be plotted. If 5 (default), the five authors with overall
#'            the most number of articles are plotted.
#' @param specific_author vector string of length 1 or more, containing the authors to be plotted. If NULL (default),
#'                         the five (or as indicated in "top") most frequent authors are being plotted.
#' @param from string i.e. date of format "yyyy-mm-dd" indicating the publishing date from which on articles should be evaluated and plotted.
#'             For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.
#' @param to string i.e. date of format "yyyy-mm-dd" indicating the publishing date until which articles should be evaluated and plotted.
#'             For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.
#'
#' @return a ggplot bar or line chart with the article frequency per author
#' @export
#'
#' @examples
#'
#' ## not run
#' # woz_articles_raw <- get_woz_articles()
#' # woz_articles_clean <- clean_woz_articles(woz_articles_raw)
#' # plot_article_frequency(woz_articles_clean)
#'
plot_author_frequency <- function(articles, period = "all", top = 5, specific_author = NULL, from = NULL, to = NULL) {

  articles <- select(articles,
                     day_published, month_published, year_published,
                     n_authors, author_1, author_2)

  if(!is.null(from) & !is.null(to)) {
    if(period == "day") articles <- try(filter(articles, day_published >= from & day_published <= to))
    else{
      if(period == "month") articles <- try(filter(articles, month_published >= from & month_published <= to))
      else{
        if(period == "year") articles <- try(filter(articles, year_published >= from & year_published <= to))
      }
    }
  }

  for(i in 1:length(articles$author_1)) {
    if(!is.na(articles$author_2[i])) {
      article_to_add <- articles[i,]
      author_1 <- article_to_add$author_1
      article_to_add$author_1 <- article_to_add$author_2
      article_to_add$author_2 <- author_1
      articles <- rbind(articles, article_to_add)
      articles <- filter(articles, author_1 != "")
    }
  }

  # keep only top authors
  if(!is.null(specific_author)) {
    authors_to_keep <- specific_author
  }
  else {
    authors_to_keep <- articles %>%
      filter(!is.na(author_1)) %>%
      group_by(author_1) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(top)
    authors_to_keep <- unique(authors_to_keep$author_1)
  }
  articles <- filter(articles, author_1 %in% authors_to_keep)

  if(period == "all") {
    articles_by_author <- articles %>%
      group_by(author_1) %>%
      summarise(n = n())
    ggplot(data = articles_by_author, aes(x = author_1, y = n)) + geom_col() + labs(x = "author")
  }
  else {
    articles <- filter(articles, !is.na(day_published))

    if(period == "day") {
      articles_by_author <- articles %>%
        group_by(author_1, day_published) %>%
        summarise(n = n())
      ggplot(data = articles_by_author, aes(x = day_published, y = n, color = author_1)) + geom_line() + labs(x = "author")
    }
    else {
      if(period == "month") {
        articles_by_author <- articles %>%
          group_by(author_1, month_published) %>%
          summarise(n = n())
        ggplot(data = articles_by_author, aes(x = month_published, y = n, color = author_1)) + geom_line() + labs(x = "author")
      }
      else {
        if(period == "year") {
          articles_by_author <- articles %>%
            group_by(author_1, year_published) %>%
            summarise(n = n())
          ggplot(data = articles_by_author, aes(x = year_published, y = n, color = author_1)) + geom_line() + labs(x = "author")
        }
      }
    }
  }
}


#' Plotting Frequency of Co-Authored Articles per Author
#'
#' Plots the number of articles published that were co-authored per author either in total in a bar chart or
#' per day, month, or year in a line chart.
#'
#' @param articles data frame with articles to be counted and plotted. Needs to contain a date (yyyy-mm-dd) column
#'                 \code{month_published}, and optionally \code{day_published} or \code{year_published},
#'                 if articles should be plotted per day or year. Also needs columns \code{author_1} and \code{author_2}
#'                 to count the articles per author.
#' @param period string indicating the time period over which the articles should be published. Either articles are counted per
#'               overall indicated with "all" (default) or per "day", "month", or "year".
#' @param top integer indicating which authors should be plotted. If 5 (default), the five authors with overall
#'            the most number of co-authored articles are plotted.
#' @param specific_author vector string of length 1 or more, containing the authors to be plotted. If NULL (default),
#'                         the five (or as indicated in "top") most frequent authors are being plotted.
#' @param from string i.e. date of format "yyyy-mm-dd" indicating the publishing date from which on articles should be evaluated and plotted.
#'             For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.
#' @param to string i.e. date of format "yyyy-mm-dd" indicating the publishing date until which articles should be evaluated and plotted.
#'             For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.
#'
#'
#' @return a ggplot bar or line chart with the article frequency per author
#' @export
#'
#' @examples
#'
#' ## not run
#' # woz_articles_raw <- get_woz_articles()
#' # woz_articles_clean <- clean_woz_articles(woz_articles_raw)
#' # plot_article_frequency(woz_articles_clean)
#'
plot_main_collaborators <- function(articles,
                                   period = "all",
                                   top = 5,
                                   specific_author = NULL,
                                   from = NULL,
                                   to = NULL) {

  articles <- select(articles,
                     day_published, month_published, year_published,
                     n_authors, author_1, author_2)

  if(!is.null(from) & !is.null(to)) {
    if(period == "day") articles <- try(filter(articles, day_published >= from & day_published <= to))
    else{
      if(period == "month") articles <- try(filter(articles, month_published >= from & month_published <= to))
      else{
        if(period == "year") articles <- try(filter(articles, year_published >= from & year_published <= to))
      }
    }
  }


  for(i in 1:length(articles$author_1)) {
    if(!is.na(articles$author_2[i])) {
      article_to_add <- articles[i,]
      author_1 <- article_to_add$author_1
      article_to_add$author_1 <- article_to_add$author_2
      article_to_add$author_2 <- author_1
      articles <- rbind(articles, article_to_add)
      articles <- filter(articles, author_1 != "")
    }
  }

  # keep only top authors
  if(is.null(specific_author)) {
    authors_to_keep <- articles %>%
      filter(n_authors == 2) %>%
      group_by(author_1) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(top)
    authors_to_keep <- unique(authors_to_keep$author_1)
  }
  else {
    authors_to_keep <- specific_author
  }
  articles <- filter(articles, author_1 %in% authors_to_keep)

  if(period == "all") {
    articles_by_author <- articles %>%
      group_by(author_1) %>%
      summarise(n = n())
    ggplot(data = articles_by_author, aes(x = author_1, y = n)) + geom_col() + labs(x = "author")
  }
  else {
    articles <- filter(articles, !is.na(day_published))

    if(period == "day") {
      articles_by_author <- articles %>%
        group_by(author_1, day_published) %>%
        summarise(n = n())
      ggplot(data = articles_by_author, aes(x = day_published, y = n, color = author_1)) + geom_line() + labs(x = "author")
    }
    else {
      if(period == "month") {
        articles_by_author <- articles %>%
          group_by(author_1, month_published) %>%
          summarise(n = n())
        ggplot(data = articles_by_author, aes(x = month_published, y = n, color = author_1)) + geom_line() + labs(x = "author")
      }
      else {
        if(period == "year") {
          articles_by_author <- articles %>%
            group_by(author_1, year_published) %>%
            summarise(n = n())
          ggplot(data = articles_by_author, aes(x = year_published, y = n, color = author_1)) + geom_line() + labs(x = "author")
        }
      }
    }
  }
}




#' Plotting Frequency of Topics in Articles
#'
#' @param articles data frame with articles to be counted and plotted. Needs to contain a date (yyyy-mm-dd) column
#'                 \code{month_published}, and optionally \code{day_published} or \code{year_published},
#'                 if articles should be plotted per day or year.
#' @param topic string vector of length 1 or more with the words to search in the article texts.
#' @param period string indicating the time period over which the articles should be published. Either articles are counted per
#'               overall indicated with "all" (default) or per "day", "month", or "year".
#' @param distinct_only If FALSE (default), also plots articles where a combination of words in \code{topic} is present.
#'                      If TRUE, plots only those articles where only one of the words is present.
#' @param show_total If FALSE (default), articles are plotted by topic. If TRUE all articles are plotted that contain at least one word in \code{topic}.
#' @param relative IF FALSE (default) the absolute number of articles per section are plotted.
#'                 If TRUE, they are plotted relatively to all articles in the selected sections.
#' @param from string i.e. date of format "yyyy-mm-dd" indicating the publishing date from which on articles should be evaluated and plotted.
#'             For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.
#' @param to string i.e. date of format "yyyy-mm-dd" indicating the publishing date until which articles should be evaluated and plotted.
#'             For the time period to be evaluated both `from` and `to` need to contain a date. The time period must include at least one first of January.
#'
#'
#' @return a ggplot bar or line chart with the article frequency per section
#' @export
#'
#' @examples
#'
#' ## not run
#' # woz_articles_raw <- get_woz_articles()
#' # woz_articles_clean <- clean_woz_articles(woz_articles_raw)
#' # topic <- c("Umweltschutz", "Altersvorsorge", "Krankenkassen")
#' # plot_topic_frequency(woz_articles_clean, topic)
#'
plot_topic_frequency <- function(articles,
                                 topic,
                                 period = "all",
                                 distinct_only = FALSE,
                                 show_total = FALSE,
                                 relative = FALSE,
                                 from = NULL,
                                 to = NULL) {

  if(!is.null(from) & !is.null(to)) {
    if(period == "day") articles <- try(filter(articles, day_published >= from & day_published <= to))
    else{
      if(period == "month") articles <- try(filter(articles, month_published >= from & month_published <= to))
      else{
        if(period == "year") articles <- try(filter(articles, year_published >= from & year_published <= to))
      }
    }
  }

  n_topics <- length(topic)
  # find articles with these topics
  if(n_topics > 1) {
    topic_filter <- paste(topic, collapse = "|")
  }
  else topic_filter <- topic

  articles <- articles %>%
    filter(grepl(c(topic_filter), text))

  articles$topic <- rep(NA, length(articles$text))

  if(n_topics > 1) {
    for(i in 1:n_topics) {
      for(j in 1:length(articles$text))
        if(grepl(topic[i], articles$text[j])) { # what if more than one is present, maybe loop different and add a column for each topic??
          if(is.na(articles$topic[j])) articles$topic[j] <- topic[i]
          else articles$topic[j] <- paste0(articles$topic[j], " & ", topic[i])
        }
    }
  }
  else {
    articles$topic <- topic
  }

  if(distinct_only) articles <- filter(articles, !grepl("&", topic))

  if(show_total) {
    if(period == "all") period <- "month"
    plot_article_frequency(articles = articles, period, from, to)
  }
  else {
    if(period == "all") {
      articles_by_topic <- articles %>%
        group_by(topic) %>%
        summarise(n = n())
      ggplot(data = articles_by_topic, aes(x = topic, y = n)) + geom_col()
    }
    else {
      articles <- filter(articles, !is.na(day_published))

      if(period == "day") {
        articles_by_topic <- articles %>%
          group_by(topic, day_published) %>%
          summarise(n = n())
        if(relative) {
          ggplot(data = articles_by_topic, aes(x = day_published, y = n, color = topic)) +
            geom_area(aes(color = topic, fill = topic), position="fill", stat="identity")
        }
        else {
          ggplot(data = articles_by_topic, aes(x = day_published, y = n, color = topic)) +
            geom_line()
        }
      }
      else{
        if(period == "month") {
          articles_by_topic <- articles %>%
            group_by(topic, month_published) %>%
            summarise(n = n())
          if(relative) {
            ggplot(data = articles_by_topic, aes(x = month_published, y = n, color = topic)) +
              geom_area(aes(color = topic, fill = topic), position="fill", stat="identity")
          }
          else {
            ggplot(data = articles_by_topic, aes(x = month_published, y = n, color = topic)) +
              geom_line()
          }
        }
        else {
          if(period == "year") {
            articles_by_topic <- articles %>%
              group_by(topic, year_published) %>%
              summarise(n = n())
            if(relative) {
              ggplot(data = articles_by_topic, aes(x = year_published, y = n, color = topic)) +
                geom_area(aes(color = topic, fill = topic), position="fill", stat="identity")
            }
            else {
              ggplot(data = articles_by_topic, aes(x = year_published, y = n, color = topic)) +
                geom_line()
            }
          }
        }
      }
    }
  }
}


