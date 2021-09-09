# 
library(rvest);library(robotstxt);library(tidyverse);library(xml2)
library(rvest);library(stringr);library(rebus);library(lubridate);library(dplyr)
library(stringi); library(purrr)

# Let us check if we can scrape the data from the website using paths_allowed()
# from robotstxt package.
paths_allowed(
  paths = c("https://www.imdb.com/title/tt1825683/reviews?ref_=tt_ov_rt")
)
# Since it has returned TRUE, we will go ahead and download the web page using
# read_html() from xml2 package

#puts together dataframe with all the review information
pull_film_data <- function(webUrl_review, webUrl_film) {
  film <- read_html(webUrl_film) %>%
    html_nodes(".title_wrapper h1") %>%
    html_text()
  if (is.na(webUrl_review) == F) {
    Sys.sleep(sample(300, 1)/100)
    html_obj <- read_html(webUrl_review)
    pull_user <- function(html_obj) {
      html_obj %>%
        html_nodes(".display-name-link") %>%
        html_text()
    }
    safely_pullUser <- possibly(
      pull_user,
      NA
    )
    user <- safely_pullUser(html_obj)

    pull_revDate <- function(html_obj) {
      html_obj %>%
        html_nodes(".review-date") %>%
        html_text()
    }
    safely_pullDate <- possibly(
      pull_revDate,
      NA
    )
    revDate <- safely_pullDate(html_obj)
    
    pull_revText <- function(html_obj) {
      html_obj %>%
        html_nodes(".content") %>%
        html_text(".show-more_control")
    }
    safely_pull_revText <- possibly(
      pull_revText,
      NA
      )
    revText <- safely_pull_revText(html_obj)
    pull_rating <- function(html_obj) {
      map(1:25, function(i) {
        rating <- html_obj %>%
          html_nodes(".review-container") %>% as.character()
        rating <- rating[[i]] %>%
          read_html() %>%
          html_nodes(".rating-other-user-rating") %>%
          html_text() %>%
          stri_extract_all_regex("\\d+(?=/)") %>%
          unlist()
        if (is.character(rating) == T) {
          as.numeric(rating)
        } else {
          NA
        }
      }) %>%
        unlist()
    }
    safely_pullRating <- possibly(
      pull_rating,
      NA
    )
    rating <- safely_pullRating(html_obj)

    tibble(
      film = film,
      user = user,
      date_of_review = revDate,
      rating = rating,
      revText = revText,
      film_url = rep(webUrl_film, length(revText)),
      review_url = rep(webUrl_review, length(revText))
    )
  } else {
    tibble(film = film,
           user = NA, 
           date_of_review = NA, 
           rating = NA,
           revText = NA,
           film_url = webUrl_film,
           review_url = NA)
  }
}

#Below function provides the 50 pages from the search
readPageVids <- function(webPage) {
  webPage <- nextUrl
  nextUrl <<- read_html(webPage) %>%
    html_node(".lister-page-next") %>%
    html_attr('href') %>%
    paste0("https://www.imdb.com", .)
  allUrls <<- c(allUrls, nextUrl)
  #Sys.sleep(sample(300, 1)/100)
}

#run function to pull the 50 pages (below is kinda funky, but it works)
allUrls <- "holderSpot"
nextUrl <- "https://www.imdb.com/search/title/?title_type=feature&release_date=1998-01-01,1998-12-31&sort=boxoffice_gross_us,desc"
pagesOf50 <- map(1:19, ~ readPageVids(webPage))
webPage <- "https://www.imdb.com/search/title/?title_type=feature&release_date=1998-01-01,1998-12-31&sort=boxoffice_gross_us,desc"
pagesOf50 <- c(webPage, pagesOf50[[19]][2:20])

pull_film <- function(pagesOf50) {
  #gives us the 50 addresses for each page
  pagesForAllMovies <- map(pagesOf50, function(page) {
    read_html(page) %>%
      html_nodes(".lister-item-header a") %>%
      html_attr('href') %>%
      paste0("https://imdb.com", .)
  }) %>% unlist()
  
  #gives us the reviews for each film
  #importantly, assigns NA if the loop ever has an error (go back later by hand)
  pagesForEachReview <- map(pagesForAllMovies, function(page) {
    Sys.sleep(sample(300, 1)/100)
    #gives single address for review
    readHtml <- function (page) {
      read_html(page) %>%
        html_nodes(".subText a") %>%
        html_attr('href') %>%
        .[[2]] %>%
        #fix below
        paste0(page %>%
                 stri_replace_all_regex("/\\?.+", ""), 
               "/", .)
    }
    safely_readHtml <- possibly(readHtml, otherwise = NA)
    safely_readHtml(page)
  }) %>% unlist()

  #pulls out dataframe for each review
  map_df(1:length(pagesForEachReview), function(i) {
    pull_film_data(pagesForEachReview[i],
             pagesForAllMovies[i]
             )
  })
}
#below, create the dataframe
df_reviews_1998_2 <- pull_film(pagesOf50)

write.csv(df_reviews_1998_2, "imdb_reviews_1998_2.csv")
