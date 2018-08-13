library(tidyverse)
library(rvest)
library(splashr)
library(pbapply)

# start splash in the terminal
# docker run -p 8050:8050 -p 5023:5023 scrapinghub/splash
# obviously this means having docker installed

# locate the very front page with all the womenswear
search_url <- 'https://www.jdwilliams.co.uk/shop/womens/2/_/N-1ytvx07/products/show.action?LpgUid=11117564&Rpp=48'

# here we need to get the total number of items shown in the top left of the page
# unfortunately the page numbering top right stops at 25...
number_items <- splash_local %>%
  splash_response_body(TRUE) %>%
  splash_enable_javascript(TRUE) %>%
  splash_plugins(TRUE) %>%
  splash_user_agent(ua_macos_chrome) %>%
  splash_go(search_url) %>%
  splash_wait(runif(1, 2, 3)) %>%
  splash_html() %>%
  html_nodes("div#numItemsContainer") %>%
  html_nodes("p") %>%
  html_nodes("span#numItems") %>%
  html_text() %>%
  gsub(",", "", .) %>%
  as.integer() 

# as we're using 48 items per page divide by that and round up
page_last <- ceiling(number_items/48)

# we now get the stub of the url before the pagination starts to use
stub_url <- substr(search_url, 1, nchar(search_url)-6)

# a helper function that will scrape all the product links on each page number
scrape_page_links <- function(page) {

  # deal with the fact if it's page 1 or page after
  if(page == 1) {
    page_url <- paste0(stub_url,"&Rpp=48")
    } else {
    page_url <- paste0(stub_url,"&Nao=",as.character((page-1)*48),"&Rpp=48")
  }

  # scrape the link, splash is a faff but does work well
  links <- splash_local %>%
      splash_response_body(TRUE) %>%
      splash_enable_javascript(TRUE) %>%
      splash_plugins(TRUE) %>%
      splash_user_agent(ua_macos_chrome) %>%
      splash_go(page_url) %>%
      splash_wait(runif(1, 2, 3)) %>%
      splash_html() %>%
      html_nodes("div#browseContent-7") %>%
      html_nodes("p.productTitle") %>%
      html_nodes("a") %>%
      html_attr("href")

  # add in jdwilliams
  links <- paste0("https://www.jdwilliams.co.uk", links)

  # we all love a data frame
  links_df <- data.frame(links, stringsAsFactors = FALSE)

  # done
  return(links_df)
}

# scrape all pages from first to last, using pbapply for the sweet progress bar
womenswear <- pblapply(seq(1,page_last), scrape_page_links) %>% 
  bind_rows()

# save off
saveRDS(womenswear, "data/womenswear_links.rds")

# helper function to scrape all the image urls on an individual pdp
scrape_img_urls <- function(url) {

  # get all the image urls on a page
  imgs <- splash_local %>%
    splash_response_body(TRUE) %>%
    splash_enable_javascript(TRUE) %>%
    splash_plugins(TRUE) %>%
    splash_user_agent(ua_macos_chrome) %>%
    splash_go(url) %>%
    splash_wait(runif(1, 0, 1)) %>%
    splash_html() %>%
    html_nodes("div.rsThumbsContainer") %>%
    html_nodes("img") %>%
    html_attr("src")

  # if no images are returned make a note otherwise make a dataframe
  if(is.na(imgs[1])) {
    df <- data.frame(prod_url = url,
                     image_url = "no_img_url",
                     stringsAsFactors = FALSE)
    } else {
    df <- data.frame(prod_url = rep(url, length(imgs)), 
                     image_url = imgs, 
                     stringsAsFactors = FALSE)
  }
  
  # return the data frame
  return(df)

}

# this saves as we go in case the website keels over
scrape_and_save <- function(url) {

  print(which(womenswear$links == url))
  
  if(which(womenswear$links == url) == 1) {
    df <- map_dfr(url, scrape_img_urls)
    saveRDS(df, "data/img_urls.rds")
  } else {
    load <- readRDS("data/img_urls.rds")
    df <- map_dfr(url, scrape_img_urls)
    combine <- bind_rows(load, df)
    saveRDS(combine, "data/img_urls.rds")
  }
}

# scrape them all. nb doing ~6400 takes around 6 hours
pblapply(womenswear$links, scrape_and_save)

ww_img_links <- readRDS("data/img_urls.rds")

# swap out the word thumb for the word main_product
ww_img_links$image_url <- img_urls$image_url %>%
  gsub("thumb", "main_product", .) %>%
  gsub("https://", "", .) %>%
  gsub("//", "/", .) %>%
  paste0("https://", .)

# letters determine whether its just product or includes model (there is a system)
ww_img_links$letter <- substr(ww_img_links$image_url, nchar(ww_img_links$image_url)-4, nchar(ww_img_links$image_url)-4)

# validates whether the image_url has the expected pattern. if not i'll have to deal with that later.
ww_img_links$valid <- grepl("[a-z]{1,2}[0-9]{2}[a-z]{2}[0-9]{5,6}[a-z].jpg", ww_img_links$image_url)

# extract the product number for cross referencing
ww_img_links$prod_num <- str_extract(ww_img_links$prod_url, "[a-z]{2}[0-9]{3}")

# extract the file name. let's use the handy regexp from before
ww_img_links$filename <- substr(ww_img_links$image_url, 59, nchar(ww_img_links$image_url))

# save off
saveRDS(ww_img_links, "data/ww_img_links.rds")

