##this R script accompanies my blog post 
# http://www.hellomister.com.au/data-blog/2019/7/23/novicewebscraping


# startng docker - enter below in terminal
#1 -  docker run -d -p 4445:4444 selenium/standalone-chrome
#2 - docker ps


# library(httr)
# library(RSelenium)
# library(rvest)
# library(xml2)
# library(tidyverse)
# library(lubridate)


##opening up server via docker
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()

##navigating to monument Aus site / search page

remDr$navigate("http://monumentaustralia.org.au/search")

# Give page time to load
Sys.sleep(5)

##take a screenshot to check
remDr$screenshot(display = TRUE)


##entering search term
searchname_field <- remDr$findElement(using = 'name', value = "keyword")
searchname_field$sendKeysToElement(list("\b", "Cook", "\uE007"))

Sys.sleep(5)
remDr$screenshot(display = TRUE)

##grabbing all the html and reating an xml file
results_html <- read_html(remDr$getPageSource()[[1]])



#grabbing url and completing it and creating first file we need 
Search_results_url <- results_html %>% 
        html_nodes(".morebutton") %>% 
        html_attr("href") %>% 
        enframe(name=NULL) %>% 
        rename(baseurl = value) %>% 
        mutate(urlpaste = "http://monumentaustralia.org.au/search/") %>% 
        mutate(url = (paste(urlpaste,baseurl, sep="" ))) %>% 
        select(-baseurl,-urlpaste)