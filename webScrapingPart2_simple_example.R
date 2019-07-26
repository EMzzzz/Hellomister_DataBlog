

library(httr)
library(RSelenium)
library(rvest)
library(xml2)
library(tidyverse)
library(lubridate)

        
webpage <- read_html("http://monumentaustralia.org.au/search/display/30861-%60last-man%60-and--%60last-shilling%60-monument")
        
Monument_Detail_Name <- html_nodes(webpage, "h1") %>% 
        html_text() %>% 
        enframe(name=NULL)%>% 
        rename(Monument= value)
        
        
monument_header <- html_nodes(webpage, "th") %>%
        html_text() %>% 
        enframe(name=NULL) %>% 
        rename(Header= value)
        
monument_detail <- html_nodes(webpage, "td") %>% 
        html_text() %>% 
        enframe(name=NULL) %>% 
        rename(Detail = value)
        
Monument_Detail_Info <- bind_cols(monument_header, monument_detail)
        
Monument_Detail_Info <- Monument_Detail_Info %>%
        mutate(Monument = Monument_Detail_Name$Monument) %>%
        select(Monument, Header, Detail) %>% 
        mutate(url = "http://monumentaustralia.org.au/search/http://monumentaustralia.org.au/search/display/30861-%60last-man%60-and--%60last-shilling%60-monument")
        

Monument <- Monument_Detail_Info %>% 
        spread(Header, Detail) 
        


