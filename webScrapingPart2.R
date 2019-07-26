
# startng docker - enter below in terminal
#1 -  docker run -d -p 4445:4444 selenium/standalone-chrome
#2 - docker ps

##you may want to set your working directory here as well
# setwd("~/YourWorkingDirectory")


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

##creating the fll list

Cook_detail <- lapply(Search_results_url$url, function(i){
        
        webpage <- read_html(i)
        
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
                mutate(url = i)
        
})

##reformatting and cleaning
Cook_finaldf <- do.call(rbind, Cook_detail)   


Cook <- Cook_finaldf %>% 
        spread(Header, Detail) 




Cook_cleaned <- Cook %>% 
        mutate(Monument= gsub("Print Page ", "", Monument)) %>% 
        mutate(`Actual Event End Date:`=dmy(`Actual Event End Date:`)) %>% 
        mutate(`Actual Event STart Date:`=dmy(`Actual Event STart Date:`)) %>% 
        mutate(`Actual Monument Dedication Date:`=dmy(`Actual Monument Dedication Date:`)) %>% 
        mutate(`Approx. Monument Dedication Date:`=dmy(`Approx. Monument Dedication Date:`)) %>%
        mutate(`GPS Coordinates:`=gsub("Long", "\\1 Long", `GPS Coordinates:`)) %>%
        mutate(`GPS Coordinates:`=gsub("Note", "\\1 Note", `GPS Coordinates:`)) %>%
        separate(`GPS Coordinates:`, "Coordinates1", sep="Note", remove=FALSE) %>%
        mutate(Coordinates1 = gsub(" ", "", Coordinates1)) %>% 
        mutate(Coordinates1 = gsub(":", "", Coordinates1)) %>% 
        separate(Coordinates1, into = c("Lat", "Long"), "(?<=[a-z])(?=[0-9])") %>% 
        mutate(Lat = gsub("Lat", "", Lat)) %>% 
        mutate(Lat = gsub("Long", "", Lat)) %>% 
        mutate(Source = "Monument Australia") %>% 
        mutate(Date_Sourced = today(tzone = "")) %>% 
        rename(Actual_Event_End_Date ='Actual Event End Date:',
               Actual_Event_Start_Date ='Actual Event STart Date:',
               Monument_Dedication_Date = 'Actual Monument Dedication Date:',
               Address='Address:',
               Approx_Event_End_Date='Approx. Event End Date:',
               Approx_Event_Start_Date='Approx. Event Start Date:',
               Area = 'Area:',
               Monument_Designer = "Monument Designer:",
               Monument_Manufacturer = "Monument Manufacturer:",
               Monument_Theme = "Monument Theme:",
               Monument_Type = "Monument Type:",
               Link = 'Link:',
               State = "State:",
               Sub_Theme = 'Sub-Theme:') %>% 
        select(-'Approx. Monument Dedication Date:', -'GPS Coordinates:', -Actual_Event_End_Date, -Approx_Event_Start_Date) %>%
        mutate(Figure = "Cook") %>% 
        select(Figure, Monument, url, Monument_Dedication_Date,Address, State, Area, Lat,
               Long, Monument_Designer, Monument_Manufacturer, Monument_Type, Monument_Theme, Sub_Theme,
               Actual_Event_Start_Date, Approx_Event_End_Date, Date_Sourced, Source)


write_csv(Cook_cleaned, "Cook_data.csv")