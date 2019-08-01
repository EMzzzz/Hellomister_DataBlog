
library(tidyverse)
library(lubridate)
library(ggraph)
library(tidygraph)
library(scales)

cookExample <- readr::read_csv("https://raw.githubusercontent.com/KellyTall/Hellomister_DataBlog/master/cookExample.csv")

# write_csv(cookExample, "cookExample.csv")

# View(cookExample)

# prepare edge file
cook_edge <- cookExample %>% 
        mutate(from = 1770) %>% 
        select(from, Dedication_Year, Figure, Monument_Type) %>% 
        rename(to=Dedication_Year) %>% 
        mutate(to=as.numeric(to)) %>% 
        group_by(Figure, Monument_Type, from, to) %>% 
        summarise(weight = n()) %>% 
        na.omit()

# View(cook_edge)

## create your graph files
cook_tidy <- tbl_graph(edges=cook_edge, directed = TRUE)


##plot your graphs

cook1 <- ggraph(cook_tidy, layout = 'linear') + 
        geom_edge_arc()

cook2 <- ggraph(cook_tidy, layout = 'linear') + 
        geom_edge_arc(aes(width=weight))

# cook2

cook3 <- ggraph(cook_tidy, layout = 'linear') + 
                geom_edge_arc(aes(width=weight), alpha=.5) 
        
# cook3

cook4 <- ggraph(cook_tidy, layout = 'linear') + 
        geom_edge_arc(aes(width=weight, colour=Figure), alpha=.5)

# cook4

cook5 <- ggraph(cook_tidy, layout = 'linear') + 
        geom_edge_arc(aes(width=weight, colour=Figure), alpha=.5) +
        theme_bw()
# cook5


cook6 <- ggraph(cook_tidy, layout = 'linear') + 
        geom_edge_arc(aes(width=weight, colour=Monument_Type), alpha=.5) +
        theme_bw()
# cook6