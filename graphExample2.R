##libraries
library(tidyverse)
library(lubridate)
library(ggraph)
library(tidygraph)
library(scales)
library(grid)
library(wesanderson)
library(egg)

##data file import

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



## create your graph files
cook_tidy <- tbl_graph(edges=cook_edge, directed = TRUE)


##plot your arc plot



##theme
cook_theme <-  theme(axis.line=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     panel.background=element_blank(),
                     panel.border=element_blank(),
                     panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),
                     plot.background=element_blank(),
                     plot.title = element_text(family="Helvetica Neue Light", size=20, face="plain"),
                     plot.subtitle =  element_text(family="Helvetica Neue Light", size=12.9, face="plain"),
                     legend.text =  element_text(family="Helvetica Neue Light", face="plain"),
                     legend.title = element_text(family="Helvetica Neue Light", face="plain"),
                     axis.text = element_text(family="Helvetica Neue Light", face="plain", size=6),
                     legend.key = element_blank())

# names(wes_palettes)
##creating object with HEX colours from Wes Andreson Colour Palette /- here using Moonrise 3
mr3 <- wes_palettes$Moonrise3
# wes_palette("Moonrise3")

cook_dot <- ggplot(cookExample, aes(Dedication_Year, fill=Figure)) + 
        geom_vline(xintercept = c(1770, 1870, 1970, 2000), colour="lightgray")+
        geom_dotplot(binwidth = .1, stackgroups = TRUE, binpositions = "all", dotsize = 10, stackdir = "down", stroke=0)+
        scale_x_continuous(limits=c(1770, 2020), breaks = c(1770, 1870, 1970, 2000)) +
        cook_theme +
        theme(legend.position = "none",
              title = element_blank()) +
        scale_fill_manual(values = mr3[c(1,3,5)]) ##this selects the 1, 2 and 3 HEX colours from the palette in object mr3
        
        

cook_arc <- ggraph(cook_tidy, layout = 'linear') + 
        geom_vline(xintercept = c(1770, 1870, 1970, 2000), colour="lightgray")+
        geom_edge_arc(aes(width=weight,edge_colour=Figure), alpha=.5) +
        scale_edge_colour_manual(values = mr3[c(1,3,5)], name = "Figure")+
        scale_edge_width(name="Number of Monuments")+
        scale_x_continuous(limits=c(1770, 2020), breaks = c(1770, 1870, 1970, 2000), position = "top") +
        cook_theme +
        labs(title="Visualising Cook Memorials",
             subtitle="Monuments dedicated to the voyage of the Endeavour from 1770 until today") 

print <- ggarrange(cook_arc, cook_dot, heights = c(10, 4))





        



