#set WD
setwd("~/GitHub/rNBACommentActivity")

#load necessary packages
library(tidyverse)
library(jsonlite)
library(data.table)
library(extrafont)
library(magick)
library(scales)
library(lubridate)
library(zoo)

#create custom theme for plot
theme_owen <- function () { 
  theme_minimal(base_size=9, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}


#Get r/nba Daily Comment Activity From Pushshift API
url <- "http://api.pushshift.io/reddit/comment/search/?subreddit=nba&aggs=created_utc&frequency=day&after=365&size=0"
json_data <- fromJSON(paste(readLines(url), collapse=""))
df <- as.data.frame(json_data[["aggs"]][["created_utc"]])


#Reformat date
df$created_utc <- as.numeric(as.character(df$key))
df$date <- format(as.POSIXct(df$created_utc, origin = "1970-01-01", tz = "America/New_York", usetz=TRUE))
df$date <- ymd(as.Date(df$date))


View(df)

#Create a vector of important dates that we'll highlight later in the graph
important.dates <- c(as.Date("2018-07-01"), as.Date("2018-07-02"), as.Date("2018-07-17"), as.Date("2018-10-16"), as.Date("2018-10-20"),
                     as.Date("2019-01-03"), as.Date("2019-01-31"), as.Date("2019-02-06"),  as.Date("2019-04-13"),  as.Date("2019-04-27"), 
                     as.Date("2019-05-08"), as.Date("2019-06-05"),  as.Date("2019-06-10"),  as.Date("2019-06-13"), as.Date("2019-06-20"), 
                     as.Date("2019-06-30"))
  
#If date is equal to important date, then say so. Otherwise leave blank
df$fill <- ifelse(df$date %in% important.dates, "Important Date", " ")

#Chart daily comment activity from July 1, 2018 to June 30, 2019
df %>% filter(date >= "2018-07-01" & date < "2019-07-01") %>%
  ggplot(aes(x = date, y = doc_count, fill = fill)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c("#969696", "#de2d26")) +
  theme_owen() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma, limits = c(0, 200000), breaks = seq(0, 200000, 50000)) +
  annotate(geom = 'text', x = as.Date("2018-07-01"), y = 145000, label = "Free Agency Begins", family = "Gill Sans MT", hjust = -.05, size = 2.25) +
  annotate(geom = 'text', x = as.Date("2018-07-02"), y = 135000, label = "LeBron Signs w/ Lakers", family = "Gill Sans MT", hjust = -.05, size = 2.25) +
  annotate(geom = 'text', x = as.Date("2018-07-17"), y = 81000, label = "Kawhi Trade", family = "Gill Sans MT",  size = 2.25) +
  annotate(geom = 'text', x = as.Date("2018-10-16"), y = 48000, label = "Season Start", family = "Gill Sans MT", size = 2.25, hjust = 1) +
  annotate(geom = 'text', x = as.Date("2018-10-20"), y = 65000, label = "Spitgate", family = "Gill Sans MT", size = 2.25) + 
  annotate(geom = 'text', x = as.Date("2019-01-03"), y = 65500, label = "Harden Game Winner\nOver GSW", family = "Gill Sans MT", size = 2.25, lineheight = .85) +
  annotate(geom = 'text', x = as.Date("2019-01-31"), y = 82500, label = "Porzingis Trade", family = "Gill Sans MT", size = 2.25, hjust = 1) + 
  annotate(geom = 'text', x = as.Date("2019-02-07"), y = 125000, label = "Trade Deadline", family = "Gill Sans MT", size = 2.25) + 
  annotate(geom = 'text', x = as.Date("2019-04-13"), y = 78000, label = "Playoffs Start", family = "Gill Sans MT", size = 2.25, hjust = .75) + 
  annotate(geom = 'text', x = as.Date("2019-04-27"), y = 115000, label = "Game 7 SAS v. DEN\nGame 1 TOR v. PHI", family = "Gill Sans MT", size = 2.25, lineheight = .85) +
  annotate(geom = 'text', x = as.Date("2019-05-10"), y = 99000, label = "Game 5\nHOU v. GSW", family = "Gill Sans MT", size = 1.85, lineheight = .85) + 
  annotate(geom = 'text', x = as.Date("2019-05-28"), y = 137500, label = "Game 3\nNBA Finals", family = "Gill Sans MT", size = 2.25, lineheight = .85) + 
  annotate(geom = 'text', x = as.Date("2019-06-05"), y = 180000, label = "Game 5 / Shitgate", family = "Gill Sans MT", hjust = .9, size = 2.25) + 
  annotate(geom = 'text', x = as.Date("2019-06-13"), y = 190000, label = "Game 6", family = "Gill Sans MT", size = 2.25) + 
  annotate(geom = 'text', x = as.Date("2019-06-20"), y = 83000, label = "Draft", family = "Gill Sans MT", size = 2.25) +
  annotate(geom = 'text', x = as.Date("2019-07-01"), y = 167000, label = "Free\nAgency", family = "Gill Sans MT", size = 2.25, lineheight = .85) + 
  geom_segment(aes(x = as.Date("2019-06-05"), y = 128000, xend =as.Date("2019-06-05"), yend = 108000), size = .2,
             arrow = arrow(type = "closed",  length = unit(0.075, "cm"))) +
  labs(title = "r/NBA Comment Activity", 
       subtitle = "July 1, 2018 - June 30, 2019", 
       x = "", 
       y = "Total Comments", 
       caption = "") + 
  theme(plot.title = element_text(face = 'bold', hjust = .5), 
        plot.subtitle = element_text(hjust = .5), 
        legend.position = 'none') 

#Save graf
ggsave("CommentData.png", width = 8, height = 4)

#Load footer
footy <- image_read("footer.png")

#Recall saved graf
graf <- image_read("CommentData.png")

#Combine graf with footer 
img <- c(graf, footy)
image_composite(graf, footy, offset = "+0+1145") %>% image_write("CommentData.png")


## Most active days on the Sub 

df$RollingAverage <- rollmean(as.numeric(df$doc_count), k = 365, na.pad = TRUE, align = 'center')
df$Per <- as.numeric(df$doc_count) / df$RollingAverage

df %>% filter(date >= "2012-01-01") %>% 
  top_n(10, Per) %>% 
  ggplot(aes(x = reorder(as.factor(date), Per), y = Per)) + 
  geom_bar(stat = 'identity', fill ='floralwhite', color = 'black') + 
  theme_owen() + 
  coord_flip() + 
  scale_y_continuous(breaks = seq(0, 5, 1), labels = c("0", "1x", "2x", "3x", "4x", "5x")) +
  labs(x = "", 
       y = "Daily comment total relative to yearly rolling average", 
       title = "Top 10 Most Active Days On r/nba Since 2012\n(adjusted for subscriber inflation)") + 
  theme(plot.title = element_text(face = 'bold', hjust = .5, size = 16, lineheight = .85), 
        plot.subtitle = element_text(hjust = .5, size = 13), 
        legend.position = 'none') + 
  annotate(geom = 'text', x = 10, y = .05, label = "July 11, 2014: LeBron Announces His Return to Cleveland", fontface = 'bold', hjust = 0) +
  annotate(geom = 'text', x = 9, y = .05, label = "June 19, 2016: NBA Finals (CLE/GSW) Game 7", fontface = 'bold', hjust = 0) +
  annotate(geom = 'text', x = 8, y = .05, label = "July 4, 2016: KD Signs with the Warriors", fontface = 'bold', hjust = 0) +
  annotate(geom = 'text', x = 7, y = .05, label = "June 12, 2017: NBA Finals (CLE/GSW) Game 5", fontface = 'bold', hjust = 0) +
  annotate(geom = 'text', x = 6, y = .05, label = "June 17, 2013: NBA Finals (MIA/SAS) Game 6", fontface = 'bold', hjust = 0) +
  annotate(geom = 'text', x = 5, y = .05, label = "May 31, 2018: NBA Finals (CLE/GSW) Game 1", fontface = 'bold', hjust = 0) +
  annotate(geom = 'text', x = 4, y = .05, label = "June 20, 2012: NBA Finals (MIA/OKC) Game 5", fontface = 'bold', hjust = 0) +
  annotate(geom = 'text', x = 3, y = .05, label = "July 1, 2018: LeBron Signs with the Lakers", fontface = 'bold', hjust = 0) +
  annotate(geom = 'text', x = 2, y = .05, label = "June 19, 2013: NBA Finals (MIA/SAS) Game 7", fontface = 'bold', hjust = 0) +
  annotate(geom = 'text', x = 1, y = .05, label = "June 16, 2016: NBA Finals (CLE/GSW) Game 6", fontface = 'bold', hjust = 0) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))

ggsave("MostActive.png", width = 6, height = 6)

footy <- image_read("ShortFooter.png")

#Recall saved graf
graf <- image_read("MostActive.png")

#Combine graf with footer 
img <- c(graf, footy)
image_composite(graf, footy, offset = "+0+1740") %>% image_write("MostActive.png")
