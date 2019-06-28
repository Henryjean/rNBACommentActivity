setwd("~/GitHub/rNBACommentActivity")

library(tidyverse)
library(jsonlite)
library(data.table)
library(extrafont)
library(magick)
library(scales)
library(lubridate)

theme_owen <- function () { 
  theme_minimal(base_size=9, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}


# Get r/nba Daily Comment Activity From Pushshift.io
url <- "http://api.pushshift.io/reddit/comment/search/?subreddit=nba&aggs=created_utc&frequency=day&after=365&size=0"
json_data <- fromJSON(paste(readLines(url), collapse=""))
df <- as.data.frame(json_data[["aggs"]][["created_utc"]])


#Reformat date
df$created_utc <- as.numeric(as.character(df$key))
df$date <- format(as.POSIXct(df$created_utc, origin = "1970-01-01", tz = 'America/New_York', usetz=TRUE))
df$date <- ymd(as.Date(df$date))

#Create a vecotor of important dates that we'll highlight later
important.dates <- c(as.Date("2019-02-06"), as.Date("2019-06-13"), as.Date("2019-06-10"), as.Date("2018-07-01"), 
           as.Date("2018-07-02"), as.Date("2018-07-17"), as.Date("2018-10-16"), as.Date("2019-06-20"), 
           as.Date("2019-04-27"), as.Date("2019-01-03"))

#If date is equal to important date, then highlight 
df$fill <- ifelse(df$date %in% important.dates, "Important Date", " ")

#Chart daily comment activity from July 1, 2018 to June 30, 2019
df %>% filter(date >= "2018-07-01") %>% 
  ggplot(aes(x = date, y = doc_count, fill = fill)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c("#969696", "#de2d26")) +
  theme_owen() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma, limits = c(0, 200000), breaks = seq(0, 200000, 50000)) +
  annotate(geom = 'text', x = as.Date("2019-02-07"), y = 125000, label = "Trade Deadline", family = "Gill Sans MT", size = 2.5) + 
  annotate(geom = 'text', x = as.Date("2019-06-13"), y = 190000, label = "Game 6 NBA Finals", family = "Gill Sans MT", size = 2.5) + 
  annotate(geom = 'text', x = as.Date("2019-06-07"), y = 180000, label = "Game 5 NBA Finals", family = "Gill Sans MT", hjust = 1, size = 2.5) + 
  annotate(geom = 'text', x = as.Date("2018-07-01"), y = 145000, label = "Free Agency Begins", family = "Gill Sans MT", hjust = -.05, size = 2.5) +
  annotate(geom = 'text', x = as.Date("2018-07-02"), y = 135000, label = "LeBron Signs w/ Lakers", family = "Gill Sans MT", hjust = -.05, size = 2.5) +
  annotate(geom = 'text', x = as.Date("2018-07-17"), y = 75000, label = "Kawhi Trade", family = "Gill Sans MT", hjust = -.05, size = 2.5) +
  annotate(geom = 'text', x = as.Date("2018-10-16"), y = 50000, label = "Season Start", family = "Gill Sans MT", size = 2.5, hjust = 1) +
  annotate(geom = 'text', x = as.Date("2019-06-20"), y = 83000, label = "Draft", family = "Gill Sans MT", size = 2.5) +
  annotate(geom = 'text', x = as.Date("2019-04-27"), y = 115000, label = "Game 7 SAS v. DEN\nGame 1 TOR v. PHI", family = "Gill Sans MT", size = 2.5, lineheight = .85) +
  annotate(geom = 'text', x = as.Date("2019-01-03"), y = 65500, label = "Harden Game Winner\nOver GSW", family = "Gill Sans MT", size = 2.5, lineheight = .85) +
  labs(title = "r/NBA Comment Activity", 
       subtitle = "July 1, 2018 - June 30, 2019", 
       x = "", 
       y = "Total Comments", 
       caption = "") + 
  theme(plot.title = element_text(face = 'bold', hjust = .5), 
        plot.subtitle = element_text(hjust = .5), 
        legend.position = 'none') 


ggsave("CommentData.png", width = 8, height = 4)



footy <- image_read("footer.png")

graf <- image_read("CommentData.png")
img <- c(graf, footy)


image_composite(graf, footy, offset = "+0+1145") %>% image_write("CommentData.png")
