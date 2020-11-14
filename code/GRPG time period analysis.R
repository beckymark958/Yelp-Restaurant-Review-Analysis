library(stringr)
library(ggplot2)
library(readr)
library(dplyr)
library(tm)
library(tidytext)
library(readr)
GR <- read_csv("C:/Users/Lawrence/Desktop/qta/review_data_GR.csv")
GR <- select(GR,-1)
GRPG <- GR %>%
  filter(business_id == "YJ8ljUhLsz6CtT_2ORNFmg")
Festival_GRPG <- GRPG %>%
  filter((date >= "2012-12-20" & date <= "2013-01-01") | (date >= "2013-12-20" & date <= "2014-01-01") | 
         (date >= "2014-12-20" & date <= "2015-01-01") | (date >= "2015-12-20" & date <= "2016-01-01") |
         (date >= "2016-12-20" & date <= "2017-01-01") | (date >= "2017-12-20" & date <= "2018-01-01") |
         (date == "2012-02-14") | (date == "2013-02-14") | (date == "2014-02-14") | (date == "2015-02-14") |
         (date == "2016-02-14") | (date == "2017-02-14") | (date == "2018-02-14") | (date == "2012-05-13") |
         (date == "2013-05-12") | (date == "2014-05-11") | (date == "2015-05-10") | (date == "2016-05-08") |
         (date == "2017-05-14") | (date == "2018-05-13") | (date == "2012-06-17") | (date == "2013-06-17") |
         (date == "2014-06-17") | (date == "2015-06-21") | (date == "2016-06-19") | (date == "2017-06-18") |
         (date == "2018-06-17"))

stopwords <- c("fish","fishs","pub","ramsay","hell","restaurants","restaurant","food","gordon","burger","tries")
triptext <- as.character(Festival_GRPG$text)
triptext <- tolower(triptext)
triptext <- removeWords(triptext, stopwords("english"))
triptext <- removeWords(triptext, stopwords)
triptext <- stemDocument(triptext)
triptext.df <- data.frame(triptext,stringsAsFactors = FALSE)

trip_raw <- triptext.df %>% unnest_tokens(word, triptext, drop = TRUE)
trip_raw <- tibble::rowid_to_column(trip_raw,"ID")
trip_raw <- trip_raw %>% anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[^\\d]")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()
trip_raw <- trip_raw %>% distinct(word, .keep_all = TRUE)

trip_raw <- trip_raw %>%
  arrange(desc(word_total))
trip_raw <- head(trip_raw,25)
ggplot(trip_raw, aes(reorder(word,word_total),word_total))+
  geom_col(fill = "yellow")+
  coord_flip()+ 
  labs(title = "Mostly mentioned words\nGordan Ramsay Pub and Grill reviews",
       y = "number of uses", x = "words")
