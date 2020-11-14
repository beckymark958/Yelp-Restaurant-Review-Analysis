library(stringr)
library(ggplot2)
library(readr)
library(dplyr)
library(tm)
library(tidytext)
library(readr)
lvb <- read_csv("C:/Users/Lawrence/Desktop/qta/set3.csv")
lvb <- select(lvb,-1)
Festival_lvb <- lvb %>%
  filter((date >= "2012-12-20" & date <= "2013-01-01") | (date >= "2013-12-20" & date <= "2014-01-01") | 
           (date >= "2014-12-20" & date <= "2015-01-01") | (date >= "2015-12-20" & date <= "2016-01-01") |
           (date >= "2016-12-20" & date <= "2017-01-01") | (date >= "2017-12-20" & date <= "2018-01-01") |
           (date == "2012-02-14") | (date == "2013-02-14") | (date == "2014-02-14") | (date == "2015-02-14") |
           (date == "2016-02-14") | (date == "2017-02-14") | (date == "2018-02-14") | (date == "2012-05-13") |
           (date == "2013-05-12") | (date == "2014-05-11") | (date == "2015-05-10") | (date == "2016-05-08") |
           (date == "2017-05-14") | (date == "2018-05-13") | (date == "2012-06-17") | (date == "2013-06-17") |
           (date == "2014-06-17") | (date == "2015-06-21") | (date == "2016-06-19") | (date == "2017-06-18") |
           (date == "2018-06-17"))

stopwords_lvb <- c("buffet","french","food","crepe")
triptext_lvb <- as.character(Festival_lvb$text)
triptext_lvb <- tolower(triptext_lvb)
triptext_lvb <- removeWords(triptext_lvb, stopwords("english"))
triptext_lvb <- removeWords(triptext_lvb, stopwords_lvb)
triptext_lvb <- stemDocument(triptext_lvb)
triptext_lvb.df <- data.frame(triptext_lvb,stringsAsFactors = FALSE)

trip_raw_lvb <- triptext_lvb.df %>% unnest_tokens(word, triptext_lvb, drop = TRUE)
trip_raw_lvb <- tibble::rowid_to_column(trip_raw_lvb,"ID")
trip_raw_lvb <- trip_raw_lvb %>% anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[^\\d]")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()
trip_raw_lvb <- trip_raw_lvb %>% distinct(word, .keep_all = TRUE)

trip_raw_lvb <- trip_raw_lvb %>%
  arrange(desc(word_total))
trip_raw_lvb <- head(trip_raw_lvb,25)
ggplot(trip_raw_lvb, aes(reorder(word,word_total),word_total))+
  geom_col(fill = "yellow")+
  coord_flip()+ 
  labs(title = "Mostly mentioned words\nGordan Ramsay Pub and Grill reviews",
       y = "number of uses", x = "words")
