library(dplyr)
library(stringr)
library(tm) 
library(RTextTools)
library(stopwords)
library(tidytext)
library(tidyverse)
library(tidyr)
library(scales)
library(SnowballC)
library(ggplot2)
library(ggraph)

# YJ8ljUhLsz6CtT_2ORNFmg --Set 1
# cYwJA2A6I12KNkm2rtXd5g --Set 2
# ZkGDCVKSdf8m76cnnalL-A --Set 3
review_filtered <- select(review, review_id, user_id, business_id, stars, date, text)
review_set3_negative <- filter(review, business_id == "ZkGDCVKSdf8m76cnnalL-A", stars < 4)

text_vector3_positive <- review_set3_positive[['text']] #options(max.print=1000000)
text_vector3_positive <- tolower(text_vector3_positive)
text_vector3_positive <- str_replace_all(text_vector3_positive, "[\n\n]", "")
text_vector3_positive <- str_replace_all(text_vector3_positive, "??", "e")
text_vector3_positive <- str_replace_all(text_vector3_positive, "??", "n")
text_vector3_positive <- str_replace_all(text_vector3_positive, " +", " ")
text_vector3_positive <- str_replace_all(text_vector3_positive, " w ", "  with ")
text_processed3_positive <- text_vector3_positive

text_processed3_negative_2 <- text_vector3_negative


for (x in 1:1201) {
  #text_processed3_negative_2[x] <- removeWords(text_processed3_negative_2[x], stopwords::stopwords(language = "en", source = "snowball"))
  text_processed3_negative_2[x] <- removePunctuation(text_processed3_negative_2[x])
  text_processed3_negative_2[x] <- stemDocument(text_processed3_negative_2[x], language="english")
}

text_processed3_negative_2 <- str_replace_all(text_processed3_negative_2, " +", " ")

#declare character vector
review_body_set1 <- as.character(text_processed1)
review_body_set1_positive <- as.character(text_processed1_positive)
review_body_set1_negative <- as.character(text_processed1_negative)
review_body_set2 <- as.character(text_processed2)
review_body_set2_positive <- as.character(text_processed2_positive)
review_body_set2_negative <- as.character(text_processed2_negative)
review_body_set3 <- as.character(text_processed3)
review_body_set3_positive <- as.character(text_processed3_positive)
review_body_set3_negative <- as.character(text_processed3_negative)
review_body_set3_negative_2 <- as.character(text_processed3_negative_2)

print(review_body_set3_positive)

#create new df
review_words1 <- data.frame(review_body_set1, stringsAsFactors = FALSE)
review_words1_positive <- data.frame(review_body_set1_positive, stringsAsFactors = FALSE)
review_words1_negative <- data.frame(review_body_set1_negative, stringsAsFactors = FALSE)
review_words2 <- data.frame(review_body_set2, stringsAsFactors = FALSE)
review_words2_positive <- data.frame(review_body_set2_positive, stringsAsFactors = FALSE)
review_words2_negative <- data.frame(review_body_set2_negative, stringsAsFactors = FALSE)
review_words3 <- data.frame(review_body_set3, stringsAsFactors = FALSE)
review_words3_positive <- data.frame(review_body_set3_positive, stringsAsFactors = FALSE)
review_words3_negative <- data.frame(review_body_set3_negative, stringsAsFactors = FALSE)
review_words3_negative_2 <- data.frame(review_body_set3_negative_2, stringsAsFactors = FALSE)

#tokenize all df
tokentable_set1 <- review_words1 %>% unnest_tokens(word, review_body_set1, drop = TRUE) #separate words as token
tokentable_set1_positive <- review_words2_positive %>% unnest_tokens(word, review_body_set2_positive, drop = TRUE)
tokentable_set1_negative <- review_words1_negative %>% unnest_tokens(word, review_body_set1_negative, drop = TRUE) 
tokentable_set2 <- review_words2 %>% unnest_tokens(word, review_body_set2, drop = TRUE) 
tokentable_set2_positive <- review_words2_positive %>% unnest_tokens(word, review_body_set2_positive, drop = TRUE) 
tokentable_set2_negative <- review_words2_negative %>% unnest_tokens(word, review_body_set2_negative, drop = TRUE) 
tokentable_set3 <- review_words3 %>% unnest_tokens(word, review_body_set3, drop = TRUE) 
tokentable_set3_positive <- review_words3_positive %>% unnest_tokens(word, review_body_set3_positive, drop = TRUE) 
tokentable_set3_negative <- review_words3_negative %>% unnest_tokens(word, review_body_set3_negative, drop = TRUE) 

#add new column 'ID' to all table
tokentable_set1 <- tibble::rowid_to_column(tokentable_set1, "ID")
tokentable_set1_positive <- tibble::rowid_to_column(tokentable_set1_positive, "ID")
tokentable_set1_negative <- tibble::rowid_to_column(tokentable_set1_negative, "ID")
tokentable_set2 <- tibble::rowid_to_column(tokentable_set2, "ID")
tokentable_set2_positive <- tibble::rowid_to_column(tokentable_set2_positive, "ID")
tokentable_set2_negative <- tibble::rowid_to_column(tokentable_set2_negative, "ID")
tokentable_set3 <- tibble::rowid_to_column(tokentable_set3, "ID")
tokentable_set3_positive <- tibble::rowid_to_column(tokentable_set3_positive, "ID")
tokentable_set3_negative <- tibble::rowid_to_column(tokentable_set3_negative, "ID")

#create df called 'total words"
review_set3_positive_totalwords <- tokentable_set3_positive %>% distinct(ID, word, .keep_all = FALSE) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[^\\d]")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()
review_set3_positive_totalwords <- review_set3_positive_totalwords %>% distinct(word, .keep_all = TRUE)

#create new df for graph
word_counts_set1 <- review_set1_totalwords %>% select(word,word_total)
word_counts_set1 <- arrange(word_counts_set1, desc(word_total))
word_counts_set3_positive <- review_set3_positive_totalwords %>% select(word,word_total)
word_counts_set3_positive <- arrange(word_counts_set3_positive, desc(word_total))
word_counts_set3_negative <- review_set3_negative_totalwords %>% select(word,word_total)
word_counts_set3_negative <- arrange(word_counts_set3_negative, desc(word_total))

#plot the graph
ggplot(data = head(word_counts_set3_negative,20), aes(reorder(word, word_total), word_total)) +
  geom_col(fill = "deepskyblue4") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title = "LVB - Top 20 words in ALL negative reviews",
       subtitle = "1,201 reviews stemmed and with stop words removed",x = "word", 
       y = "# of uses") 
