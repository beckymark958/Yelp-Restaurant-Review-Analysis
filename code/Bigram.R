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

#create df for bigrams #Set 1 - GRPG, Set 2 - GRBGR
review_bigrams_set1 <-review_words1 %>% unnest_tokens(bigram, review_body_set1, token = "ngrams", n = 2, drop = TRUE)
bigrams_separated_set1 <- review_bigrams_set1 %>% separate(bigram, c("word1", "word2"), sep = " ")

review_bigrams_set1_negative <-review_words1_negative %>% unnest_tokens(bigram, review_body_set1_negative, token = "ngrams", n = 2, drop = TRUE)
bigrams_separated_set1_negative <- review_bigrams_set1_negative %>% separate(bigram, c("word1", "word2"), sep = " ")

review_bigrams_set1_positive <-review_words1_positive %>% unnest_tokens(bigram, review_body_set1_positive, token = "ngrams", n = 2, drop = TRUE)
bigrams_separated_set1_positive <- review_bigrams_set1_positive %>% separate(bigram, c("word1", "word2"), sep = " ")

#set 2
review_bigrams_set2 <-review_words2 %>% unnest_tokens(bigram, review_body_set2, token = "ngrams", n = 2, drop = TRUE)
bigrams_separated_set2 <- review_bigrams_set2 %>% separate(bigram, c("word1", "word2"), sep = " ")

review_bigrams_set2_negative <-review_words2_negative %>% unnest_tokens(bigram, review_body_set2_negative, token = "ngrams", n = 2, drop = TRUE)
bigrams_separated_set2_negative <- review_bigrams_set2_negative %>% separate(bigram, c("word1", "word2"), sep = " ")

review_bigrams_set2_positive <-review_words2_positive %>% unnest_tokens(bigram, review_body_set2_positive, token = "ngrams", n = 2, drop = TRUE)
bigrams_separated_set2_positive <- review_bigrams_set2_positive %>% separate(bigram, c("word1", "word2"), sep = " ")

#set 3
review_bigrams_set3 <-review_words3 %>% unnest_tokens(bigram, review_body_set3, token = "ngrams", n = 2, drop = TRUE)
bigrams_separated_set3 <- review_bigrams_set3 %>% separate(bigram, c("word1", "word2"), sep = " ")

review_bigrams_set3_negative <-review_words3_negative %>% unnest_tokens(bigram, review_body_set3_negative, token = "ngrams", n = 2, drop = TRUE)
bigrams_separated_set3_negative <- review_bigrams_set3_negative %>% separate(bigram, c("word1", "word2"), sep = " ")

review_bigrams_set3_negative_2 <-review_words3_negative_2 %>% unnest_tokens(bigram, review_body_set3_negative_2, token = "ngrams", n = 2, drop = TRUE)
bigrams_separated_set3_negative_2 <- review_bigrams_set3_negative_2 %>% separate(bigram, c("word1", "word2"), sep = " ")

review_bigrams_set3_positive <-review_words3_positive %>% unnest_tokens(bigram, review_body_set3_positive, token = "ngrams", n = 2, drop = TRUE)
bigrams_separated_set3_positive <- review_bigrams_set3_positive %>% separate(bigram, c("word1", "word2"), sep = " ")

#words following the disappoint & bad
bigrams_separated_set1_negative %>%
  filter(word1 == "disappoint") %>%
  count(word1, word2, sort = TRUE)

bigrams_separated_set1_negative %>%
  filter(word1 == "bad") %>%
  count(word1, word2, sort = TRUE)

bigrams_separated_set2_negative %>%
  filter(word1 == "disappoint") %>%
  count(word1, word2, sort = TRUE)

bigrams_separated_set2_negative %>%
  filter(word1 == "bad") %>%
  count(word1, word2, sort = TRUE)

bigrams_separated_set3_negative %>%
  filter(word1 == "disappoint") %>%
  count(word1, word2, sort = TRUE)

bigrams_separated_set3_negative %>%
  filter(word1 == "bad") %>%
  count(word1, word2, sort = TRUE)

bigrams_separated_set3_negative %>%
  filter(word1 == "worth") %>%
  count(word1, word2, sort = TRUE)

#words following the good & great

bigrams_set1_plot_1 <- bigrams_separated_set1 %>%
  filter(word2 == "lack") %>%
  count(word1, word2, sort = TRUE)

bigrams_set1_plot_1 %>%
  top_n(5) %>% 
  ggplot(aes(reorder(word1,n,descending=TRUE), n, fill = word2)) +
  geom_col(show.legend = FALSE, fill = "deepskyblue4") +
  facet_wrap(~word2, scales = "free") +
  labs(y = "Counts", x = NULL) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

bigrams_set1_plot_2 <- bigrams_separated_set1 %>%
  filter(word1 == "best" ) %>%
  count(word1, word2, sort = TRUE)

# word1 == "disappoint" | word1 == "worst" | word1 == "lack" | word1 == "good"

bigrams_set1_plot_2 %>%
  top_n(5) %>% 
  ggplot(aes(reorder(word2,n,descending=TRUE), n, fill = word1)) +
  geom_col(show.legend = FALSE, fill = "deepskyblue4") +
  facet_wrap(~word1, scales = "free") +
  labs(y = "Counts", x = NULL) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




bigrams_set3_plot_1 <- bigrams_separated_set3_negative %>%
  filter(word2 == "price") %>%
  count(word1, word2, sort = TRUE)

bigrams_set3_plot_1 %>%
  top_n(5) %>% 
  ggplot(aes(reorder(word1,n,descending=TRUE), n, fill = word2)) +
  geom_col(show.legend = FALSE, fill = "deepskyblue4") +
  facet_wrap(~word2, scales = "free") +
  labs(y = "Counts", x = NULL) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

bigrams_set3_plot_2 <- bigrams_separated_set3_negative %>%
  filter(word1 == "bad" ) %>%
  count(word1, word2, sort = TRUE)

bigrams_set3_plot_2 %>%
  top_n(5) %>% 
  ggplot(aes(reorder(word2,n,descending=TRUE), n, fill = word1)) +
  geom_col(show.legend = FALSE, fill = "deepskyblue4") +
  facet_wrap(~word1, scales = "free") +
  labs(y = "Counts", x = NULL) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# bigrams_separated_set2 %>%
#   filter(word1 == "good") %>%
#   count(word1, word2, sort = TRUE)

#  bigrams_separated_set2 %>%
#   filter(word1 == "best") %>%
#   count(word1, word2, sort = TRUE)
# 
# bigrams_separated_set2_positive %>%
#   filter(word2 == "burger") %>%
#   count(word1, word2, sort = TRUE)
# 
# bigrams_separated_set3_positive %>%
#   filter(word1 == "good") %>%
#   count(word1, word2, sort = TRUE)
# 
# bigrams_separated_set3_positive %>%
#   filter(word1 == "like") %>%
#   count(word1, word2, sort = TRUE)
# 
# bigrams_separated_set3_positive %>%
#   filter(word2 == "crab") %>%
#   count(word1, word2, sort = TRUE)
