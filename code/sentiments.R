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

#all review for Gordon Ramsay Pub & Grill
bing_word_counts_set1 <- tokentable_set1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts_set1 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Counts", x = NULL) +
  coord_flip() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle('GRPG - Sentimental Words in ALL Review ')

#all negative review for Gordon Ramsay Pub & Grill
bing_word_counts_set1_negative <- tokentable_set1_negative %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts_set1_negative %>%
  filter(sentiment == "negative") %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Counts", x = NULL) +
  coord_flip() + 
  ggtitle('GRPG - Sentimental Words in negative reviews')

#all review for Gordon Ramsay BurGR
bing_word_counts_set2 <- tokentable_set2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts_set2 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Counts", x = NULL) +
  coord_flip() + 
  ggtitle('GRBGR - Sentimental Words in ALL Review ')

#negative review for Gordon Ramsay BurGR
bing_word_counts_set2_negative <- tokentable_set2_negative %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts_set2_negative %>%
  filter(sentiment == "negative") %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Counts", x = NULL) +
  coord_flip() + 
  ggtitle('GRBGR - Sentimental Words in negative Review ')

#all review for Le Village Buffet
bing_word_counts_set3 <- tokentable_set3 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts_set3 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Counts", x = NULL) +
  coord_flip() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle('LVB - Sentimental Words in ALL Review ')

#all negative review for Le Village Buffet
bing_word_counts_set3_negative <- tokentable_set3_negative %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts_set3_negative %>%
  filter(sentiment == "negative") %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Counts", x = NULL) +
  coord_flip() + 
  ggtitle('LVB - Sentimental Words in negative reviews')
