setwd("D:/NTU/4_Exchange/HSG/Fall 2018/Quantitative Text Analysis/QTA_Final Project")

# Switch different restaurants

# grill
data <- read.csv("review_data_GR.csv")
data <- data[data$business_id == "YJ8ljUhLsz6CtT_2ORNFmg", ]

# buffet
#data <- read.csv("set3.csv")
#data <- data[data$business_id == "ZkGDCVKSdf8m76cnnalL-A", ]

library("dplyr")
library("tm")
library("readr")
library("stringr")
library("textstem")
library("corpus")

# Define stopwords for restaurant review analysis
defined_stopwords <- c("meal", "eat", 'burger', "food", "gordon", "ramsay", "ramsey","vegas","vega","one", "think",
                       "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "just", "since","look","restaurant",
                       "take","get","say","can","will","feel","wasnt","find","people","person","make","ever","sit","want","visit",
                       "though","burgr","hell","ive","didnt","will","know","thing","also","come","much","give", "kitchen")

defined_stopwords2 <- c("meal", "eat", "food","one", "think","village","vega","vegas","buffet","le",
                        "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "just", "since","look","restaurant",
                        "take","get","say","can","will","feel","wasnt","find","people","person","make","ever","sit","want","visit",
                        "though","ive","didnt","will","know","thing","also","come","much","give")

# Data Pre-processing
clean_data <- data %>%
    mutate(text = as.character(text)) %>%
    mutate(text = removeNumbers(text)) %>%
    mutate(text = tolower(text)) %>%
    mutate(text = removePunctuation(text)) %>%
    mutate(text = stripWhitespace(text)) %>%
    #mutate(text = lapply(text, unique)) %>% 
    mutate(text = lemmatize_strings(text)) %>%
    mutate(text = removeWords(text, stopwords("english"))) %>%
    mutate(text = removeWords(text, defined_stopwords)) %>%
    mutate(text = text_tokens(text, stemmer = "en")) %>%
    mutate(text = substring(gsub(",", "", gsub("\"", "", str_c(text))), 3))
#  mutate(text = str_replace_all(text, "\\s", " ")) %>%
         
#clean_data$review_id= NULL
clean_data$user_id = NULL
clean_data$X = NULL
  

# Create Document-term Matrix
#DTM_matrix <- strsplit(as.character(clean_data$text), "\\s+")
myCorpus <- Corpus(VectorSource(clean_data$text))
review_matrix_counts <- DocumentTermMatrix(myCorpus)
rowTotal <- apply(review_matrix_counts, 1, sum)
review_matrix_counts <- review_matrix_counts[rowTotal > 0,]


# Calculate term frequency
counts <- colSums(as.matrix(review_matrix_counts))
counts <- sort(counts, decreasing = TRUE)


library(topicmodels)

# fit LDA model
review_LDA <- LDA(review_matrix_counts,
                method = "Gibbs",
                k = 6,                     # suppose we have 5 topics
                control = list(seed = 1234))

#terms(review_LDA)
#topics(review_LDA)

library(tidytext)
betaMatrix <- tidy(review_LDA, matrix="beta")

topTerms <- betaMatrix %>% group_by(topic) %>% top_n(15) %>% ungroup() %>% arrange(topic, -beta)
topTerms

library(tidyr)
beta_spread <- betaMatrix %>%
	mutate(topic = paste0("topic", topic)) %>%
	spread(topic, beta) %>%
	filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001 | topic5 > .001| topic6 > .001 )#| topic7 > .001 | topic8 > .001)# | topic9 > .001 | topic10 > .001)

# Selecting best topic setting for word
beta_spread$bestTopic = names(beta_spread)[apply(beta_spread, 1, which.max)]
beta_spread = mutate(beta_spread, beta = (pmax(topic1,topic2,topic3,topic4,topic5,topic6)))#,topic7,topic8, topic9)))#, topic10)))#) / sum(topic1,topic10,topic2,topic3,topic4,topic5,topic6,topic7,topic8,topic9)))
# Removing redudant topic columns ("2 = topic1" ~ "7 = topic6")
beta_spread <- beta_spread[, -c(2:7)]

# Group by best topic fit and selecting the top 5 words
beta_spread <- beta_spread %>% group_by(bestTopic) %>% top_n(5)

# Plot term-beta graph of 8 topics
library(ggplot2)
beta_spread %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(reorder(term, beta), beta, fill = factor(bestTopic))) +
    ggtitle("LVB\nTop 5 Term & Beta Value") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
    facet_wrap(~bestTopic, scales = "free_y", ncol = 2) +
    coord_flip() + xlab("term") 

review_document = tidy(review_LDA, matrix = "gamma")


# Calculating the topic probablity for each review
topics <- posterior(review_LDA)$topics
colnames(topics) <- paste("topic", 1:6, sep = "")

sentiment_data <- clean_data %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("bing")) %>%
    count(review_id, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

# Combining original data, topic probability, and sentiment data
combined <- merge(cbind(clean_data, topics), sentiment_data, by = "review_id") %>%
  filter(sentiment < 30 & sentiment > -20) # Remove outliers

# Ploting out the data with the probability of the two chosen topics as the axis, and the sentiment as the color scale
ggplot(combined, mapping = aes(x = topic3, y = topic6, color = sentiment)) + geom_point(size = 1) +
    scale_color_gradientn(colours = rainbow(5))

