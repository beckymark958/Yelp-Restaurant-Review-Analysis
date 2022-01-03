# Gordon Ramsey's Restaurants Yelp Review Analysis

## Overview
- **Project Paper**: [pdf](https://github.com/beckymark958/Yelp-Restaurant-Review-Analysis/blob/main/group5%20paper.pdf)  
- **Presentation Deck**: [pdf](https://github.com/beckymark958/Yelp-Restaurant-Review-Analysis/blob/3fc9d433842d5b8f17c4146aa908574d0f16aa6a/Presentation_Deck.pdf)
- **Goal**: Analyzing restaurant reviews on Yelp to explore potential insights, providing advice for the restaurants to adjust their marketing strategy, find out hidden problems and identify target customers.

## Data Pre-Processing
-  Lowercase text
-  Remove punctuation and unicode symbols
-  Remove stopwords (with customized stopwordr list)
-  Tokenization

## Analysis Summary (Refer to our paper for more details)
1. Opinion Mining:
- Extract bigrams to identify frequently mentioned sentimental words and its following words to see what are the positive and negative reviews' most related to
- For the word "service", the number of positive words outperform the negative words, which indicates that the service level is acceptable for most of the customers
- It shows that the limb burger is one of the most popular dishes in the restaurant
<img width="749" alt="Bigram_analysis" src="https://user-images.githubusercontent.com/25638475/147896816-1c4e059a-0733-4100-a220-b694c906f6ea.png">

2. Time-period Analysis:
- Analyze data in holidays to see whether the ratings are different from non-holidays
- Words such as "service", "time", "table", "wait" are frequently mentioned in negative reviews during holidays, comparing to words related to dishes, such as "steak", "dish". It indicates that customers are satisfied with current service quality, but are dissatisfied with service. The restaurant should consider prioritize serving time during holiday seasons over the dish quality.
- <img width="659" alt="Time_Series_analysis" src="https://user-images.githubusercontent.com/25638475/147896911-1e2950f9-637c-4837-9621-66d7a962700a.png">

3. LDA Topic Classification:
- Implement Latent Dirichlet Allocation(LDA) to allocate reviews into six categories and examine the words in each category to identify their "topics"
<img width="718" alt="LDA_Slide" src="https://user-images.githubusercontent.com/25638475/147893239-a693f809-631b-4bc5-975b-ba8a4391ba62.png">

