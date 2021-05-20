library(tidyr)
library(data.table)
library(rvest)
library(jsonlite)
library(dplyr)
library(data.table)
# install.packages("tidytext")
library(tidytext)
# install.packages("rlang")
library(rlang)
library(tidyverse)
library(readr)
library(ggplot2)
library(stringr)
library(scales)
# install.packages("textdata")
library(textdata)
library(forcats)
# installed.packages("igraph")
library(igraph)
# installed.packages("ggraph")
library(ggraph)
# install.packages("topicmodels")
library(topicmodels)
# install.packages("reshape2")
library(reshape2)



urlfile = "https://raw.githubusercontent.com/fanni-k/Unstructured_text_analysis/main/Scrape_articles/Data/guardian_climate_change5.csv"
df <-read_csv(url(urlfile))
View(head(df))

####
# tidy text format
# a table with one token per row
# one token is one word

tidy_df <- df %>% tidytext::unnest_tokens(word, text)
# View(head(tidy_df))

# remove stop words

data("stop_words")
tidy_df2 <- tidy_df %>% anti_join(stop_words)

# View(head(tidy_df2))

# find most common words
tidy_df2 %>% count(word, sort = TRUE)

# visualize most common words in Guardian articles (more than 3,000)
tidy_df2 %>% count(word, sort = TRUE) %>%
  filter(n > 3000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# calculate the frequency for each word by time_group
## pre_IPCC
frequency1 <- tidy_df2 %>% 
  filter(time_group == 'pre_IPCC') %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(word) %>% 
  mutate(pre_IPCC_proportion = n / sum(n)) %>% 
  select(-n)

# post_IPCC
frequency2 <- tidy_df2 %>% 
  filter(time_group == 'post_IPCC') %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(word) %>% 
  mutate(post_IPCC_proportion = n / sum(n)) %>% 
  select(-n) 

frequency <- inner_join(frequency1, frequency2, by = 'word')

# View(head(frequency))

# visualize word frequency of common words in the two time groups

ggplot(frequency, aes(x = pre_IPCC_proportion, y = post_IPCC_proportion)) +
  geom_abline(color = "#1c9099", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_colour_gradient(limits = c(0, 0.001), 
                        low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "Proportion of words in post-IPCC group", x = "Proportion of words in pre-IPCC group")



###
# sentiment analysis

tidy_time_groups <- df %>% arrange(publication_date) %>% 
  group_by(time_group) %>% 
  mutate(linenumber = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word, text)

# View(head(tidy_time_groups))

get_sentiments("afinn")
# get_sentiments("bing")
# nrc <- get_sentiments("nrc")

## NRC

# filter on fear
nrc_fear <- get_sentiments("nrc") %>% filter(sentiment == "fear")
tidy_time_groups %>% inner_join(nrc_fear) %>% count(word, sort = TRUE)

# filter on negative
nrc_neg <- get_sentiments("nrc") %>% filter(sentiment == "negative")
tidy_time_groups %>% inner_join(nrc_neg) %>% count(word, sort = TRUE)

# filter on positive
nrc_poz <- get_sentiments("nrc") %>% filter(sentiment == "positive")
tidy_time_groups %>% inner_join(nrc_poz) %>% count(word, sort = TRUE)

# BING
group_sentiment <- tidy_time_groups %>% inner_join(get_sentiments("bing")) %>% 
  count(time_group, index = linenumber %/% 80, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# sentiment comparison of the two time groups by BING lexicon
ggplot(group_sentiment, aes(index, sentiment, fill = time_group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~time_group, ncol = 2, scales = "free_x")

# 1) comparing the three sentiment dictionaries
afinn <- tidy_time_groups %>%  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  tidy_time_groups %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_time_groups %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


afinn[which.max(afinn$sentiment),] # index 2 most positive in afinn
afinn[which.min(afinn$sentiment),] # index 16 most negative in afinn
bing_and_nrc %>% filter(method == "NRC") %>% arrange(desc(sentiment)) # index 2 most positive in NRC
bing_and_nrc %>% filter(method == "NRC") %>% arrange(sentiment) # index 19 lowest sentiment in NRC (but still positive)
bing_and_nrc %>% filter(method == "Bing et al.") %>% arrange(desc(positive)) # index 4 most positive in NRC
bing_and_nrc %>% filter(method == "Bing et al.") %>% arrange(desc(negative)) # index 4 most negative (but still positive)


# AFINN seems to have more variance

# 2) sentiment analysis in pre_IPCC and post_IPCC

afinn_time_group <- tidy_time_groups %>%  inner_join(get_sentiments("afinn")) %>% 
  group_by(time_group, index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

afinn_time_group %>% filter(time_group == 'pre_IPCC') %>% ggplot(aes(x = index, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) + labs(title = "Sentiment Analysis by AFINN lexicon", 
                                       subtitle = "pre_IPCC group",
                                       caption = "Source: The Guardian")

afinn_time_group %>% filter(time_group == 'post_IPCC') %>% ggplot(aes(x = index, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE)+ labs(title = "Sentiment Analysis by AFINN lexicon", 
                                      subtitle = "post_IPCC group",
                                      caption = "Source: The Guardian")

afinn_time_group$time_group <- factor(afinn_time_group$time_group, levels = c("pre_IPCC", "post_IPCC"))

afinn_time_group %>% ggplot(aes(x = time_group, y = sentiment))+
  geom_col()+
  labs(title = "Before and after the IPCC report release",
       subtitle = "Sentiment Analysis on Words in Articles with 'climate' and 'change'",
       caption = "Source: The Guardian")

# after the IPCC report release, sentiment is less positive

###########
# TF-IDF

# create a table, where one row is for each time_group-word combination
df_word <- tidy_df %>% count(time_group, word, sort = TRUE)
total_words <- df_word %>% group_by(time_group) %>% summarize(total = sum(n))
df_word <- left_join(df_word, total_words)

df_word

# term frequency distribution in each time_group --> short tails to the right, not much extremely rare words. distribution is similar in each time_group

ggplot(df_word, aes(n/total, fill = time_group)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~time_group, ncol = 2, scales = "free_y")

# Zipf's law for Guardian articles - the rank of each word in each time_group - there is a negative relationship between rank and frequency
freq_by_rank <- df_word %>% group_by(time_group) %>% mutate(rank = row_number(), `term frequency` = n/total) %>% ungroup()
freq_by_rank 
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = time_group)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = time_group)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# the articles contain fewer rare words than predicted by a single power law

# what are the important words for the content of each time_group?
article_tf_idf <- df_word %>% bind_tf_idf(word, time_group, n)
article_tf_idf

# high tf-idf words

article_tf_idf %>% select(-total) %>% arrange(desc(tf_idf))

# Highest tf-idf words in each time_group
article_tf_idf %>%
  group_by(time_group) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = time_group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~time_group, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# this shows that the two examined time groups are distinguished by these names and places

# 4) tokenizing bigrams: what is the context of climate change in each group?

articles_bigrams <- df %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- articles_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 200) %>%
  graph_from_data_frame()

set.seed(20210512)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# create separate bigram graphs for each time_group

# pre

df_pre <- df %>% filter(time_group == "pre_IPCC")

articles_bigrams_pre <- df_pre %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated_pre <- articles_bigrams_pre %>% separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_pre <- bigrams_separated_pre %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts_pre <- bigrams_filtered_pre %>% 
  count(word1, word2, sort = TRUE)

bigram_graph_pre <- bigram_counts_pre %>%
  filter(n > 100) %>%
  graph_from_data_frame()

set.seed(20210512)
ggraph(bigram_graph_pre, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# post

df_post <- df %>% filter(time_group == "post_IPCC")

articles_bigrams_post <- df_post %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated_post <- articles_bigrams_post %>% separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_post <- bigrams_separated_post %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts_post <- bigrams_filtered_post %>% 
  count(word1, word2, sort = TRUE)

bigram_graph_post <- bigram_counts_post %>%
  filter(n > 100) %>%
  graph_from_data_frame()

set.seed(20210512)
ggraph(bigram_graph_post, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# 5) topic modelling: What topics can be separated in each group?

time_group_dtm <- tidy_df2 %>% count(time_group, word) %>% cast_dtm(time_group, word, n)

# LDA

time_group_lda <- LDA(time_group_dtm, k = 2, control = list(seed = 20210513))

tg_topics <- tidy(time_group_lda, matrix = "beta") 

# find the 10 most common terms within each topic

tg_top_terms <- tg_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
tg_top_terms %>% mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


############ article

article_dtm <- tidy_df2 %>% count(id, word) %>% cast_dtm(id, word, n)

# LDA

article_lda <- LDA(article_dtm, k = 2, control = list(seed = 20210513))

a_topics <- tidy(article_lda, matrix = "beta") 

# find the 10 most common terms within each topic

a_top_terms <- a_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
a_top_terms %>% mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

###########x

# pre
tidy_df2_pre <- tidy_df2 %>% filter(time_group == "pre_IPCC")
time_group_dtm_pre <- tidy_df2_pre %>% count(time_group, word) %>% cast_dtm(time_group, word, n)

# LDA

time_group_lda_pre <- LDA(time_group_dtm_pre, k = 2, control = list(seed = 20210513))

tg_topics_pre <- tidy(time_group_lda_pre, matrix = "beta") 

# find the 10 most common terms within each topic in pre_IPCC group

tg_top_terms_pre <- tg_topics_pre %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
tg_top_terms_pre %>% mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# post
tidy_df2_post <- tidy_df2 %>% filter(time_group == "post_IPCC")
time_group_dtm_post <- tidy_df2_post %>% count(time_group, word) %>% cast_dtm(time_group, word, n)

# LDA

time_group_lda_post <- LDA(time_group_dtm_post, k = 2, control = list(seed = 20210513))

tg_topics_post <- tidy(time_group_lda_post, matrix = "beta") 

# find the 10 most common terms within each topic in pre_IPCC group

tg_top_terms_post <- tg_topics_post %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
tg_top_terms_post %>% mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

