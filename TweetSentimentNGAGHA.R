install.packages("rtweet")

install.packages("tidyverse")

library(dplyr)
library(rtweet)
library(tidyr)
library(tidyselect)
library(tidytext)
library(ggplot2)
library(tidytext)
library(textdata)
library(janeaustenr)
library(stringr)
library(tidyverse)


create_token(app = "twtrtrend",
             consumer_key = "PQP3NHfVvZDHXFX4DRJJPXMvo",
             consumer_secret = "Q7g43JQvtTZL1o4AmE5ot8DuvCd6zb3xSD8KoUbe64r8KtbTVw",
             access_token = "135698192-vVSYWT8trRNrl2Tdz1UzmtBq9wr3AL8R9ia23IEp",
             access_secret = "WsXudWi9vLiwVt0jhjFDVg6tBGxfBRAPg2LJxO61MZbL1")
nigeria<- search_tweets("#Nigeria", n=1000, include_rts = FALSE)
ghana <- search_tweets("#Ghana", n=1000, include_rts = FALSE)

nigeria
ghana

tweets.nigeria=nigeria %>% select(screen_name, text)
tweets.nigeria
tweets.ghana=ghana %>% select(screen_name, text)

library(janeaustenr)
library(stringr)
###CLEANING NIGERIA DATA
head(tweets.nigeria$text)


# Remove mentions, urls, emojis, numbers, punctuations, etc.
tweets.nigeria$text <- gsub("@\\w+", "", tweets.nigeria$text)
tweets.nigeria$text <- gsub("https?://.+", "", tweets.nigeria$text)
tweets.nigeria$text <- gsub("\\d+\\w*\\d*", "", tweets.nigeria$text)
tweets.nigeria$text <- gsub("#\\w+", "", tweets.nigeria$text)
tweets.nigeria$text <- gsub("[^\x01-\x7F]", "", tweets.nigeria$text)
tweets.nigeria$text <- gsub("[[:punct:]]", " ", tweets.nigeria$text)
# Remove spaces and newlines
tweets.nigeria$text <- gsub("\n", " ", tweets.nigeria$text)
tweets.nigeria$text <- gsub("^\\s+", "", tweets.nigeria$text)
tweets.nigeria$text <- gsub("\\s+$", "", tweets.nigeria$text)
tweets.nigeria$stripped_text1 <- gsub("[ |\t]+", " ", tweets.nigeria$text)


tweets.nigeria$stripped_text1


tweets.nigeria_stem<-tweets.nigeria %>% 
  select (stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

head(tweets.nigeria_stem)

cleaned_tweets.nigeria <- tweets.nigeria_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.nigeria)
head(tweets.nigeria$text)

###CLEANING GHANA DATA
head(tweets.ghana$text)


# Remove mentions, urls, emojis, numbers, punctuations, etc.
tweets.ghana$text <- gsub("@\\w+", "", tweets.ghana$text)
tweets.ghana$text <- gsub("https?://.+", "", tweets.ghana$text)
tweets.ghana$text <- gsub("\\d+\\w*\\d*", "", tweets.ghana$text)
tweets.ghana$text <- gsub("#\\w+", "", tweets.ghana$text)
tweets.ghana$text <- gsub("[^\x01-\x7F]", "", tweets.ghana$text)
tweets.ghana$text <- gsub("[[:punct:]]", " ", tweets.ghana$text)
# Remove spaces and newlines
tweets.ghana$text <- gsub("\n", " ", tweets.ghana$text)
tweets.ghana$text <- gsub("^\\s+", "", tweets.ghana$text)
tweets.ghana$text <- gsub("\\s+$", "", tweets.ghana$text)
tweets.ghana$stripped_text1 <- gsub("[ |\t]+", " ", tweets.ghana$text)


tweets.ghana$stripped_text1


tweets.ghana_stem<-tweets.ghana %>% 
  select (stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

head(tweets.ghana_stem)

cleaned_tweets.ghana <- tweets.ghana_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.ghana)
head(tweets.ghana$text)

####TOP WORDS IN TWEET

library(ggplot2)

cleaned_tweets.nigeria %>% 
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y = n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  theme_classic()+
  labs(x="Counts",
       y="Unique words",
       title = "Unique word counts found in #Nigeria tweets")


cleaned_tweets.ghana %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y = n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  theme_classic()+
  labs(x="Counts",
       y="Unique words",
       title = "Unique word counts found in #Ghana tweets")


###LET'S DO SENTIMENT HERE

library(tidytext)

install.packages("textdata")
library(textdata)

get_sentiments("bing") %>% filter(sentiment=="positive")

get_sentiments("bing") %>% filter(sentiment=="negative")

get_sentiments("afinn") %>% filter(value=="3")

get_sentiments("afinn") %>% filter(value=="-3")


bing_nigeria <- cleaned_tweets.nigeria %>%
  inner_join (get_sentiments("bing")) %>%
  count (word, sentiment, sort = TRUE) %>%
  ungroup()

bing_nigeria

bing_ghana <- cleaned_tweets.ghana %>%
  inner_join (get_sentiments("bing")) %>%
  count (word, sentiment, sort = TRUE) %>%
  ungroup()

bing_ghana

#####NIGERIA

bing_nigeria %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#Nigeria'", 
       y= "Contribution to sentiment",
       x=NULL) +
  coord_flip()+theme_bw()


#####GHANA

bing_ghana %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#Ghana'", 
       y= "Contribution to sentiment",
       x=NULL) +
  coord_flip()+theme_bw()


sentiment_bing = function(twt) {
  twt_tbl = tibble(text = twt) %>%
    mutate (
      stripped_text = gsub ("http\\s+","",text)
    ) %>%
    unnest_tokens (word, stripped_text) %>%
    anti_join (stop_words) %>%
    inner_join (get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup () %>%
    mutate (
      score = case_when (
        sentiment == 'negative' ~ n*(-1),
        sentiment == 'positive' ~n*1
      )
    )
  sent.score = case_when (
    nrow(twt_tbl) == 0~0,
    nrow (twt_tbl) > 0~sum(twt_tbl$score)
  )
  zero.type = case_when(
    nrow(twt_tbl) ==0~"Type 1",
    nrow(twt_tbl)>0~"Type 2"
  )
  list(score = sent.score, type = zero.type, twt_tbl=twt_tbl)
}


nigeria_sent = lapply(nigeria$text, function(x){sentiment_bing(x)})
nigeria_sent
ghana_sent = lapply(ghana$text, function(x){sentiment_bing(x)})
nigeria_sent

install.packages("tibble")

library (tibble)

country_sentiment = bind_rows(
  tibble(
    country = 'Nigeria',
    score = unlist(map(nigeria_sent, 'score')),
    type = unlist (map(nigeria_sent, 'type'))
  ),
  tibble(
    country = 'Ghana',
    score = unlist(map(ghana_sent, 'score')),
    type = unlist (map(ghana_sent, 'type'))
  )
)


country_sentiment


ggplot(country_sentiment, aes(x = score, fill = country)) + geom_histogram(bins = 30, alpha=1.8) + 
  facet_grid(~country) + theme_minimal()
