
# APPLICATION SETTINGS

appname <- "YOURAPPNAME"
key     <- "YOURAPIKEY"
secret  <- "YOURAPIKEYSECRET"

# TOKEN

twitter_token = rtweet::create_token(app = appname,
                                     consumer_key = key,
                                     consumer_secret = secret)

# LOAD LIBRARIES

library(rtweet)
library(tidyverse)
library(tidytext)
library(waffle)
library(tm)
library(stopwords)
library(wordcloud2)
library(syuzhet)
library(stringi)
library(parallel)
library(udpipe)
library(kableExtra)
library(knitr)
library(lubridate)
library(plotly)
library(udpipe)
library(leaflet)
library(ggrepel)
library(sf)


# TESTING LOCATION FILTER

trends_available() %>% 
  filter(countryCode=="MX")


# TESTING WOEID (Where On Earth IDentifier)

get_trends(woeid = 116545)


# EXTRACT DATA FROM HASHTAG OR KEYWORD

tweetsStrangerThings = search_tweets(q = "#strangerthingsseason4", 
                          n = 500, geocode = "19.429496,-99.163151,1500mi", 
                          include_rts = T, lang="es") 
tweetsStrangerThings %>% 
  head()

# EXTRACT DATA FROM USERNAME

tweetsMyAccount = get_timeline(user = "@your_username", n = 3500, lang="es", 
                               include_rts = T)
saveRDS(tweetsMyAccount, "myTweets.RDS")
tweetsMyAccount %>% 
  head()


# ORGANIC TWEETS

organicMyAccount = tweetsMyAccount %>% 
  filter(is_retweet==F, is.na(reply_to_status_id))


# RETWEETS

rtsMyAccount = tweetsMyAccount %>% 
  filter(is_retweet==T)


# REPLIES

repliesMyAccount = tweetsMyAccount %>% 
  filter(!is.na(reply_to_status_id))


# TEXT CLEANING

organicMyAccount = organicMyAccount %>% 
  mutate(text=str_replace_all(text, "https\\S*", "")) %>% # urls
  mutate(text=str_replace_all(text, "@\\S*", "")) %>% # mentions
  mutate(text=str_replace_all(text, "[\r\n\t]", "")) %>% # dividers
  mutate(text=removeNumbers(text)) %>% # numbers
  mutate(text=removePunctuation(text)) %>%  # punctuation
  mutate(text=str_squish(text))


# EMPTY WORDS 

stpwSnow = stopwords(language = "es", source = "snowball")
stpwIso = stopwords(language = "es", source = "stopwords-iso")
stpwNtlk = stopwords(language = "es", source = "nltk")
stpwNtlkEn = stopwords(language = "en", source = "nltk")


# WORD COUNT

myTweets = organicMyAccount %>%
  select(text) %>%
  unnest_tokens(token, text, to_lower = F)

myTweets = myTweets %>%
  filter(!token %in% c(stpwNtlk)) %>%
  filter(!token %in% c(stpwNtlkEn)) 

myTweets %>%
  head(10)


# DOWNLOAD PRETRAINED MODEL UDPIPE

#udpipe::udpipe_download_model('spanish') # Uncomment on first run
theModel = udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")
tweetsAnn = as_tibble(udpipe_annotate(theModel, myTweets$token))


# STEMMING

myTweets = tweetsAnn %>% 
  select(token, lemma) %>% 
  filter(!is.na(lemma))

myTweets %>%
  head(10)


# WORD COUNT (AGAIN)

myTweets = myTweets %>%
  mutate(lemma=tolower(lemma)) %>% 
  filter(!lemma %in% c(stpwNtlk)) %>% 
  filter(!lemma %in% c(stpwNtlkEn))

myTweets %>% 
  head(10) 


# TOP TWEETS BY LIKE COUNT 

tweetsMyAccount = tweetsMyAccount %>% 
  arrange(desc(favorite_count))

tweetsMyAccount %>% 
  head(10) %>% 
  select(text, created_at, favorite_count) %>% 
  kable() %>%
  kable_styling(full_width = F, font_size = 12)


# TOP TWEETS BY RETWEET COUNT

tweetsMyAccount = tweetsMyAccount %>% 
  filter(is_retweet != "TRUE") %>%
  arrange(-retweet_count)

tweetsMyAccount %>% 
  head(10) %>% 
  select(text, created_at, retweet_count)%>%
  kable() %>%
  kable_styling(full_width = F, font_size = 12)


# AVERAGE TWEET LENGTH

tweetsMyAccount %>% 
  ggplot()+
  aes(x= screen_name, y= display_text_width) +
  geom_boxplot (fill="deepskyblue2") +
  labs(title = "How long or short are my tweets usually?", x="", 
       subtitle = "Average tweet-length", y = "Characters number") +
  coord_flip() 


# TWEETS COMPOSITION 

countTweets = data.frame(type=c("organic","retweets","replies"),
                         counting=c(nrow(organicMyAccount), 
                                  nrow(rtsMyAccount), 
                                  nrow(repliesMyAccount)))

countTweets = countTweets %>% 
  mutate(percentage=round(counting/sum(counting)*100,0)) %>% 
  arrange(desc(counting))


# PLOT ORIGIN TWEETS

originTweets = countTweets$percentage
names(originTweets) = countTweets$type
names(originTweets) = paste0(names(originTweets),
                             " (",originTweets,"% / ",
                             sum(originTweets),"%)")

waffle(originTweets, rows = 10) +
  labs(title="What is the origin of my tweets?",
       subtitle="Origin by type (organic, retweets, and replies)")


# TWEETS OVER TIME

ts_plot(tweetsMyAccount, by="day", color="deepskyblue3") +
  labs(x = "Date", y = "Count", title = "What is the frequency of my tweets over time?", 
       subtitle = "Tweets per day frequency")


# TWEETS PER WEEK

ggplot(tweetsMyAccount, aes(x=wday(created_at, label = TRUE))) + 
  labs(y = "Count", x = "Day", 
       title = "What is the frequency of my tweets over time?", 
       subtitle = "Tweets per day of the week") +
    geom_bar(aes(fill = ..count..))


# TWEETS PER DAY AND HOUR

tweetsMyAccount %>% 
  mutate(day = wday(created_at, label = T)) %>% 
  mutate(hour = hour(with_tz(created_at, "America/Mexico_City"))) %>% 
  plot_ly(x = ~day, y = ~hour, colors = "PuBu") %>% 
  add_histogram2d(nbinsx = 7, nbinsy = 24) %>%
  layout(title = "Tweets per day of the week and hour")


# LOCATION TWEETS

tweetsMyAccount %>%
  filter(place_full_name != "", !is.na(place_full_name)) %>% 
  count(place_full_name) %>% 
  top_n(13, n) %>% 
  ggplot(aes(x = reorder(place_full_name, n), y = n)) +
  geom_col(aes(fill = n)) +
  coord_flip() +
  labs(title = "From which places have I written the most tweets?", 
       subtitle = "Top 13 places name",
       x = "Location / Place name",
       y = "Count") +
  geom_text(aes(label = n), size = 3, hjust = 1.5, color = "white")


# MAP LOCATION TWEETS

mapMyTweets <- lat_lng(tweetsMyAccount)

mapMyTweetsGeo <- mapMyTweets %>%
  filter(is.na(lat) == FALSE & is.na(lng) == FALSE)

mapMyTweetsGeoSF <- st_as_sf(mapMyTweetsGeo, coords = c("lng", "lat"), 
                             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")

leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addCircles(data = mapMyTweetsGeoSF, 
             color = "blue")


# TWEETS SOURCE 

sourcesTwMyAccount = tweetsMyAccount %>% 
  group_by(source) %>%
  summarize(counting=n()) %>% 
  mutate(percentage=round(counting/sum(counting)*100,0)) %>% 
  arrange(desc(counting)) 


# PLOT TWEETS SOURCE

sourcesMyTweets = sourcesTwMyAccount$percentage
names(sourcesMyTweets) = sourcesTwMyAccount$source
names(sourcesMyTweets) = paste0(names(sourcesMyTweets),
                             " (",sourcesMyTweets,"% / ",
                             sum(sourcesMyTweets),"%)")

waffle(sourcesMyTweets, rows = 10) +
  scale_fill_manual(values=c("#2FA2E9", "#FF8F35", "#66e0ff", "#00CCCC", 
                             "#2e7ead", "#CC0066", "#BBAA66"))+
  labs(title="What is the source of my tweets?",
       subtitle="My tweets source by app or client")


# GET FOLLOWERS DATA

myFollowers=get_followers("cosmoduende")
myFollowersdata <- lookup_users(myFollowers$user_id)
names(myFollowersdata)


# MOST INFLUENTIAL FOLLOWERS GREATER THAN 100K

myFollowersdata%>%
  mutate(label=ifelse(friends_count>100000 | followers_count>100000,screen_name,""))%>%
  ggplot(.,aes(friends_count,followers_count))+
  geom_point(aes(colour = followers_count))+
  geom_text_repel(aes(label=label, colour = followers_count),size = 9/.pt, 
                  point.padding = 0.1, box.padding = .6, max.overlaps = 250, 
                  force = 1, min.segment.length = 0, seed = 7654)+
  labs(x="Count of friends",y="Count of followers", options(scipen=3))+
  ggtitle("Who are my most influential Twitter followers?", 
          "With Friends greater than 100k or Followers greater than 100k")


# MOST INFLUENTIAL FOLLOWERS LESS THAN 100K

myFollowersdata%>%
  filter(friends_count<=100000 & followers_count<=100000)%>%
  mutate(label=ifelse(friends_count>40000|followers_count>40000,screen_name,""))%>%
  ggplot(.,aes(friends_count,followers_count))+
  geom_point(aes(colour = followers_count))+
  geom_text_repel(aes(label=label, colour = followers_count), size = 9/.pt, 
                  point.padding = 0.1, 
                  box.padding = .6, force = 1, max.overlaps = 200,
                  min.segment.length = 0, seed = 7654)+
  labs(x="Count of friends",y="Count of followers")+
  ggtitle("Who are my most influential Twitter followers?", 
          "With Friends less than 100k and Followers less than 100k")


# MOST USED HASHTAGS

data.frame(text=unlist(organicMyAccount$hashtags)) %>% 
  filter(text != "NA" ) %>%
  count(text, sort = TRUE) %>%
  top_n(20) %>%
  mutate(text = reorder(text, n)) %>%
  ggplot(aes(x = text, y = n)) +
  geom_col(aes(fill = n)) +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Hashtags",
       title = "What are the most frequent hashtags in my Twitter account?",
       subtitle = "Top 20 most used hashtags") +
  geom_text(aes(label = n), size = 3, hjust = 1.5, color = "white")

  
# CLOUD MOST USED HASHTAGS

data.frame(text=unlist(organicMyAccount$hashtags)) %>% 
  filter(text != "null" ) %>%
  count(text, sort = TRUE) %>%
  mutate(text = reorder(text, n)) %>%
  select(word=text, freq=n) %>% 
  wordcloud2()


# MOST USED WORDS

myTweets %>% 
  filter(lemma != "im", lemma != "w", lemma != "pm", lemma != "df", lemma != "benito", lemma != "city") %>%
  count(lemma, sort = TRUE) %>%
  top_n(30) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  ggplot(aes(x = lemma, y = n)) +
  geom_col(aes(fill = n)) +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Words",
       title = "What are the most frequent words in my Twitter account?",
       subtitle = "Top 30 most used words")+
  geom_text(aes(label = n), size = 3, hjust = 1.5, color = "white")


# CLOUD MOST USED WORDS

myTweets %>% 
  filter(lemma != "im", lemma != "w", lemma != "pm") %>%
  count(lemma, sort = TRUE) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  select(word=lemma, freq=n) %>% 
  wordcloud2()


# CREATING THE PARALLELIZATION ENVIRONMENT

cl = makeCluster(detectCores()-1)
clusterExport(cl = cl, c("get_sentiment", "get_sent_values", "get_nrc_sentiment", 
                         "get_nrc_values", "parLapply"))


# SENTIMENT ANALYSIS

tweetSentimentNRC = get_nrc_sentiment(myTweets$lemma,language = "spanish", cl=cl)
stopCluster(cl)


# SENTIMENTS LABELING

tweetSentimentNRC = cbind(myTweets, tweetSentimentNRC)
tweetSentimentNRC %>% 
  filter(rowSums(tweetSentimentNRC[,-c(1,2)]) > 0) %>% 
  head()


# SENTIMENTS FREQUENCY

sentimentScores = data.frame(colSums(tweetSentimentNRC %>% 
                                       filter(lemma!="general") %>% 
                                       select(-token,-lemma)))
names(sentimentScores) = "Score"
sentimentScores = cbind("sentiment"= rownames(sentimentScores), sentimentScores)

sentimentScores = sentimentScores 

ggplot(data=sentimentScores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  xlab("Sentimients")+ylab("Scores")+
  ggtitle("What are the sentiments on my Twitter account?", 
          "Sentiments based on Score")+
  theme(axis.text.x = element_text(angle=45),
        legend.position = "none")


# CLOUD POSITIVE SENTIMENTS

tweetSentimentNRC %>% 
  filter(positive > 0) %>% 
  select(lemma) %>% 
  count(lemma) %>% 
  select(word=lemma, freq=n) %>% 
  wordcloud2()


# MOST POSITIVE WORDS

tweetSentimentNRC %>% 
  filter(positive > 0 , joy > 0 , trust > 0) %>% 
  count(lemma, sort = TRUE) %>%
  top_n(20) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  ggplot(aes(x = lemma, y = n)) +
  geom_segment(aes(x=lemma, xend=lemma, y=0, yend=n), color="grey")+
  geom_point(size=3, color="darkgreen")+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none") +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Words",
       title = "What are the most positive words in my Twitter account?",
       subtitle = "Top 20 most used words")


# MOST NEGATIVE WORDS

tweetSentimentNRC %>% 
  filter(negative > 0 , disgust > 0 , anger > 0) %>% 
  filter(lemma != "infantil") %>%
  filter(lemma != "tratar") %>%
  filter(lemma != "autor") %>%
  count(lemma, sort = TRUE) %>%
  top_n(20) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  ggplot(aes(x = lemma, y = n)) +
  geom_segment(aes(x=lemma, xend=lemma, y=0, yend=n), color="grey")+
  geom_point(size=3, color="darkred")+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none") +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Words",
       title = "What are the most negative words in my Twitter account?",
       subtitle = "Top 20 most used words")

