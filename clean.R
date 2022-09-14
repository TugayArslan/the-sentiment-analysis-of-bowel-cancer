install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("tidytext")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("syuzhet")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("reshape2")

library(httr)
library(jsonlite)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(tidyverse)
library(tidytext)
library(tidyr)

files <- c(
           'health-tweets.csv',
           'ni-tweets.csv',
           'pro-tweets.csv',
           'emergency-tweets.csv',
           'charity-tweets.csv',
           'media-tweets.csv',
           'belfast-query-tweets.csv',
           'belfast-hashtag-tweets.csv',
           'hashtag-tweets.csv',
           'text-query-tweets.csv'
          )
files <- c(
  'health-tweets-detailed.csv',
  'ni-tweets-detailed.csv',
  'pro-tweets-detailed.csv',
  'emergency-tweets-detailed.csv',
  'charity-tweets-detailed.csv',
  'media-tweets-detailed.csv',
  'belfast-query-tweets-detailed.csv',
  'belfast-hashtag-tweets-detailed.csv',
  'hashtag-tweets-detailed.csv',
  'text-query-tweets-detailed.csv'
)
files <- c(
  'belfastnorthernireland-query-tweets-detailed.csv',
  'belfastnorthernireland-hashtag-tweets-detailed.csv',
  'cardiffwales-query-tweets-detailed.csv',
  'cardiffwales-hashtag-tweets-detailed.csv',
  'dublinireland-query-tweets-detailed.csv',
  'dublinireland-hashtag-tweets-detailed.csv',
  'edinburghscotland-query-tweets-detailed.csv',
  'edinburghscotland-hashtag-tweets-detailed.csv',
  'londonengland-query-tweets-detailed.csv',
  'londonengland-hashtag-tweets-detailed.csv',
  'englanduk-query-tweets-detailed.csv',
  'englanduk-hashtag-tweets-detailed.csv',
  'northernirelanduk-query-tweets-detailed.csv',
  'northernirelanduk-hashtag-tweets-detailed.csv',
  'roi-query-tweets-detailed.csv',
  'roi-hashtag-tweets-detailed.csv',
  'scotlanduk-query-tweets-detailed.csv',
  'scotlanduk-hashtag-tweets-detailed.csv',
  'walesuk-query-tweets-detailed.csv',
  'walesuk-hashtag-tweets-detailed.csv',
  'unitedkingdom-query-tweets-detailed.csv',
  'unitedkingdom-hashtag-tweets-detailed.csv',
  'hashtag-tweets-detailed.csv',
  'text-query-tweets-detailed.csv'
)

df.complete <- data.frame()
size <- 0

for (file in files)
{
        data <- read.csv(file = paste0('./Exports/', file))
        #head(data)
        print(paste0(file, ": ", length(data)))

        df.temp <- data %>%
                mutate(File = file, .before = Text)

        #if (file == 'hashtag-tweets.csv' || file == 'text-query-tweets.csv')
        if (file == 'hashtag-tweets-detailed.csv' || file == 'text-query-tweets-detailed.csv')
        {
                df.temp_filtered <- filter(df.temp, grepl("Armagh|Belfast|B[ée]al Feirste|Bilfawst|Derry|Londonderry|Lisburn|Newry|
                                                          Newtownabbey|North Down|Banbridge|Craigavon|Strabane|Castlereagh|Ballymena|Carrickfergus|Larne|Ulster|Dungannon|
                                                          Antrim|\bCo[ .]?Down\b|Fermanagh|Tyrone|Downpatrick|Enniskillen|Coleraine|Omagh|
                                                          \bN[ .]?I\b|\bN[ .-_]?Ireland\b|Northern[ .-_]?Ireland", 
                                                          Location, ignore.case = TRUE))
#                df.temp_filtered <- filter(df.temp, grepl("Republic of Ireland|Ireland|N\\.I\\.|Northern Ireland|Scotland|Wales|England|United Kingdom|U\\. Kingdom|U\\.K\\.", Location, ignore.case = TRUE))
        }
        #else if (file != 'belfast-hashtag-tweets.csv' && file != 'belfast-query-tweets.csv')
        else if (file != 'belfast-hashtag-tweets-detailed.csv' && file != 'belfast-query-tweets-detailed.csv')
        {
#                df.temp_filtered <- filter(df.temp, grepl("bowel cancer|colon cancer|colorectal cancer|bowelcancer|coloncancer|colorectalcancer", Text, ignore.case = TRUE))
                df.temp_filtered <- filter(df.temp, grepl("(bowel.*cancer)|(colon.*cancer)|(colorectal.*cancer)|(cancer.*bowel)|(cancer.*colon)|(cancer.*colorectal)", Text, ignore.case = TRUE))
        }
        else
        {
                df.temp_filtered <- df.temp
        }

        names(df.temp_filtered) <- c("Url","Datetime","Tweet.Id","File","Text","Username","Location","Hastags","Reply.Count",
                                     "Retweet.Count","Like.Count","Quote.Count","Conv..Id","Language","Retweeted.Tweet",
                                     "Quoted.Tweet","Mentioned.Users","Replied.Tweet","Replied.User")
        #print(colnames(df.temp_filtered))
        #print(identical(names(df.complete), names(df.temp_filtered)))
        size <- size + length(df.temp_filtered$File)
        df.complete <- rbind(df.complete, df.temp_filtered)
}

df <- df.complete[!duplicated(df.complete$Tweet.Id), ]
unique(df$File)
head(df)
print(length(df$File))
write.csv(df, 'df.csv', row.names = FALSE)
write.csv(df, 'df_detailed.csv', row.names = FALSE)
write.csv(df, 'df_ukireland.csv', row.names = FALSE)

#dataset <- read.csv(file = './Exports/belfast-query-tweets-detailed.csv')
df <- read.csv(file = './df.csv')
dataset <- read.csv(file = './df.csv')
head(dataset)

dataset <- dataset %>% rename(doc_id = Tweet.Id, text = Text)
data <- Corpus(DataframeSource(dataset))

data <- tm_map(data, PlainTextDocument)
data <- tm_map(data, content_transformer(tolower))

data <- tm_map(data, gsub, pattern = '&amp;', replacement = ' ')

removeMention <- function(x) gsub("@\\S+", " ", x, perl=T)
data <- tm_map(data, content_transformer(removeMention))

removeHashtag <- function(x) gsub("#\\S+", " ", x, perl=T)
data <- tm_map(data, content_transformer(removeHashtag))

data <- tm_map(data, removeWords, c('rt'))

removeURL <- function(x) gsub("(f|ht)tp(s?)://\\S+", " ", x, perl=T)
data <- tm_map(data, content_transformer(removeURL))

data <- tm_map(data, removeWords, stopwords('english'))
data <- tm_map(data, removePunctuation)
data <- tm_map(data, removeNumbers)
#data <- tm_map(data, stemDocument)
data <- tm_map(data, stripWhitespace)

inspect(data[1:25])

tdm <- TermDocumentMatrix(data)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Sort by descearing value of frequency
dtm_v <- sort(rowSums(tdm),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 10)

# Plot the most frequent words
barplot(dtm_d[1:10,]$freq, las = 2, names.arg = dtm_d[1:10,]$word,
        col = rainbow(10), main ="Top 10 most frequent words",
        ylab = "Word frequencies")
#OR
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)


dataframe <- data.frame(text=sapply(data, identity),
                        stringsAsFactors=F)
#dataframe <- data.frame(text = get("content", mycorpus))
dataframe <- dataframe %>%
        mutate(Date = as.Date(df$Datetime)) %>%
        mutate(Year = format(as.POSIXct(Date), format="%Y")) %>%
        mutate(Id = df$Tweet.Id) %>%
        mutate(File = df$File) %>%
        mutate(Username = df$Username) %>%
        mutate(Location = df$Location)

s <- get_nrc_sentiment(dataframe$text)
head(s)

s_v <- sort(colSums(s),decreasing=TRUE)
barplot(s_v,
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores - Tweets in N.I.')

barplot(sort(colSums(prop.table(s))),
        las = 2,
        col = rainbow(10),
        ylab = 'Percentage',
        main = 'Sentiment Scores Tweets')

s_new <- s %>%
        mutate(Date = dataframe$Datetime) %>%
        mutate(Year = dataframe$Year) %>%
        mutate(Id = dataframe$Tweet.Id) %>%
        mutate(File = dataframe$File) %>%
        mutate(Username = dataframe$Username) %>%
        mutate(Location = dataframe$Location)

s_year <- s_new %>%
        group_by(Year) %>%
        summarise(anger = sum(anger), anticipation = sum(anticipation),
                  disgust = sum(disgust), fear = sum(fear),
                  joy = sum(joy), sadness = sum(sadness),
                  surprise = sum(surprise), trust = sum(trust),
                  negative = sum(negative), positive = sum(positive))

s_table <- s_year %>%
        pivot_longer(c('anger','anticipation','disgust','fear','joy','sadness','surprise','trust','negative','positive'),
                     names_to = "Emotion", values_to = "Value") %>%
        filter(Year %in% c(2017, 2018, 2019, 2020, 2021, 2022))

p = ggplot(data = s_table, aes(x=reorder(Emotion, -Value), y=Value, fill=Emotion))
p = p + geom_bar(stat='identity')
p = p + facet_grid(~Year)
p = p + theme_minimal()
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p = p + ggtitle("Sentiment per year \n 2017-2022")
p = p + xlab("") + ylab("Count")
p

library(splitstackshape)
df2 <- cSplit(dataframe, "text", sep = " ", direction = "long")
names(df2)[names(df2) == 'text'] <- 'word'

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

afinn <- df2 %>%
        filter(!word %in% c('cancer', 'bowel', 'colon', 'colorectal')) %>%
        inner_join(get_sentiments("afinn")) %>%
        group_by(Year, index = week(Date)) %>%
        summarise(sentiment = sum(value)) %>%
        mutate(method = "AFINN")

bing <- df2 %>%
        filter(!word %in% c('cancer', 'bowel', 'colon', 'colorectal')) %>%
        inner_join(get_sentiments("bing")) %>%
        count(Year, index = week(Date), sentiment) %>%
        pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
        mutate(sentiment = positive - negative) %>%
        mutate(method = "BING")

nrc <- df2 %>%
        filter(!word %in% c('cancer', 'bowel', 'colon', 'colorectal')) %>%
        inner_join(get_sentiments("nrc")) %>%
        filter(sentiment %in% c("positive", "negative")) %>%
        count(Year, index = week(Date), sentiment) %>%
        pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
        mutate(sentiment = positive - negative) %>%
        mutate(method = "NRC")

sentiment <- bind_rows(afinn, bing, nrc)

sentiment %>%
        filter(Year %in% c(2017, 2018, 2019, 2020, 2021, 2022)) %>%
        ggplot(aes(index, sentiment, fill = method)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~method+Year, nrow = 3, scales = "free_y")

bing_word_counts <- df2 %>%
        filter(!word %in% c('cancer', 'bowel', 'colon', 'colorectal')) %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()

bing_word_counts %>%
        group_by(sentiment) %>%
        slice_max(n, n = 10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(x = "Contribution to sentiment",
             y = NULL)

df2 %>%
        filter(!word %in% c('cancer', 'bowel', 'colon', 'colorectal')) %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("gray20", "gray80"),
                         max.words = 100)

bingnegative <- get_sentiments("bing") %>%
        filter(sentiment == "negative")

wordcounts <- df2 %>%
        mutate(Month = month(Date)) %>%
        group_by(Year, Month) %>%
        summarize(words = n())

df3 <- df2 %>%
        mutate(Month = month(Date)) %>%
        semi_join(bingnegative) %>%
        group_by(Year, Month) %>%
        summarize(negativewords = n()) %>%
        left_join(wordcounts, by = c("Year", "Month")) %>%
        mutate(ratio = negativewords/words) %>%
        filter(Month != 0) %>%
        ungroup()

df3_sum <- df3 %>%
        filter(Year %in% c(2017, 2018, 2019, 2020, 2021, 2022)) %>%
        group_by(Year) %>%
        summarize(negativewords = sum(negativewords), words = sum(words)) %>%
        mutate(positivewords = words-negativewords) %>%
        mutate(ratio = negativewords/words) %>%
        pivot_longer(cols = c("negativewords", "positivewords"), names_to = "type", values_to = "value")

df3_max <- df2 %>%
        mutate(Month = month(Date)) %>%
        semi_join(bingnegative) %>%
        group_by(Year, Month) %>%
        summarize(negativewords = n()) %>%
        left_join(wordcounts, by = c("Year", "Month")) %>%
        mutate(ratio = negativewords/words) %>%
        filter(Month != 0) %>%
        slice_max(ratio, n = 1) %>%
        ungroup()

ggplot(df3_sum, aes(fill=type, y=value, x=Year)) +
        geom_bar(position="stack", stat="identity")
