library(dplyr)
library(qdap)
library(tm)
library(lubridate)
library(RWeka)
library(ggplot2)
library(tidytext)
library(textdata)

#clean corpus function
clean_corpus <- function(corpus) {
  # replace numbers with text 
  corpus <- tm_map(corpus, content_transformer(removeNumbers))
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"),"true","false"))
  # Strip whitespace
  corpus <- tm_map(corpus,stripWhitespace)
  # stemDocument
  #corpus <- tm_map(corpus,stemDocument, language = "eng")
  return(corpus)
}


tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}
nrc <-get_sentiments("nrc")

#read in data
subm_list_dads_2019 <- read.csv("subm_list_dads_2019_2.csv")
subm_list_dads_2020 <- read.csv("subm_list_dads_2020_2.csv")
subm_list_moms_2019 <- read.csv("subm_list_moms_2019_2.csv")
subm_list_moms_2020 <- read.csv("subm_list_moms_2020_2.csv")

all_comments_dads_2019 <- read.csv("all_comments_dads_2019.csv")
all_comments_dads_2020 <- read.csv("all_comments_dads_2020.csv")
all_comments_moms_2019 <- read.csv("all_comments_moms_2019.csv")
all_comments_moms_2020 <- read.csv("all_comments_moms_2020.csv")



#create dataframes with data and post (title+content)
dads2019 <- data.frame("date" = as.POSIXct(subm_list_dads_2019$created_utc, origin='1970-01-01'), 
                             "post" = paste(subm_list_dads_2019$title, subm_list_dads_2019$selftext))
dads2020 <- data.frame("date" = as.POSIXct(subm_list_dads_2020$created_utc, origin='1970-01-01'), 
                       "post" = paste(subm_list_dads_2020$title, subm_list_dads_2020$selftext))
moms2019 <- data.frame("date" = as.POSIXct(subm_list_moms_2019$created_utc, origin='1970-01-01'), 
                       "post" = paste(subm_list_moms_2019$title, subm_list_moms_2019$selftext))
moms2020 <- data.frame("date" = as.POSIXct(subm_list_moms_2020$created_utc, origin='1970-01-01'), 
                       "post" = paste(subm_list_moms_2020$title, subm_list_moms_2020$selftext))

d19_pol <- polarity(dads2019$post)
d20_pol <- polarity(dads2020$post)
m19_pol <- polarity(moms2019$post)
m20_pol <- polarity(moms2020$post)


dads2019 <- dads2019 %>%
  mutate(pol = d19_pol$all$polarity) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(parent = factor("dad"))

dads2020 <- dads2020 %>%
  mutate(pol = d20_pol$all$polarity) %>%
  mutate(month = month(date))%>%
  mutate(year = year(date))%>%
  mutate(parent = factor("dad"))

moms2019 <- moms2019 %>%
  mutate(pol = m19_pol$all$polarity) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(parent = factor("mom"))

moms2020 <- moms2020 %>%
  mutate(pol = m20_pol$all$polarity) %>%
  mutate(month = month(date))%>%
  mutate(year = year(date))  %>%
  mutate(parent = factor("mom"))


m_dads2019 <- dads2019 %>%
  mutate(month = factor(month)) %>%
  group_by(month, year, parent) %>%
  summarise(avg_pol = mean(pol, na.rm = TRUE))%>%
  mutate(year = factor(year))

m_dads2020 <- dads2020 %>%
  mutate(month = factor(month)) %>%
  group_by(month,year,parent) %>%
  summarise(avg_pol = mean(pol, na.rm = TRUE)) %>%
  mutate(year = factor(year)) 

m_moms2019 <- moms2019 %>%
  mutate(month = factor(month)) %>%
  group_by(month, year,parent) %>%
  summarise(avg_pol = mean(pol, na.rm = TRUE))%>%
  mutate(year = factor(year)) 

m_moms2020 <- moms2020 %>%
  mutate(month = factor(month)) %>%
  group_by(month,year,parent) %>%
  summarise(avg_pol = mean(pol, na.rm = TRUE)) %>%
  mutate(year = factor(year)) 

mean(moms2020$pol,ra.rm = TRUE)
moms2020$pol
m_da
m_dads <- rbind(m_dads2019, m_dads2020)
m_moms <- rbind(m_moms2019, m_moms2020)
m_parent <- rbind(m_dads,m_moms)

t_moms2020 <- moms2020 %>%
  mutate(d_year = ifelse(year==2020,1,0))%>%
  mutate(d_year = factor(d_year)) 
t_moms2019 <- moms2019 %>%
  mutate(d_year = ifelse(year==2020,1,0)) %>%
  mutate(d_year = factor(d_year))
t_moms <- rbind(t_moms2019, t_moms2020)
str(t_moms)
mean(t_moms2020$pol,na.rm=TRUE)
mean(t_moms2019$pol,na.rm=TRUE)
mean(t_dads2020$pol,na.rm=TRUE)
mean(t_dads2019$pol,na.rm=TRUE)

t_dads%>%group_by(year)%>%summarise(count = n())
  
head(t_moms2020)
t.test(t_dads$pol~t_dads$d_year)
t.test(t_moms$pol~t_moms$d_year)

dads2019

t_dads2020 <- dads2020 %>%
  mutate(d_year = ifelse(year==2020,1,0))%>%
  mutate(d_year = factor(d_year)) 
t_dads2019 <- dads2019 %>%
  mutate(d_year = ifelse(year==2020,1,0)) %>%
  mutate(d_year = factor(d_year))
t_dads <- rbind(t_dads2019, t_dads2020)

m_parent %>% 
  group_by(parent,year) %>%
  summarise(avg_pol = mean(avg_pol))
str(m_parent)
ggplot(m_parent, aes(x = month, y=avg_pol, group = year:parent, color = parent))  + geom_point() + geom_line(aes(linetype = year))+
  ylab("Average Polarity") +
  xlab("Month") +
  scale_x_discrete(breaks = c(3,4,5,6,7,8,9),
                     labels=c("March", "April", "May", "June","July","August","September")) 
  
ggplot(m_dads, aes(x = month, y=avg_pol, group = year, color = year))  + geom_line() + geom_point()
ggplot(m_moms, aes(x = month, y=avg_pol, group = year, color = year))  + geom_line() + geom_point()
head(dads2019)

#t test 
d_pol_2019 = dads2019[dads2019$month==4,c("pol","year")]
d_pol_2020 = dads2020[dads2020$month==4,c("pol","year")]
t.test(d_pol_2019$pol,d_pol_2020$pol)

#t test 
m_pol_2019 = moms2019[moms2019$month==3,c("pol","year")]
m_pol_2020 = moms2020[moms2020$month==3,c("pol","year")]
t.test(m_pol_2019$pol,m_pol_2020$pol)

#t test 
m_pol_3 = moms2020[moms2020$month==3,c("pol","year")]
m_pol_4 = moms2020[moms2020$month==4,c("pol","year")]
t.test(m_pol_3$pol,m_pol_4$pol)

#create corpus and clean corpus
dads2019_source <- VectorSource(dads2019$post)
dads2019_corpus <- VCorpus(dads2019_source)
dads2019_corpus <- clean_corpus(dads2019_corpus)

dads2020_source <- VectorSource(dads2020$post)
dads2020_corpus <- VCorpus(dads2020_source)
dads2020_corpus <- clean_corpus(dads2020_corpus)

moms2019_source <- VectorSource(moms2019$post)
moms2019_corpus <- VCorpus(moms2019_source)
moms2019_corpus <- clean_corpus(moms2019_corpus)

moms2020_source <- VectorSource(moms2020$post)
moms2020_corpus <- VCorpus(moms2020_source)
moms2020_corpus <- clean_corpus(moms2020_corpus)



c(stopwords("en"))




dads2019_source_c <- VectorSource(all_comments_dads_2019$comment)
dads2019_corpus_c <- VCorpus(dads2019_source_c)
dads2019_corpus_c <- clean_corpus(dads2019_corpus_c)

dads2020_source_c <- VectorSource(all_comments_dads_2020$comment)
dads2020_corpus_c <- VCorpus(dads2020_source_c)
dads2020_corpus_c <- clean_corpus(dads2020_corpus_c)

moms2019_source_c <- VectorSource(all_comments_moms_2019$comment)
moms2019_corpus_c <- VCorpus(moms2019_source_c)
moms2019_corpus_c <- clean_corpus(moms2019_corpus_c)

moms2020_source_c <- VectorSource(all_comments_moms_2020$comment)
moms2020_corpus_c <- VCorpus(moms2020_source_c)
moms2020_corpus_c <- clean_corpus(moms2020_corpus_c)
content(moms2020_corpus_c[[502]])


#create tdm and transform to matrix
dads2019_tdm <- TermDocumentMatrix(dads2019_corpus)
dads2019_m <- as.matrix(dads2019_tdm)


dads2020_tdm <- TermDocumentMatrix(dads2020_corpus)
dads2020_m <- as.matrix(dads2020_tdm)

moms2019_tdm <- TermDocumentMatrix(moms2019_corpus)
moms2019_m <- as.matrix(moms2019_tdm)

moms2020_tdm <- TermDocumentMatrix(moms2020_corpus)
moms2020_m <- as.matrix(moms2020_tdm)

dads2020_tidy <- tidy(dads2020_tdm)
dads2019_tidy <- tidy(dads2019_tdm)

moms2020_tidy <- tidy(moms2020_tdm)
moms2019_tidy <- tidy(moms2019_tdm)

View(head(dads2019_tidy,100))

#comments
dads2019_tdm_c <- TermDocumentMatrix(dads2019_corpus_c)
dads2019_m_c <- as.matrix(dads2019_tdm_c)

dads2020_tdm_c <- TermDocumentMatrix(dads2020_corpus_c)
dads2020_m_c <- as.matrix(dads2020_tdm_c)

moms2019_tdm_c <- TermDocumentMatrix(moms2019_corpus_c) 
moms2019_m_c <- as.matrix(moms2019_tdm_c)

moms2020_tdm_c <- TermDocumentMatrix(moms2020_corpus_c)
moms2020_m_c <- as.matrix(moms2020_tdm_c)



#sum rows and sort
dads2019_freq <- rowSums(dads2019_m)
dads2019_freq <- sort(dads2019_freq, decreasing = TRUE)

dads2020_freq <- rowSums(dads2020_m)
dads2020_freq <- sort(dads2020_freq, decreasing = TRUE)

dads <- rbind(dads2019,dads2020)

dads <- dads %>%
  mutate(year=factor(year))

ggplot(dads, aes(x=pol,color = year)) + geom_histogram()




dads2020_tidy_c <- tidy(dads2020_tdm_c)
dads2019_tidy_c <- tidy(dads2019_tdm_c)

moms2020_tidy_c <- tidy(moms2020_tdm_c)
moms2019_tidy_c <- tidy(moms2019_tdm_c)

dads2020_nrc <- dads2020_tidy %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count))%>%
  # Get sentiment percentage
  mutate(average = total_count/sum(total_count)) %>%
  mutate(year = 2020)

dads2019_nrc <- dads2019_tidy %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count)) %>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(year = 2019)

moms2020_nrc <- moms2020_tidy %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count))%>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(year = 2020)

moms2019_nrc <- moms2019_tidy %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count)) %>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(year = 2019)

moms2019_nrc_c <- moms2019_tidy_c %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count)) %>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(total = total_count + moms2019_nrc$total_count) %>%
  mutate(total_avg = total/sum(total)) %>%  
  mutate(year = 2019)

moms2020_nrc_c <- moms2020_tidy_c %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count)) %>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(total = total_count + moms2020_nrc$total_count) %>%
  mutate(total_avg = total/sum(total)) %>%
  mutate(year = 2020)

dads2019_nrc_c <- dads2019_tidy_c %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count)) %>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(total = total_count + dads2019_nrc$total_count) %>%
  mutate(total_avg = total/sum(total)) %>%  
  mutate(year = 2019)

dads2020_nrc_c <- dads2020_tidy_c %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count)) %>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(total = total_count + dads2020_nrc$total_count) %>%
  mutate(total_avg = total/sum(total)) %>%
  mutate(year = 2020)

View(dads2020_tidy_c)

dads_nrc <- rbind(dads2019_nrc,dads2020_nrc)
dads_nrc <- dads_nrc %>%
  mutate(year = factor(year)) %>%
  mutate(parent = factor("dad"))
moms_nrc <- rbind(moms2019_nrc,moms2020_nrc)
moms_nrc <- moms_nrc %>%
  mutate(year = factor(year)) %>%
  mutate(parent = factor("mom"))

dads_nrc_c <- rbind(dads2019_nrc_c,dads2020_nrc_c)
dads_nrc_c <- dads_nrc_c %>%
  mutate(year = factor(year)) %>%
  mutate(parent = factor("dad")) 

moms_nrc_c <- rbind(moms2019_nrc_c,moms2020_nrc_c)
moms_nrc_c <- moms_nrc_c %>%
  mutate(year = factor(year)) %>%
  mutate(parent = factor("mom")) 

parents_nrc <- rbind(dads_nrc, moms_nrc)
parents_nrc_c <- rbind(dads_nrc_c, moms_nrc_c)
dads_nrc
sum(dads_nrc$average)

d1 = dads_nrc[dads_nrc$sentiment=="sadness"&dads_nrc$year==2019,c("total_count")]
d_total = sum(dads_nrc[dads_nrc$year==2019,]$total_count)
m1 = moms_nrc[moms_nrc$sentiment=="sadness"&moms_nrc$year==2019,c("total_count")]
m_total = sum(moms_nrc[moms_nrc$year==2019,]$total_count)
mat = matrix(c(d1[[1]],d_total,m1[[1]],m_total),ncol=2,nrow=2)

chisq.test(mat)

d1 = dads_nrc[dads_nrc$sentiment=="sadness"&dads_nrc$year==2020,c("total_count")]
d_total = sum(dads_nrc[dads_nrc$year==2020,]$total_count)
m1 = moms_nrc[moms_nrc$sentiment=="sadness"&moms_nrc$year==2020,c("total_count")]
m_total = sum(moms_nrc[moms_nrc$year==2020,]$total_count)
mat = matrix(c(d1[[1]],d_total,m1[[1]],m_total),ncol=2,nrow=2)

chisq.test(mat)


d1 = dads_nrc[dads_nrc$sentiment=="trust"&dads_nrc$year==2019,c("total_count")]
d_total = sum(dads_nrc[dads_nrc$year==2019,]$total_count)
m1 = moms_nrc[moms_nrc$sentiment=="trust"&moms_nrc$year==2019,c("total_count")]
m_total = sum(moms_nrc[moms_nrc$year==2019,]$total_count)
mat = matrix(c(d1[[1]],d_total,m1[[1]],m_total),ncol=2,nrow=2)

chisq.test(mat)

d1 = dads_nrc[dads_nrc$sentiment=="trust"&dads_nrc$year==2020,c("total_count")]
d_total = sum(dads_nrc[dads_nrc$year==2020,]$total_count)
m1 = moms_nrc[moms_nrc$sentiment=="trust"&moms_nrc$year==2020,c("total_count")]
m_total = sum(moms_nrc[moms_nrc$year==2020,]$total_count)
mat = matrix(c(d1[[1]],d_total,m1[[1]],m_total),ncol=2,nrow=2)

chisq.test(mat)


d1 = dads_nrc[dads_nrc$sentiment=="joy",c("total_count")]
d_totat19 = sum(dads_nrc[dads_nrc$year==2019,]$total_count) - d1[[1]][[1]]
d_totat20 = sum(dads_nrc[dads_nrc$year==2020,]$total_count) - d1[[1]][[2]]

m1 = dads_nrc[dads_nrc$sentiment=="joy",c("total_count")]
m_totat19 = sum(moms_nrc[moms_nrc$year==2019,]$total_count) - m1[[1]][[1]]
m_totat20 = sum(moms_nrc[moms_nrc$year==2020,]$total_count) - m1[[1]][[2]]
myl = c(d1[[1]][[1]]+m1[[1]][[1]],d1[[1]][[2]]+m1[[1]][[2]], d_totat19+m_totat19, d_totat20+m_totat20)
mat = matrix(myl,ncol=2,nrow=2)

chisq.test(mat)


d1 = dads_nrc[dads_nrc$sentiment=="anticipation",c("total_count")]
d_totat19 = sum(dads_nrc[dads_nrc$year==2019,]$total_count) - d1[[1]][[1]]
d_totat20 = sum(dads_nrc[dads_nrc$year==2020,]$total_count) - d1[[1]][[2]]

m1 = dads_nrc[dads_nrc$sentiment=="anticipation",c("total_count")]
m_totat19 = sum(moms_nrc[moms_nrc$year==2019,]$total_count) - m1[[1]][[1]]
m_totat20 = sum(moms_nrc[moms_nrc$year==2020,]$total_count) - m1[[1]][[2]]
myl = c(d1[[1]][[1]]+m1[[1]][[1]],d1[[1]][[2]]+m1[[1]][[2]], d_totat19+m_totat19, d_totat20+m_totat20)
mat = matrix(myl,ncol=2,nrow=2)

chisq.test(mat)

d1 = dads_nrc[dads_nrc$sentiment=="trust",c("total_count")]
d_totat19 = sum(dads_nrc[dads_nrc$year==2019,]$total_count) - d1[[1]][[1]]
d_totat20 = sum(dads_nrc[dads_nrc$year==2020,]$total_count) - d1[[1]][[2]]

m1 = dads_nrc[dads_nrc$sentiment=="trust",c("total_count")]
m_totat19 = sum(moms_nrc[moms_nrc$year==2019,]$total_count) - m1[[1]][[1]]
m_totat20 = sum(moms_nrc[moms_nrc$year==2020,]$total_count) - m1[[1]][[2]]
myl = c(d1[[1]][[1]]+m1[[1]][[1]],d1[[1]][[2]]+m1[[1]][[2]], d_totat19+m_totat19, d_totat20+m_totat20)
mat = matrix(myl,ncol=2,nrow=2)

chisq.test(mat)

d1 = dads_nrc[dads_nrc$sentiment=="disgust",c("total_count")]
d_totat19 = sum(dads_nrc[dads_nrc$year==2019,]$total_count) - d1[[1]][[1]]
d_totat20 = sum(dads_nrc[dads_nrc$year==2020,]$total_count) - d1[[1]][[2]]

m1 = dads_nrc[dads_nrc$sentiment=="disgust",c("total_count")]
m_totat19 = sum(moms_nrc[moms_nrc$year==2019,]$total_count) - m1[[1]][[1]]
m_totat20 = sum(moms_nrc[moms_nrc$year==2020,]$total_count) - m1[[1]][[2]]
myl = c(d1[[1]][[1]]+m1[[1]][[1]],d1[[1]][[2]]+m1[[1]][[2]], d_totat19+m_totat19, d_totat20+m_totat20)
mat = matrix(myl,ncol=2,nrow=2)

chisq.test(mat)

d1 = dads_nrc[dads_nrc$sentiment=="fear",c("total_count")]
d_totat19 = sum(dads_nrc[dads_nrc$year==2019,]$total_count) - d1[[1]][[1]]
d_totat20 = sum(dads_nrc[dads_nrc$year==2020,]$total_count) - d1[[1]][[2]]

m1 = dads_nrc[dads_nrc$sentiment=="fear",c("total_count")]
m_totat19 = sum(moms_nrc[moms_nrc$year==2019,]$total_count) - m1[[1]][[1]]
m_totat20 = sum(moms_nrc[moms_nrc$year==2020,]$total_count) - m1[[1]][[2]]
myl = c(d1[[1]][[1]]+m1[[1]][[1]],d1[[1]][[2]]+m1[[1]][[2]], d_totat19+m_totat19, d_totat20+m_totat20)
mat = matrix(myl,ncol=2,nrow=2)

chisq.test(mat)

help(chisq.test())
ggplot(dads_nrc, aes(x=sentiment, y = average, fill = year)) + geom_bar(stat="identity",position=position_dodge()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.0,.23)) +
  ylab("Percent Sentiment") +
  xlab("Sentiment")+
  ggtitle("Daddit Sentiment")
  
ggplot(moms_nrc, aes(x=sentiment, y = average, fill = year)) + geom_bar(stat="identity",position=position_dodge())+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.0,.23)) +
  ylab("Percent Sentiment") +
  xlab("Sentiment")+
  ggtitle("Mommit Sentiment")
ggplot(parents_nrc, aes(x=sentiment, y = average, fill = factor(parent:year))) + geom_bar(stat="identity",position=position_dodge())

ggplot(moms2019_nrc_c, aes(x=sentiment, y = average)) + geom_bar(stat="identity",position=position_dodge())

ggplot(moms_nrc_c, aes(x=sentiment, y = average, fill = year)) + geom_bar(stat="identity",position=position_dodge())
ggplot(dads_nrc_c, aes(x=sentiment, y = average, fill = year)) + geom_bar(stat="identity",position=position_dodge())

ggplot(parents_nrc_c, aes(x=sentiment, y = average, fill = factor(year:parent))) +  geom_bar(stat="identity",position=position_dodge())

covid <- read.csv("us.csv")
head(covid)
covid_month <- covid %>%
  group_by(month) %>%
  summarise(cases_month = sum(new_cases)/1000000) %>%
  filter(month>=3 & month<=9)

library(grid)
library(gridExtra)
library(lattice)
moms_covid
parent_covid <- m_parent %>%
  filter( year == 2020) %>%
  merge(covid_month) %>%
  mutate(month = as.integer(month))
m_moms_2020 <- m_moms %>%
  filter(year==2020) %>%
  mutate(month=as.integer(month))
m_moms_2020
str(m_moms_2020)
m_moms_2020
g1<- ggplot(parent_covid )  + geom_line(aes(x = month, y=avg_pol,color= parent,group=parent)) + geom_point(aes(x = month, y=avg_pol,color = parent,group = parent))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7),
                      labels=c("March", "April", "May", "June","July","August","September")) +
  ylab("Average Polarity")+
  xlab("")+
  theme(legend.position = "top")
ylab <- c(0, 0.5, 1, 1.5,2.0)

g2 <- ggplot(covid_month, aes(x=month, y =cases_month)) + geom_line()+
  scale_x_continuous(breaks = c(3,4,5,6,7,8,9),
                     labels=c("March", "April", "May", "June","July","August","September"))+
  ylab("COVID Cases/Month (Million)") +
  xlab("Month")#  +  
  #scale_y_continuous(labels = paste(ylab, "M"),breaks = ylab)
  
grid.arrange(g1,g2, nrow=2)
g2

grid.arrange(g1,g2, nrow=2)

g1



##### Emotion over time
dads2020_d <- dads2020 %>%
  mutate(date = format(date, "%Y-%m-%d"))

date_seq <- seq(as.Date("2020-03-16"), as.Date("2020-09-15"), by="days")

dads2020_source <- VectorSource(dads2020_d[dads2020_d$date==date_seq[1],]$post)
dads2020_corpus <- VCorpus(dads2020_source)
dads2020_corpus <- clean_corpus(dads2020_corpus)

dads2020_tdm <- DocumentTermMatrix(dads2020_corpus)#control = list(tokenize = tokenizer))
dads2020_m <- as.matrix(dads2020_tdm)
dads2020_tidy <- tidy(dads2020_tdm)


dads2020_nrc_d <- dads2020_tidy %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count))%>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(date = date_seq[1]) %>%
  mutate(month = month(date))

for (d in seq_along(date_seq)) {
  if(d==1) next
  day <- date_seq[d]
  
  dads2020_source <- VectorSource(dads2020_d[dads2020_d$date==date_seq[d],]$post)
  dads2020_corpus <- VCorpus(dads2020_source)
  dads2020_corpus <- clean_corpus(dads2020_corpus)
  
  dads2020_tdm <- DocumentTermMatrix(dads2020_corpus)#control = list(tokenize = tokenizer))
  dads2020_m <- as.matrix(dads2020_tdm)
  dads2020_tidy <- tidy(dads2020_tdm)
  
  dads2020_nrc_dd <- dads2020_tidy %>% 
    # Join to nrc lexicon by term = word
    inner_join(nrc, by = c("term" = "word")) %>% 
    # Only consider Plutchik sentiments
    filter(!sentiment %in% c("positive", "negative")) %>%
    # Group by sentiment
    group_by(sentiment) %>% 
    # Get total count by sentiment
    summarize(total_count = sum(count))%>%
    mutate(average = total_count/sum(total_count)) %>%
    mutate(date = date_seq[d]) %>%
    mutate(month = month(date))
  
  dads2020_nrc_d <- rbind(dads2020_nrc_d, dads2020_nrc_dd)

}
dads2020_nrc_d
ggplot(dads2020_nrc_d) + geom_line(aes(y = average,x=date,group = sentiment, color = sentiment))

dads2020_nrc_m <- dads2020_nrc_d %>%
  group_by(sentiment,month) %>%
  summarise(avg = mean(average),total_count = sum(total_count))



g4<-ggplot(dads2020_nrc_m) + geom_line(aes(y = avg,x=month,group = sentiment, color = sentiment)) + 
  geom_point(aes(y = avg,x=month,group = sentiment, color = sentiment)) +
  scale_x_continuous(breaks = c(3,4,5,6,7,8,9),
    labels=c("March", "April", "May", "June","July","August","September")) +
  ylab("Percentage of Sentiment")+
  xlab("Month")+
  ggtitle("Daddit Sentiment in 2020") +
  scale_y_continuous(breaks = c(0.05,0.07,0.09,0.11,0.13,0.15,0.17,.19,0.21,.23),
                     labels = scales::percent_format(accuracy = 1), limits = c(0.05,.23)) 
 


dads2020_nrc_m



ggplot(m_dads[m_dads$year==2020,], aes(x = month, y=avg_pol, group = year, color = year))  + geom_smooth()

moms2020_d <- moms2020 %>%
  mutate(date = format(date, "%Y-%m-%d"))

date_seq <- seq(as.Date("2020-03-16"), as.Date("2020-09-15"), by="days")

moms2020_source <- VectorSource(moms2020_d[moms2020_d$date==date_seq[1],]$post)
moms2020_corpus <- VCorpus(moms2020_source)
moms2020_corpus <- clean_corpus(moms2020_corpus)
head(dads2020_d)

moms2020_tdm <- DocumentTermMatrix(moms2020_corpus)#control = list(tokenize = tokenizer))
moms2020_m <- as.matrix(moms2020_tdm)
moms2020_tidy <- tidy(moms2020_tdm)


moms2020_nrc_d <- moms2020_tidy %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count))%>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(date = date_seq[1]) %>%
  mutate(month = month(date))

moms2020_nrc_d
date_seq
for (d in seq_along(date_seq)) {
  if(d==1) next
  
  moms2020_source <- VectorSource(moms2020_d[moms2020_d$date==date_seq[d],]$post)
  moms2020_corpus <- VCorpus(moms2020_source)
  moms2020_corpus <- clean_corpus(moms2020_corpus)
  
  moms2020_tdm <- DocumentTermMatrix(moms2020_corpus)#control = list(tokenize = tokenizer))
  moms2020_m <- as.matrix(moms2020_tdm)
  moms2020_tidy <- tidy(moms2020_tdm)
  
  moms2020_nrc_dd <- moms2020_tidy %>% 
    # Join to nrc lexicon by term = word
    inner_join(nrc, by = c("term" = "word")) %>% 
    # Only consider Plutchik sentiments
    filter(!sentiment %in% c("positive", "negative")) %>%
    # Group by sentiment
    group_by(sentiment) %>% 
    # Get total count by sentiment
    summarize(total_count = sum(count))%>%
    mutate(average = total_count/sum(total_count)) %>%
    mutate(date = date_seq[d]) %>%
    mutate(month = month(date))
  moms2020_nrc_d <- rbind(moms2020_nrc_d, moms2020_nrc_dd)
}

moms2020_nrc_m <- moms2020_nrc_d %>%
  group_by(sentiment,month) %>%
  summarise(avg = mean(average),total_count = sum(total_count))
moms2020_nrc_m
g3<- ggplot(moms2020_nrc_m) + geom_line(aes(y = avg,x=month,group = sentiment, color = sentiment)) + 
  geom_point(aes(y = avg,x=month,group = sentiment, color = sentiment)) +
  scale_x_continuous(breaks = c(3,4,5,6,7,8,9),
                     labels=c("March", "April", "May", "June","July","August","September")) +
  ylab("Percentage of Sentiment")+
  xlab("Month")+
  ggtitle("Mommit Sentiment in 2020") +
  scale_y_continuous(breaks = c(0.05,0.07,0.09,0.11,0.13,0.15,0.17,.19,0.21,.23),
                     labels = scales::percent_format(accuracy = 1), limits = c(0.05,.23)) 
grid.arrange(g1,g2, nrow=2)

g3
g4



moms2019_d <- moms2019 %>%
  mutate(date = format(date, "%Y-%m-%d"))

date_seq <- seq(as.Date("2019-03-16"), as.Date("2019-09-15"), by="days")

moms2019_source <- VectorSource(moms2019_d[moms2019_d$date==date_seq[1],]$post)
moms2019_corpus <- VCorpus(moms2019_source)
moms2019_corpus <- clean_corpus(moms2019_corpus)
head(dads2020_d)

moms2019_tdm <- DocumentTermMatrix(moms2019_corpus)#control = list(tokenize = tokenizer))
moms2019_m <- as.matrix(moms2019_tdm)
moms2019_tidy <- tidy(moms2019_tdm)


moms2019_nrc_d <- moms2019_tidy %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count))%>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(date = date_seq[1]) %>%
  mutate(month = month(date))

for (d in seq_along(date_seq)) {
  if(d==1) next
  day <- date_seq[d]
  
  moms2019_source <- VectorSource(moms2019_d[moms2019_d$date==date_seq[d],]$post)
  moms2019_corpus <- VCorpus(moms2019_source)
  moms2019_corpus <- clean_corpus(moms2019_corpus)
  
  moms2019_tdm <- DocumentTermMatrix(moms2019_corpus)#control = list(tokenize = tokenizer))
  moms2019_m <- as.matrix(moms2019_tdm)
  moms2019_tidy <- tidy(moms2019_tdm)
  
  moms2019_nrc_dd <- moms2019_tidy %>% 
    # Join to nrc lexicon by term = word
    inner_join(nrc, by = c("term" = "word")) %>% 
    # Only consider Plutchik sentiments
    filter(!sentiment %in% c("positive", "negative")) %>%
    # Group by sentiment
    group_by(sentiment) %>% 
    # Get total count by sentiment
    summarize(total_count = sum(count))%>%
    mutate(average = total_count/sum(total_count)) %>%
    mutate(date = date_seq[d]) %>%
    mutate(month = month(date))
  
  moms2019_nrc_d <- rbind(moms2019_nrc_d, moms2019_nrc_dd)
  
}

moms2019_nrc_m <- moms2019_nrc_d %>%
  group_by(sentiment,month) %>%
  summarise(avg = mean(average),total_count = sum(total_count))

g5<- ggplot(moms2019_nrc_m) + geom_line(aes(y = avg,x=month,group = sentiment, color = sentiment)) + 
  geom_point(aes(y = avg,x=month,group = sentiment, color = sentiment)) +
  scale_x_continuous(breaks = c(3,4,5,6,7,8,9),
                     labels=c("March", "April", "May", "June","July","August","September")) +
  ylab("Percentage of Sentiment")+
  xlab("Month")+
  ggtitle("Mommit Sentiment in 2019") +
  scale_y_continuous(breaks = c(0.05,0.07,0.09,0.11,0.13,0.15,0.17,.19,0.21,.23),
                     labels = scales::percent_format(accuracy = 1), limits = c(0.05,.23)) 
g5


dads2019_d <- dads2019 %>%
  mutate(date = format(date, "%Y-%m-%d"))

date_seq <- seq(as.Date("2019-03-16"), as.Date("2019-09-15"), by="days")

dads2019_source <- VectorSource(dads2019_d[dads2019_d$date==date_seq[1],]$post)
dads2019_corpus <- VCorpus(dads2019_source)
dads2019_corpus <- clean_corpus(dads2019_corpus)
head(dads2020_d)

dads2019_tdm <- DocumentTermMatrix(dads2019_corpus)#control = list(tokenize = tokenizer))
dads2019_m <- as.matrix(dads2019_tdm)
dads2019_tidy <- tidy(dads2019_tdm)


dads2019_nrc_d <- dads2019_tidy %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count))%>%
  mutate(average = total_count/sum(total_count)) %>%
  mutate(date = date_seq[1]) %>%
  mutate(month = month(date))

for (d in seq_along(date_seq)) {
  if(d==1) next
  day <- date_seq[d]
  
  dads2019_source <- VectorSource(dads2019_d[dads2019_d$date==date_seq[d],]$post)
  dads2019_corpus <- VCorpus(dads2019_source)
  dads2019_corpus <- clean_corpus(dads2019_corpus)
  
  dads2019_tdm <- DocumentTermMatrix(dads2019_corpus)#control = list(tokenize = tokenizer))
  dads2019_m <- as.matrix(dads2019_tdm)
  dads2019_tidy <- tidy(dads2019_tdm)
  
  dads2019_nrc_dd <- dads2019_tidy %>% 
    # Join to nrc lexicon by term = word
    inner_join(nrc, by = c("term" = "word")) %>% 
    # Only consider Plutchik sentiments
    filter(!sentiment %in% c("positive", "negative")) %>%
    # Group by sentiment
    group_by(sentiment) %>% 
    # Get total count by sentiment
    summarize(total_count = sum(count))%>%
    mutate(average = total_count/sum(total_count)) %>%
    mutate(date = date_seq[d]) %>%
    mutate(month = month(date))
  
  dads2019_nrc_d <- rbind(dads2019_nrc_d, dads2019_nrc_dd)
  
}

dads2019_nrc_m <- dads2019_nrc_d %>%
  group_by(sentiment,month) %>%
  summarise(avg = mean(average),total_count = sum(total_count))

g5<- ggplot(dads2019_nrc_m) + geom_line(aes(y = avg,x=month,group = sentiment, color = sentiment)) + 
  geom_point(aes(y = avg,x=month,group = sentiment, color = sentiment)) +
  scale_x_continuous(breaks = c(3,4,5,6,7,8,9),
                     labels=c("March", "April", "May", "June","July","August","September")) +
  ylab("Percentage of Sentiment")+
  xlab("Month")+
  ggtitle("Daddit Sentiment in 2019") +
  scale_y_continuous(breaks = c(0.05,0.07,0.09,0.11,0.13,0.15,0.17,.19,0.21,.23),
                     labels = scales::percent_format(accuracy = 1), limits = c(0.05,.23)) 

dads2019_nrc_m 
dads2020_nrc_m 

moms2019_nrc_m[moms2019_nrc_m$sentiment=="trust",] 
moms2020_nrc_m[moms2020_nrc_m$sentiment=="anticipation",] 

m3 = moms2019_nrc_m[moms2019_nrc_m$sentiment=="anticipation"&moms2019_nrc_m$month==3,"total_count"]
m4 = moms2019_nrc_m[moms2019_nrc_m$sentiment=="anticipation"&moms2019_nrc_m$month==4,"total_count"]

m3tot= sum(moms2019_nrc_m[moms2019_nrc_m$month==3,"total_count"]) - m3[[1]]
m4tot= sum(moms2019_nrc_m[moms2019_nrc_m$month==4,"total_count"]) - m4[[1]]

myl = c(m3[[1]],m4[[1]],m3tot,m4tot)
mat = matrix(myl,ncol=2,nrow=2)
mat

chisq.test(mat)


m3 = moms2020_nrc_m[moms2020_nrc_m$sentiment=="anticipation"&moms2020_nrc_m$month==3,"total_count"]
m4 = moms2020_nrc_m[moms2020_nrc_m$sentiment=="anticipation"&moms2020_nrc_m$month==4,"total_count"]
m3
m3tot= sum(moms2020_nrc_m[moms2020_nrc_m$month==3,"total_count"]) - m3[[1]]
m4tot= sum(moms2020_nrc_m[moms2020_nrc_m$month==4,"total_count"]) - m4[[1]]

myl = c(m3[[1]],m4[[1]],m3tot,m4tot)
myl
mat = matrix(myl,ncol=2,nrow=2)
mat

chisq.test(mat)
mat[1]/mat[1,2]
mat[1,2]

m3 = moms2019_nrc_m[moms2019_nrc_m$sentiment=="fear"&moms2019_nrc_m$month==3,"total_count"]
m4 = moms2019_nrc_m[moms2019_nrc_m$sentiment=="fear"&moms2019_nrc_m$month==4,"total_count"]

m3tot= sum(moms2019_nrc_m[moms2019_nrc_m$month==3,"total_count"]) - m3[[1]]
m4tot= sum(moms2019_nrc_m[moms2019_nrc_m$month==4,"total_count"]) - m4[[1]]

myl = c(m3[[1]],m4[[1]],m3tot,m4tot)
mat = matrix(myl,ncol=2,nrow=2)
mat

chisq.test(mat)

m3 = moms2020_nrc_m[moms2020_nrc_m$sentiment=="anger"&moms2020_nrc_m$month==8,"total_count"]
m4 = moms2020_nrc_m[moms2020_nrc_m$sentiment=="anger"&moms2020_nrc_m$month==9,"total_count"]

m3tot= sum(moms2020_nrc_m[moms2020_nrc_m$month==8,"total_count"]) - m3[[1]]
m4tot= sum(moms2020_nrc_m[moms2020_nrc_m$month==9,"total_count"]) - m4[[1]]

myl = c(m3[[1]],m4[[1]],m3tot,m4tot)
mat = matrix(myl,ncol=2,nrow=2)
mat

chisq.test(mat)


m3 = moms2020_nrc_m[moms2020_nrc_m$sentiment=="trust"&moms2020_nrc_m$month==7,"total_count"]
m4 = moms2020_nrc_m[moms2020_nrc_m$sentiment=="trust"&moms2020_nrc_m$month==9,"total_count"]

m3tot= sum(moms2020_nrc_m[moms2020_nrc_m$month==7,"total_count"]) - m3[[1]]
m4tot= sum(moms2020_nrc_m[moms2020_nrc_m$month==9,"total_count"]) - m4[[1]]

myl = c(m3[[1]],m4[[1]],m3tot,m4tot)
mat = matrix(myl,ncol=2,nrow=2)
mat

chisq.test(mat)


m3 = dads2019_nrc_m[dads2019_nrc_m$sentiment=="trust"&dads2019_nrc_m$month==8,"total_count"]
m4 = dads2019_nrc_m[dads2019_nrc_m$sentiment=="trust"&dads2019_nrc_m$month==9,"total_count"]

m3tot= sum(dads2019_nrc_m[dads2019_nrc_m$month==8,"total_count"]) - m3[[1]]
m4tot= sum(dads2019_nrc_m[dads2019_nrc_m$month==9,"total_count"]) - m4[[1]]

myl = c(m3[[1]],m4[[1]],m3tot,m4tot)
mat = matrix(myl,ncol=2,nrow=2)
mat

chisq.test(mat)

dads2019_nrc_m[dads2019_nrc_m$sentiment=="anger",] 
dads2020_nrc_m[dads2020_nrc_m$sentiment=="anger",] 


m3 = dads2019_nrc_m[dads2019_nrc_m$sentiment=="anger"&dads2019_nrc_m$month==7,"total_count"]
m4 = dads2019_nrc_m[dads2019_nrc_m$sentiment=="anger"&dads2019_nrc_m$month==9,"total_count"]

m3tot= sum(dads2019_nrc_m[dads2019_nrc_m$month==7,"total_count"]) - m3[[1]]
m4tot= sum(dads2019_nrc_m[dads2019_nrc_m$month==9,"total_count"]) - m4[[1]]

myl = c(m3[[1]],m4[[1]],m3tot,m4tot)
mat = matrix(myl,ncol=2,nrow=2)

chisq.test(mat)


m3 = dads2020_nrc_m[dads2020_nrc_m$sentiment=="anger"&dads2020_nrc_m$month==5,"total_count"]
m4 = dads2020_nrc_m[dads2020_nrc_m$sentiment=="anger"&dads2020_nrc_m$month==6,"total_count"]

m3tot= sum(dads2020_nrc_m[dads2020_nrc_m$month==5,"total_count"]) - m3[[1]]
m4tot= sum(dads2020_nrc_m[dads2020_nrc_m$month==6,"total_count"]) - m4[[1]]

myl = c(m3[[1]],m4[[1]],m3tot,m4tot)
mat = matrix(myl,ncol=2,nrow=2)

chisq.test(mat)


dads2019_nrc_m[dads2019_nrc_m$sentiment=="joy",] 
dads2020_nrc_m[dads2020_nrc_m$sentiment=="joy",] 


m3 = dads2019_nrc_m[dads2019_nrc_m$sentiment=="joy"&dads2019_nrc_m$month==3,"total_count"]
m4 = dads2019_nrc_m[dads2019_nrc_m$sentiment=="joy"&dads2019_nrc_m$month==4,"total_count"]

m3tot= sum(dads2019_nrc_m[dads2019_nrc_m$month==3,"total_count"]) - m3[[1]]
m4tot= sum(dads2019_nrc_m[dads2019_nrc_m$month==4,"total_count"]) - m4[[1]]

myl = c(m3[[1]],m4[[1]],m3tot,m4tot)
mat = matrix(myl,ncol=2,nrow=2)

chisq.test(mat)


m3 = dads2020_nrc_m[dads2020_nrc_m$sentiment=="anger"&dads2020_nrc_m$month==3,"total_count"]
m4 = dads2020_nrc_m[dads2020_nrc_m$sentiment=="anger"&dads2020_nrc_m$month==4,"total_count"]

m3tot= sum(dads2020_nrc_m[dads2020_nrc_m$month==3,"total_count"]) - m3[[1]]
m4tot= sum(dads2020_nrc_m[dads2020_nrc_m$month==4,"total_count"]) - m4[[1]]

myl = c(m3[[1]],m4[[1]],m3tot,m4tot)
mat = matrix(myl,ncol=2,nrow=2)

chisq.test(mat)
