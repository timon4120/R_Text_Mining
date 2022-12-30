library(gutenbergr)
library(dplyr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(gridExtra)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(ggplot2)
library(reshape2)
library(wordcloud)

#Functions=====================================================================

GetXD <- function(XD,n,coll,tit,p = 50)
{
  uniXD <- XD %>% tokens_ngrams(n = n,concatenator = " ") %>% dfm() %>% textstat_frequency(n = p, groups = title) 
  uniplot <- uniXD %>% ggplot() + geom_col(aes(x = frequency, y = reorder_within(feature, frequency, group), 
  fill = group), show.legend = F, fill = coll) + facet_wrap(~group, scales = "free", nrow = 1) + scale_y_reordered() + ggtitle(tit) 
  return(uniplot)
}

Produce_plots <- function(XD2,wordd,nn)
{
  bigram.sep <- as_tibble(XD2) %>% separate(feature, c("word1", "word2"), " ") %>% filter(word2 == wordd)
  grimm_books2 <- dplyr::filter(grimm_books, grimm_books$word %in% bigram.sep$word1)
  pos_neg <- grimm_books2 %>%
    inner_join(nrc) %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    group_by(sentiment) %>%
    count(word, sort = T) %>%
    top_n(nn, n) %>%
    ungroup() %>%
    mutate(word = reorder(word, n))
  
  ggplot(pos_neg) + geom_col(aes(word, n, fill = sentiment)) + 
    coord_flip() + 
    facet_wrap( ~ sentiment, scales = "free") + ggtitle(wordd)
}

Get_wordcloud <- function(XD2,wordd,nn)
{
  bigram.sep <- as_tibble(XD2) %>% separate(feature, c("word1", "word2"), " ") %>% filter(word2 == wordd)
  grimm_books2 <- dplyr::filter(grimm_books, grimm_books$word %in% bigram.sep$word1)
  grimm_books2 %>%
    inner_join(nrc) %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    group_by(sentiment) %>%
    count(word, sort = T) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("red", "darkgreen"),max.words = nn)
}

Get_parts <- function(XD2,wordd)
{
  bigram.sep <- as_tibble(XD2) %>% separate(feature, c("word1", "word2"), " ") %>% filter(word2 == wordd)
  grimm_books2 <- dplyr::filter(grimm_books, grimm_books$word %in% bigram.sep$word1)
  grimm_books2 %>%
    left_join(parts_of_speech) %>%
    count(pos, sort = T) %>%
    mutate(nn = n / sum(n))
}

#Main==========================================================================

g <- gutenberg_works()
v <- gutenberg_download(2591)
books <- tibble(gutenberg_id = 2591, title = "Grimms' Fairy Tales")
v %<>% left_join(books) %>% mutate(gutenberg_id = NULL)
v <- v[c(-1:-88),]
df <- v %>% group_by(title) %>% summarise(text = paste(text, collapse = " ")) %>% corpus()
XD <- df %>% tokens(remove_punct = T) %>% tokens_remove(stopwords("english"))
grimm_books <- v %>% group_by(title) %>% mutate(linenumber = row_number()) %>% ungroup() %>% unnest_tokens(word, text)

#Unigram
uniplot <- GetXD(XD,1,"darkred","Unigram",10)
uniplot

#Bigram
biplot <- GetXD(XD,2,"darkgreen","Bigram",10)
biplot

#Plot
grid.arrange(uniplot,biplot, ncol=2, nrow=1)

#Sentiment and WordClouds
nrc <- get_sentiments("nrc")
XD2 <- XD %>% tokens_ngrams(concatenator = " ") %>% dfm() %>% textstat_frequency() 
Produce_plots(XD2, "said",30)
Get_wordcloud(XD2,"said",30)
Produce_plots(XD2, "came", 20)
Get_wordcloud(XD2,"came",30)
Produce_plots(XD2, "went", 30)
Get_wordcloud(XD2,"went",30)

#Parts_of_speech
Get_parts(XD2,"said")[c(1:5),]
Get_parts(XD2,"came")[c(1:5),]
Get_parts(XD2,"went")[c(1:5),]
