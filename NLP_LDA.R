library(gutenbergr)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(tm)
library(lsa)
library(topicmodels)
library(topicdoc)

j_verne_ids <- c(
	164, #Twenty Thousand Leagues under the Sea
	103, #Around the World in Eighty Days
	18857) #A Journey to the Centre of the Earth 

j_austen_ids <- c(
	1342, #Pride and Prejudice
	158,  #Emma
	161)  #Sense and Sensibility

all_ids <- c(j_verne_ids,j_austen_ids)
g <- gutenberg_works()

Get_terms <- function(IDs,knumb,title)
{
  v <- gutenberg_download(IDs)
  removeSpecialChars <- function(x) gsub("[^0-9A-Za-z///' ]"," ",x)
  df <- v %>% group_by(gutenberg_id) %>% summarise(text = paste(text, collapse = " "))
  colnames(df) <- c("doc_id","text")
  corpus <- VCorpus(DataframeSource(as.data.frame(df)))
  corpus <- corpus %>% tm_map(removeWords, stopwords("english")) %>% 
    tm_map(removePunctuation) %>% tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(removeSpecialChars)) %>% tm_map(stripWhitespace) %>%
    tm_map(stemDocument)
  
  doc <- DocumentTermMatrix(corpus)
  
  ap_lda <- LDA(doc, k = knumb, control = list(seed = 1234))
  ap_topics <- tidy(ap_lda, matrix = "beta")
  
  print(paste("Coherence:",title))
  print(topic_diagnostics(ap_lda, doc)[7])
  
  ap_top_terms <- ap_topics %>% group_by(topic) %>% top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  ap_top_terms %>% mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") + scale_x_reordered() + coord_flip() + ggtitle(title)
}

Get_terms(j_verne_ids,2,"J.Verne books")

Get_terms(j_austen_ids,2,"J.Austen books")

Get_terms(all_ids,2,"Both authors books")

Get_terms(all_ids,3,"Both authors books - 3 terms")




