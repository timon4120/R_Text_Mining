library(gutenbergr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidytext)
library(gridExtra)

#20k info
bk <- gutenberg_works(languages = "en") %>% filter(author == "Verne, Jules",title == "Twenty Thousand Leagues under the Sea")

#Word frequency counter creation
book <- collect(select(bk, 'gutenberg_id'))[[1]] %>% gutenberg_download() %>% unnest_tokens(word,text) %>% 
  count(word, sort = TRUE) %>%
  mutate(word1 = reorder(word, n))

#Lack of conjunctions, function words etc. 
pruned_book <- book %>% anti_join(stop_words)

#Ulysses info
bk2 <- gutenberg_works(languages = "en") %>% filter(author == "Joyce, James",title == "Ulysses")

#Word frequency counter creation
book2 <- collect(select(bk2, 'gutenberg_id'))[[1]] %>% gutenberg_download() %>% unnest_tokens(word,text) %>% 
  count(word, sort = TRUE) %>%
  mutate(word1 = reorder(word, n))

#Lack of conjunctions, function words etc.
pruned_book2 <- book2 %>% anti_join(stop_words)

showme <- function(N,colorss)
{
  XD1 <- book %>% head(N) %>% ggplot() + geom_col(aes(word1, n),fill=colorss[1]) + coord_flip() + ggtitle("20 000 ~ Verne") +
    ylab("Number of words") + xlab("Words")
  XD2 <- pruned_book %>% head(N) %>% ggplot() + geom_col(aes(word1, n),fill=colorss[2]) + coord_flip() + ggtitle(" Pruned 20 000 ~ Verne") +
    ylab("Number of words") + xlab("Words")
  
  YD1 <- book2 %>% head(N) %>% ggplot() + geom_col(aes(word1, n),fill=colorss[3]) + coord_flip() + ggtitle("Ulysses ~ Joyce") +
    ylab("Number of words") + xlab("Words")
  YD2 <- pruned_book2 %>% head(N) %>% ggplot() + geom_col(aes(word1, n),fill=colorss[4]) + coord_flip() + ggtitle("Pruned Ulysses ~ Joyce") +
    ylab("Number of words") + xlab("Words")
  return(grid.arrange(XD1,XD2,YD1,YD2, ncol=2))
}

plot <- showme(5,c("darkred","darkgreen","purple","darkblue"))


