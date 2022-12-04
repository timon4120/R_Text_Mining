library(gutenbergr)
library(dplyr)
library(tidytext)
library(magrittr)

#
# Kamil Orzechowski
#


make.lm <- function(data, threshold) 
{
  data %>% filter(M > 10 & M < threshold) %>%
    summarise(th = threshold, A = lm(log(V) ~ log(M))$coefficients[1], beta = lm(log(V) ~ log(M))$coefficients[2])
}

                       #Book's were selected manually from The Gutenberg webpage.
english_IDs = c(236,   #Jungle Book
                1524,  #Hamlet ~ W.Shakespeare
                1533,  #Makbet ~ W.Shakespeare
                2610,  #The Hunchback of Notre Dame
                11,    #Alice's Adventures in Wonderland
                730,   #Oliver Twist ~ C.Dickens
                46,    #A Christmas Carol in Prose ~ C.Dickens
                103,   #Around the World in Eighty Days ~ J.Verne
                19942, #Candide ~ Voltaire
                1513,  #Romeo and Juliet ~ W.Shakespeare
                36098) #The Flowers of Evil ~ C.Baudelaire

                       #French equivalents.
french_IDs = c(54183,
               15032,
               13868,
               19657,
               55456,
               16023,
               16021,
               800,
               4650,
               18143,
               6099)

g <- gutenberg_works()
english_books <- gutenberg_download(english_IDs)
french_books <- gutenberg_download(french_IDs)

english_titles <- g[g$gutenberg_id %in% english_IDs,c("gutenberg_id","title")]
french_titles <- arrange(tibble(english_IDs,french_IDs),english_IDs)
french_titles <- tibble(gutenberg_id = french_titles$french_IDs, title = english_titles$title)

english_final <- english_books %>% left_join(english_titles) %>% mutate(gutenberg_id = NULL)
french_final <- french_books %>% left_join(french_titles) %>% mutate(gutenberg_id = NULL)

english_words_dist <- english_final %>% unnest_tokens(word, text) %>% group_by(title)
french_words_dist <- french_final %>% unnest_tokens(word, text) %>% group_by(title)

english_heap <- english_words_dist %>% mutate(M = row_number(), V = cumsum(!duplicated(word)))
french_heap <- french_words_dist %>% mutate(M = row_number(), V = cumsum(!duplicated(word)))

english_permutation <- english_words_dist[sample(nrow(english_words_dist)),]
french_permutation <- french_words_dist[sample(nrow(french_words_dist)),]

english_perm_heap <- english_permutation %>% mutate(M = row_number(), V = cumsum(!duplicated(word)))
french_perm_heap <- french_permutation %>% mutate(M = row_number(), V = cumsum(!duplicated(word)))

english_summarize <- english_heap %>% summarise(a = lm(log10(V) ~ log10(M))$coefficients[1], beta = lm(log10(V) ~ log10(M))$coefficients[2])
french_summarize <- french_heap %>% summarise(a = lm(log10(V) ~ log10(M))$coefficients[1], beta = lm(log10(V) ~ log10(M))$coefficients[2])

english_permutation_summarize <- english_perm_heap %>%
  summarise(a = lm(log10(V) ~ log10(M))$coefficients[1], beta = lm(log10(V) ~ log10(M))$coefficients[2])
french_permutation_summarize <- french_perm_heap %>%
  summarise(a = lm(log10(V) ~ log10(M))$coefficients[1], beta = lm(log10(V) ~ log10(M))$coefficients[2])

Test <- cor.test(english_summarize$beta,french_summarize$beta)
print(paste0("Correlation: " ,Test$estimate)) #"Correlation: 0.744267956627863"
print(paste0("p-value: ", Test$p.value))      #"p-value: 0.00861736051630317"

Test_permutation <- cor.test(english_permutation_summarize$beta,french_permutation_summarize$beta)
print(paste0("Correlation: " ,Test_permutation$estimate))  #"Correlation: 0.826150240817724"
print(paste0("p-value: ", Test_permutation$p.value))       #"p-value: 0.00172361267751838"
