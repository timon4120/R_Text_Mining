library(gutenbergr)
library(dplyr)
library(tidyr)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(ggplot2)
library(igraph)
library(ggraph)

g <- gutenberg_works()
v <- gutenberg_download(c(8119, 27081))

books <- tibble(gutenberg_id = c(8119, 27081), title = c("Sklepy cynamonowe", "Sonety Krymskie"))
v %<>% left_join(books) %>% mutate(gutenberg_id = NULL)
v <-v [c(-1:-15,-5150:-5867),]
df <- v %>% group_by(title) %>% summarise(text = paste(text, collapse = " ")) %>% corpus()

stops <- read.table("http://www.if.pw.edu.pl/~julas/TEXT/lab/stopw.dat", stringsAsFactors = F)
stops <- unlist(lapply(stops,as.character))
names(stops) <- NULL

XD <- df %>% tokens(remove_punct = T) %>% tokens_remove(stops) %>%
  tokens_ngrams(concatenator = " ") %>% dfm() %>%
  textstat_frequency(n = 20, groups = title)

XD %>% ggplot() + 
  geom_col(aes(x = frequency, y = reorder_within(feature, frequency, group), fill = group), show.legend = F) + 
  facet_wrap(~group, scales = "free", nrow = 2) + scale_y_reordered()

#==============================================

bigram.sep.shops <- as_tibble(head(XD,20)) %>% separate(feature, c("word1", "word2"), " ")
bigram.sep.crimea <- as_tibble(tail(XD,20)) %>% separate(feature, c("word1", "word2"), " ")

graph.bi.shops <- bigram.sep.shops %>% select(!(rank:group)) %>% graph_from_data_frame()
graph.bi.crimea <- bigram.sep.crimea %>% select(!(rank:group)) %>% graph_from_data_frame()

bi.cut.g <- graph.bi.shops - E(graph.bi.shops)[E(graph.bi.shops)$frequency < 2]
bi.cut.g <- bi.cut.g - V(bi.cut.g)[degree(bi.cut.g) < 1]

ggraph(bi.cut.g, layout = "nicely") + 
  geom_edge_link(aes(width = frequency), color = "darkblue") + 
  scale_edge_width_continuous(range = c(0.5,2)) +
  geom_node_point(size = 3, color = "orange") +
  geom_node_text(aes(label = name), size = 3, nudge_y = 0.5)  + ggtitle("Sklepy cynamonowe")

#bi.cut.g2 <- graph.bi.crimea - E(graph.bi.crimea)[E(graph.bi.crimea)$frequency < 5]
bi.cut.g2 <- graph.bi.crimea - V(graph.bi.crimea)[degree(graph.bi.crimea) < 1]

ggraph(bi.cut.g2, layout = "nicely") + 
  geom_edge_link(aes(width = frequency), color = "darkred") + 
  scale_edge_width_continuous(range = c(0.5,2)) +
  geom_node_point(size = 3, color = "orange") +
  geom_node_text(aes(label = name), size = 3, nudge_y = 0.5) + ggtitle("Sonety Krymskie")
