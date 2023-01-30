library(stringr)
library(gsubfn)
library(tidyverse)
library(tidytext)
library(rstudioapi)
library(tm)
library(reshape2)
library(ggwordcloud)

#
# Kamil Orzechowski & Kordian Makulski
#

#Folders - the program determines the folder in which the program is located
#workdirectory + creates a folder for sentiment unigrams for each person
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Dirs <- function(HiT)
{
  print(paste(dirname(rstudioapi::getActiveDocumentContext()$path),"/",HiT, sep = ""))
  dir.create(paste(dirname(rstudioapi::getActiveDocumentContext()$path),"/",HiT, sep = ""))
}

#Properly formatted .TXT files are retrieved and a tibble with dates and speeches is returned. 
#The last line of the tibble is the combined text from all the person's speeches.
Make_It_Possible <- function(filepath)
{
    filee <- gsub(" ","",paste(filepath,".txt"))
    z <- read.delim(filee,quote = "") %>% str_split(pattern = "---New Data---")
    times <- sapply(1:length(z[[1]]), function(i) strapplyc(z[[1]][i], "[{](.*)[}]", simplify = TRUE))
    X <- sapply(1:length(z[[1]]), function(i) str_remove_all(z[[1]][i], "[{](.*)[}]")) %>% as.list()
    XD <- sapply(1:length(X), function(i) gsub('[[:punct:] ]+',' ',gsub('[\n\t]+','',X[i])))
    data <- tibble(doc_id = times, text = XD)
    
    zz <- paste(z, collapse = '')
    zz <- str_remove_all(zz, "[{](.*)[}]")
    XDD <- gsub('[[:punct:] ]+',' ',gsub('[\n\t]+','',zz))
    dataa <- tibble(doc_id = "All", text = XDD)

    fin_data <- rbind(data,dataa)
    return(fin_data)
}

#Unigrams for each speech are generated in the previously created folders.
Get_Sentiments_Plots <- function(XStruct,Nth_speech,NN,HiT)
{
  nrc <- get_sentiments("nrc")
  
  XMan <- XStruct[Nth_speech,] %>%
    group_by(doc_id) %>%
    mutate(linenumber = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word, text)
  
  nrc_class <- XMan %>% inner_join(nrc) %>%
    filter(!(sentiment %in% c("positive", "negative"))) %>%
    group_by(sentiment) %>%
    count(word, sort = T) %>%
    top_n(NN, n) %>%
    ungroup() %>%
    mutate(word = reorder(word, n))
  
  ggplot(nrc_class) + geom_col(aes(word, n, fill = sentiment), show.legend = FALSE) + 
    coord_flip() + facet_wrap(~sentiment, nrow = 3, scales = "free") + 
    ggtitle(paste(HiT,"- speech of",XStruct[Nth_speech,1]))
  
  ggsave(gsub(" ","",paste(HiT,"/",HiT,"_",XStruct[Nth_speech,1],".png")), width = 16, height = 12)
  
  return(nrc_class)
}

#Calculation of the alignment of a given speech on a scale of -3:3
Get_Sentiment_Alignment <- function(nrc_class)
{
  tmp <-  nrc_class %>%
    group_by(sentiment) %>%
    count(wt = n) %>%
    ungroup()
  t<-tmp$n[tmp$sentiment=="trust"]
  f<-tmp$n[tmp$sentiment=="fear"]
  a<-tmp$n[tmp$sentiment=="anticipation"]
  s<-tmp$n[tmp$sentiment=="sadness"]
  j<-tmp$n[tmp$sentiment=="joy"]
  ag<-tmp$n[tmp$sentiment=="anger"]
  if (length(t) == 0) t = 0
  if (length(f) == 0) f = 0
  if (length(a) == 0) a = 0
  if (length(s) == 0) s = 0
  if (length(j) == 0) j = 0
  if (length(ag) == 0) ag = 0
  return(sign(t - f) +
    sign(a - s) +
    sign(j - ag))
}

#Generation of alignment charts + wordclouds
Count_Time_Changes <- function(nrc_classes, overall_nrc_class, who, HiT)
{
  tmp <- tibble(Value = sapply(1:(length(who[[1]])-1), function(i) Get_Sentiment_Alignment(as_tibble(nrc_classes[,i]))),
               Date = as.Date(who$doc_id[1:(length(who[[1]])-1)], "%d.%m.%Y"),
               Type = "alignment")
  
  meanv <- tibble(Value = mean(tmp$Value), Date = c(min(tmp$Date), max(tmp$Date)), Type = "mean alignment")
  meano <- tibble(Value = Get_Sentiment_Alignment(overall_nrc_class), Date = c(min(tmp$Date), max(tmp$Date)), Type = "overall alignment")
  ggplot() +
    geom_point(data = tmp, aes(x = Date, y = Value, color = Type), size = 4) + 
    geom_line(data = meanv, aes(x = Date, y = Value, color = Type), size = 1.5) +
    geom_line(data = meano, aes(x = Date, y = Value, color = Type), size = 1.5) +
    geom_line(data = tmp, aes(x = Date, y = Value, color = Type), size = 1.5) +
    theme(axis.title = element_text(size = 24), legend.title = element_text(size = 24), legend.text = element_text(size = 18))
  ggsave(gsub(" ","",paste(HiT,"/",HiT,"_alignments.png")), width = 16, height = 12)
  
  ggplot(overall_nrc_class, aes(label = word, size = n, color = sentiment, angle_group = sentiment)) +
    geom_text_wordcloud() +
    scale_size_area(max_size = 20)
  ggsave(gsub(" ","",paste(HiT,"/",HiT,"_wordcloud.png")), width = 16, height = 12)
}

#=============================================================================
#This is where the actual program starts.
#=============================================================================

#People and create their folders
Leaders <- c("J_Tito","A_Hitler","J_Stalin","B_Mussolini","F_Marcos","F_Castro","M_Zedong",
             "Franklin_Delano_Roosevelt", "Harry_S._Truman", "Lyndon_Baines_Johnson", "Margaret_Hilda_Thatcher", "Richard_Milhous_Nixon", "Ronald_Wilson_Reagan", "Winston_Churchill")
sapply(1:length(Leaders), function(i) Dirs(Leaders[i]))

#Tibble for every leader
J_Tito <- Make_It_Possible(Leaders[1])
A_Hitler <- Make_It_Possible(Leaders[2])
J_Stalin <- Make_It_Possible(Leaders[3])
B_Mussolini <- Make_It_Possible(Leaders[4])
F_Marcos <- Make_It_Possible(Leaders[5])
F_Castro <- Make_It_Possible(Leaders[6])
M_Zedong <- Make_It_Possible(Leaders[7])
F_Roosevelt <- Make_It_Possible(Leaders[8])
H_Truman <- Make_It_Possible(Leaders[9])
L_Johnson <- Make_It_Possible(Leaders[10])
M_Thatcher <- Make_It_Possible(Leaders[11])
R_Nixon <- Make_It_Possible(Leaders[12])
R_Reagan <- Make_It_Possible(Leaders[13])
W_Churchill <- Make_It_Possible(Leaders[14])

#The number of words in the unigram
nmb = 10

#Sentiment analysis - unigrams of emotions - each speech separately
J_Tito_Sentiments_Time <- sapply(1:(length(J_Tito[[1]])-1), function(i) Get_Sentiments_Plots(J_Tito,i,nmb,Leaders[1]))
A_Hitler_Sentiments_Time <- sapply(1:(length(A_Hitler[[1]])-1), function(i) Get_Sentiments_Plots(A_Hitler,i,nmb,Leaders[2]))
J_Stalin_Sentiments_Time <- sapply(1:(length(J_Stalin[[1]])-1), function(i) Get_Sentiments_Plots(J_Stalin,i,nmb,Leaders[3]))
B_Mussolini_Sentiments_Time <- sapply(1:(length(B_Mussolini[[1]])-1), function(i) Get_Sentiments_Plots(B_Mussolini,i,nmb,Leaders[4]))
F_Marcos_Sentiments_Time <- sapply(1:(length(F_Marcos[[1]])-1), function(i) Get_Sentiments_Plots(F_Marcos,i,nmb,Leaders[5]))
F_Castro_Sentiments_Time <- sapply(1:(length(F_Castro[[1]])-1), function(i) Get_Sentiments_Plots(F_Castro,i,nmb,Leaders[6]))
M_Zedong_Sentiments_Time <- sapply(1:(length(M_Zedong[[1]])-1), function(i) Get_Sentiments_Plots(M_Zedong,i,nmb,Leaders[7]))
F_Roosevelt_Sentiments_Time <- sapply(1:(length(F_Roosevelt[[1]])-1), function(i) Get_Sentiments_Plots(F_Roosevelt,i,nmb,Leaders[8]))
H_Truman_Sentiments_Time <- sapply(1:(length(H_Truman[[1]])-1), function(i) Get_Sentiments_Plots(H_Truman,i,nmb,Leaders[9]))
L_Johnson_Sentiments_Time <- sapply(1:(length(L_Johnson[[1]])-1), function(i) Get_Sentiments_Plots(L_Johnson,i,nmb,Leaders[10]))
M_Thatcher_Sentiments_Time <- sapply(1:(length(M_Thatcher[[1]])-1), function(i) Get_Sentiments_Plots(M_Thatcher,i,nmb,Leaders[11]))
R_Nixon_Sentiments_Time <- sapply(1:(length(R_Nixon[[1]])-1), function(i) Get_Sentiments_Plots(R_Nixon,i,nmb,Leaders[12]))
R_Reagan_Sentiments_Time <- sapply(1:(length(R_Reagan[[1]])-1), function(i) Get_Sentiments_Plots(R_Reagan,i,nmb,Leaders[13]))
W_Churchill_Sentiments_Time <- sapply(1:(length(W_Churchill[[1]])-1), function(i) Get_Sentiments_Plots(W_Churchill,i,nmb,Leaders[14]))

#Sentiment analysis - unigrams of emotions - all speeches combined
J_Tito_Sentiments <- Get_Sentiments_Plots(J_Tito,length(J_Tito[[1]]),nmb,Leaders[1])
A_Hitler_Sentiments <- Get_Sentiments_Plots(A_Hitler,length(A_Hitler[[1]]),nmb,Leaders[2])
J_Stalin_Sentiments <- Get_Sentiments_Plots(J_Stalin,length(J_Stalin[[1]]),nmb,Leaders[3])
B_Mussolini_Sentiments <- Get_Sentiments_Plots(B_Mussolini,length(B_Mussolini[[1]]),nmb,Leaders[4])
F_Marcos_Sentiments <- Get_Sentiments_Plots(F_Marcos,length(F_Marcos[[1]]),nmb,Leaders[5])
F_Castro_Sentiments <- Get_Sentiments_Plots(F_Castro,length(F_Castro[[1]]),nmb,Leaders[6])
M_Zedong_Sentiments <- Get_Sentiments_Plots(M_Zedong,length(M_Zedong[[1]]),nmb,Leaders[7])
F_Roosevelt_Sentiments <- Get_Sentiments_Plots(F_Roosevelt,length(F_Roosevelt[[1]]),nmb,Leaders[8])
H_Truman_Sentiments <- Get_Sentiments_Plots(H_Truman,length(H_Truman[[1]]),nmb,Leaders[9])
L_Johnson_Sentiments <- Get_Sentiments_Plots(L_Johnson,length(L_Johnson[[1]]),nmb,Leaders[10])
M_Thatcher_Sentiments <- Get_Sentiments_Plots(M_Thatcher,length(M_Thatcher[[1]]),nmb,Leaders[11])
R_Nixon_Sentiments <- Get_Sentiments_Plots(R_Nixon,length(R_Nixon[[1]]),nmb,Leaders[12])
R_Reagan_Sentiments <- Get_Sentiments_Plots(R_Reagan,length(R_Reagan[[1]]),nmb,Leaders[13])
W_Churchill_Sentiments <- Get_Sentiments_Plots(W_Churchill,length(W_Churchill[[1]]),nmb,Leaders[14])

#sentiment pairs: trust - fear; anticipation - sadness, joy - anger
Count_Time_Changes(J_Tito_Sentiments_Time, J_Tito_Sentiments, J_Tito, Leaders[1])
Count_Time_Changes(A_Hitler_Sentiments_Time, A_Hitler_Sentiments, A_Hitler, Leaders[2])
Count_Time_Changes(J_Stalin_Sentiments_Time, J_Stalin_Sentiments, J_Stalin, Leaders[3])
Count_Time_Changes(B_Mussolini_Sentiments_Time, B_Mussolini_Sentiments, B_Mussolini, Leaders[4])
Count_Time_Changes(F_Marcos_Sentiments_Time, F_Marcos_Sentiments, F_Marcos, Leaders[5])
Count_Time_Changes(F_Castro_Sentiments_Time, F_Castro_Sentiments, F_Castro, Leaders[6])
Count_Time_Changes(M_Zedong_Sentiments_Time, M_Zedong_Sentiments, M_Zedong, Leaders[7])
Count_Time_Changes(F_Roosevelt_Sentiments_Time, F_Roosevelt_Sentiments, F_Roosevelt, Leaders[8])
Count_Time_Changes(H_Truman_Sentiments_Time, H_Truman_Sentiments, H_Truman, Leaders[9])
Count_Time_Changes(L_Johnson_Sentiments_Time, L_Johnson_Sentiments, L_Johnson, Leaders[10])
Count_Time_Changes(M_Thatcher_Sentiments_Time, M_Thatcher_Sentiments, M_Thatcher, Leaders[11])
Count_Time_Changes(R_Nixon_Sentiments_Time, R_Nixon_Sentiments, R_Nixon, Leaders[12])
Count_Time_Changes(R_Reagan_Sentiments_Time, R_Reagan_Sentiments, R_Reagan, Leaders[13])
Count_Time_Changes(W_Churchill_Sentiments_Time, W_Churchill_Sentiments, W_Churchill, Leaders[14])
