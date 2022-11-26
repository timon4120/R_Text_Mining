library(tm)
library(ggplot2)
library(dplyr)
library(gridExtra)
 

#50 samples from Reuters (tm package)
data("acq")

tdm <- TermDocumentMatrix(acq, control = list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE))
tdm <- as.matrix(tdm)

tdm2 <- TermDocumentMatrix(acq, control = list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE,weighting = weightTfIdf))
tdm2 <- as.matrix(tdm2)

#Cosine similarity for TFs & (TF-IDF)
cosin <- function(df,n) 
{
  f2 <- function(x) sqrt(sum(x^2))
  return((df[,n] %*% df / (f2(df[,n]) * apply(df, 2, f2))) %>% round(3))
}

cosins <- sapply(1:50, function(x) (cosin(tdm,x)))
cosins2 <- sapply(1:50, function(x) (cosin(tdm2,x)))

#Product for TFs & (TF-IDF)
tfs <- sapply(1:50, function(n) (tdm[,n] %*% tdm))
tfs2 <- sapply(1:50, function(n) (tdm2[,n] %*% tdm2))

#Plots
XD1 <- ggplot() + aes(as.vector(tfs[upper.tri(tfs)])) + geom_histogram(binwidth=1, colour="black", fill="darkred") + ggtitle("TF") + xlab("TFs product") + ylab("Counts of words")
XD2 <- ggplot() + aes(as.vector(tfs2[upper.tri(tfs2)])) + geom_histogram(binwidth=0.01, colour="black", fill="darkred") + ggtitle("TF-IDF") + xlab("TF-IDFs product") + ylab("Counts of words")
XD3 <- ggplot() + aes(as.vector(cosins[upper.tri(cosins)])) + geom_histogram(binwidth=0.01, colour="black", fill="darkgreen") +ggtitle("Commons TF")+ xlab("Cosine similarity = TF") + ylab("Counts of words")
XD4 <- ggplot() + aes(as.vector(cosins2[upper.tri(cosins2)])) + geom_histogram(binwidth=0.01, colour="black", fill="purple") +ggtitle("Commons TF-IDF")+ xlab("Cosine similarity = TF-IDF") + ylab("Counts of words")
grid.arrange(XD1,XD2,XD3,XD4, ncol=2, nrow=2)

#Pairs of text from product TF
idxx2_0 <- which(tfs==min(tfs),arr.ind=T)[1:2][1]
idxx2_1 <- which(tfs==min(tfs),arr.ind=T)[1:2][2]
acq[[idxx2_0]]$meta$heading
acq[[idxx2_1]]$meta$heading

#Pairs of text from product TF-IDF
idxx2_02 <- which(tfs2==min(tfs2),arr.ind=T)[1:2][1]
idxx2_12 <- which(tfs2==min(tfs2),arr.ind=T)[1:2][2]
acq[[idxx2_02]]$meta$heading
acq[[idxx2_12]]$meta$heading


#Pairs of text from cosine sim.TF
idxx <- which(cosins==min(cosins),arr.ind=T)[1:2][1]
idxxx <- which(cosins==min(cosins),arr.ind=T)[1:2][2]
acq[[idxx]]$meta$heading
acq[[idxxx]]$meta$heading

#Pairs of text from cosine sim.TF-IDF
idxx2 <- which(cosins2==min(cosins2),arr.ind=T)[1:2][1]
idxxx2 <- which(cosins2==min(cosins2),arr.ind=T)[1:2][2]
acq[[idxx2]]$meta$heading
acq[[idxxx2]]$meta$heading
