library(jsonlite)
library(tidytext)
library(geosphere, quietly = T, warn.conflicts = F)
library(dplyr)
library(ggmap)
library(magrittr)
library(lubridate)

Get_data <- function(i,delay,url,api,linbrig)
{
  if(i == 1) {cat("Warsaw Public Transport Data downloading... \n")} 
  cat("Step: ", i, "\n")
  Sys.sleep(delay)
  url.api <- paste(url, api,"&type=2&line=",linbrig[1],"&brigade=",linbrig[2], sep = "")
  trams <- fromJSON(url.api)
  trams <- data.frame(trams$result)
  return(trams)
}

Get_dist <- function(XX)
{
  D <- distm(XX %>% select(c(2,5)), fun = distHaversine) 
  dist <- sapply(1:nrow(XX),function(i) D[upper.tri(D)][i*(i+1)/2])
  return(dist[!is.na(dist)])
}

Get_tim <- function(XX,steps)
{
  res <- sapply(1:steps,function(i) XX %>% arrange(Nth_time) %>% summarise(res = Nth_time[i+1] - Nth_time[i])) %>% as.vector("numeric")
  return(res[!is.na(res)])
}

um.waw.api <- "9c9a80e9-68d1-4f74-8d49-c241f3f5649f"
um.waw.url <- "https://api.um.warszawa.pl/api/action/busestrams_get/?resource_id=f2e5503e-927d-4ad3-9500-4ab9e55deb59&apikey="

linbrig <- c(33,3) #Line and brigade number
delay <- 60
steps <- 35

data.trams <- do.call(rbind,lapply(1:steps,function(i) Get_data(i,delay,um.waw.url,um.waw.api,linbrig)))
data.trams$Lines <- as.numeric(gsub(" ", "", data.trams$Lines))
XX <- data.trams
start <- as_datetime(XX[1,'Time']) %>% seconds() %>% as.numeric()
XX <- XX %>% mutate(Nth_time = abs(as_datetime(Time) %>% seconds() %>% as.numeric() - start))

register_google(":)") #GMaps code
map <- get_googlemap(center = c(lon = 21.00, lat = 52.25), zoom = 12, maptype = "road", color = "bw", language = "PL")

ggmap(map) + geom_point(data = XX, aes(x = Lon, y = Lat, color = Nth_time), size = 3) + 
  scale_color_gradient2("Time", low = "green", high = "violet", mid = "red", midpoint = 1500) + 
  ggtitle(paste("Warsaw Public Transport - Line: ",linbrig[1],", brigade: ",linbrig[2],sep = ""))

s <- Get_dist(XX) # [s] = m
t <- Get_tim(XX,steps)

YY <- data.frame(1:length(s), s * 3.6 / t)
colnames(YY) <- c("Step","V")

ggplot(YY, aes(x = Step, y = V)) +
  xlab("Timestep") + ylab("v(Timestep) [km/h]") +
  geom_line(alpha=0.25, color = "red",linewidth = 0.75) +
  geom_point(color = "darkgreen",size = 3) +
  geom_smooth(method = "loess", span = 1) + ggtitle(paste("Warsaw Public Transport - Line: ",linbrig[1],", brigade: ",linbrig[2],sep = ""))


