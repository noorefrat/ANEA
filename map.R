library(lingtypology)
library(dplyr)
library(htmlwidgets)

test.set <- read.csv("C:/Users/NourEfrat-Kowalsky/Desktop/code/pRDA/test.set.csv", encoding="UTF-8")
test.set$latitude<- as.numeric(test.set$latitude)
test.set$longitude<-as.numeric(test.set$longitude)
test.set
names<- test.set[,7:ncol(test.set)]
names

for(n in colnames(names)){
  
  print(n)
  
  mystring <- sprintf("C:\\Users/NourEfrat-Kowalsky/Desktop/code/pRDA/Maps/%s.html", n)
  
  n_map<- map.feature(test.set $language, 
                      longitude = test.set $longitude, 
                      latitude = test.set $latitude, 
                      features = test.set [[n]], 
                      label = test.set $language, 
                      color = c("#66c2a5",
                                "#fc8d62",
                                "#8da0cb",
                                "#e78ac3",
                                "#a6d854",
                                "#ffd92f",
                                "#e5c494"))
  #print(n_map)
  saveWidget(n_map, file= mystring)
}

