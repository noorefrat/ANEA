library(vegan)

ref.data<- read.csv("C:/Users/NourEfrat-Kowalsky/Desktop/code/pRDA/reference.data.csv", encoding="UTF-8")
anea.data<- read.csv("C:/Users/NourEfrat-Kowalsky/Desktop/code/pRDA/data.csv", encoding="UTF-8")
  
test.set<- rbind(anea.data, ref.data)
names(test.set)[1]<-paste("language")

test.imp <- imputeMCA(test.set[,-c(1:6)])
dim.red <- MCA(test.set[,-c(1:6)], tab.disj = test.imp$tab.disj)

for(i in 1:ncol(dim.red$ind$coord)){
  dim.red$ind$coord[,i] <- scale(dim.red$ind$coord[,i])*dim.red$eig[i,'percentage of variance']}
mca.dist <- gower.dist(dim.red$ind$coord)
area.dist <- gower.dist(test.set$area)
affiliation.dist <- gower.dist(test.set$affiliation)

dbrda(mca.dist ~ area.dist + Condition(affiliation.dist))

mod= capscale(mca.dist ~ area.dist + Condition(affiliation.dist))
plot(mod)
anova(mod)
anova(mod, by="terms", permu=200) 
