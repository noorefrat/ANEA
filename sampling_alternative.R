#The sampling algorithm provides a sample of languages from a reference dataset
#with a given family distribution and desired properties. If it is possible
#to attain a sample with the desired distribution, the algorithm continues
#to sample until the reference dataset is exhausted. If it is not possible 
#then it will provide suboptimal solutions, aiming to give approximately
#the same distribution.

#There are two options:

#setting 'sampling_method' = indifferent 
#will randomly pick a set of languages and make sure that it fits the distribution
#as provided by a vector with families called 'distribution'. If it was not possible 
#to pick a set according to the distribution (as a result of few reference languages),
#it will try to give a suboptimal solution (yet to be defined).

#'sampling_method' = proximity
#will try to find a set of languages that match the distribution in one area only.
#(And otherwise pick a suboptimal set)

#Additionally, we can set the initial sampling procedure:

#'sampling_unit' = family
#does sample a family from the reference set and then continues to look
#for other families in the area/all areas

#'sampling_unit' = language
#does so with languages. The rationale is that if we sample from families,
#we avoid ending up with sampling lots of times from large families.



distribution <- my_data$affiliation[!is.na(my_data$affiliation)]
feature <- reference$NPAgrCat
affiliation <- reference$affiliation
area <- reference$area

df <- drop_na(data.frame(feature, affiliation, area))

distr.count <- table(distribution)
ref.count <- table(df$affiliation)
family.sample <- c()
for(u in 1:length(distr.count)){
  
  family.sample <- c(family.sample, sample(
}


