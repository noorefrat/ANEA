reference.feature <- wals$Passive.Constructions
reference.language.name <- wals$language
reference.glottocode <- wals$glottocode
reference.affiliation <- wals$affiliation
data.feature <- my_data$Passive.Constructions
data.affiliation <- my_data$affiliation


#The below functions do choose a subset from the reference features in order to get the 
#same distribution of family sizes as in the reference data set. So if we have three families in the
#data with 3, 2 and a single language, we want to have the same proportions of languages in the families
#in the reference dataset. So we have the take a subset of the reference data.

#We want to throw away as few data as possible, but we have to decide if we want to 
#throw away as few languages or families as possible. The function below does the former:

subsample_max_no_languages <- function(reference.feature, reference.affiliation, reference.language.name, data.affiliation){
  #preparation:
  reference.affiliation <- reference.affiliation[!is.na(reference.feature)]
  reference.language.name <- reference.language.name[!is.na(reference.feature)]
  reference.feature <- reference.feature[!is.na(reference.feature)]
  reference.feature <- reference.feature[!is.na(reference.affiliation)] #some affiliations in wals are unknown so:
  reference.language.name <- reference.language.name[!is.na(reference.affiliation)]
  reference.affiliation <- reference.affiliation[!is.na(reference.affiliation)]
  
  data.affiliation <- data.affiliation[!is.na(data.affiliation)]
  data.distribution <- sort(as.numeric(table(data.affiliation)), decreasing = T) #Get data distribution (no. of family members)
  reference.distribution <- sort(as.numeric(table(reference.affiliation)), decreasing = T)
  
  #actual calculation: take the n_data largest families in the reference data and divide through the data distribution
  n_data <- length(data.distribution)
  reference.sample.distribution <- (floor(min(reference.distribution[1:n_data]/data.distribution)) * data.distribution)
  
  #now one has to sample the numbers given by reference.sample.distribution from the corresponding families:
  family.sorted <- names(table(reference.affiliation))[match(sort(as.numeric(table(reference.affiliation)), decreasing = T), as.numeric(table(reference.affiliation)))][1:n_data]
  reference.language.sample <- c()
  for(u in 1:n_data){
    reference.language.sample <- c(reference.language.sample, sample(reference.language.name[reference.affiliation == family.sorted[u]], 
                                                                     size = reference.sample.distribution[u], replace = F))
  }
  reference.sample <- data.frame(reference.language.sample, reference.feature[match(reference.language.sample, reference.language.name)])
  return(reference.feature.sample)
}

#This function adopts the distribution of families and keeps the maximal number of families as possible:

subsample_max_no_families <- function(reference.feature, reference.affiliation, reference.language.na data.affiliation){
  #preparation:
  reference.affiliation <- reference.affiliation[!is.na(reference.feature)]
  reference.language.name <- reference.language.name[!is.na(reference.feature)]
  reference.feature <- reference.feature[!is.na(reference.feature)]
  reference.feature <- reference.feature[!is.na(reference.affiliation)] #some affiliations in wals are unknown so <-
  reference.language.name <- reference.language.name[!is.na(reference.affiliation)]
  reference.affiliation <- reference.affiliation[!is.na(reference.affiliation)]
  
  data.feature <- data.feature[which(!is.na(data.feature))]
  data.affiliation <- data.affiliation[which(!is.na(data.affiliation))]
  data.distribution <- sort(as.numeric(table(data.affiliation)), decreasing = T) #Get data distribution (no. of family members)
  
  reference.family.distribution.size <- as.numeric(names(table(as.numeric(table(reference.affiliation))))) #Get family sizes
  reference.family.distribution <- as.numeric(table(as.numeric(table(reference.affiliation)))) #Get family size distribution
  
  #Instead of taking the maximal number of languages, we aim to take the maximal number of families.
  #Hence we have to rearrange the data in family size: e.g. if we have two families in the data with one language,
  #we have two families with size one. 
  # For example, if we'd have: 2 with 1 language, 1 with two language and two with four languages,
  # we have to prepare the reference family sizes such that we can see that e.g. we have ten with one 
  # language, thirty with *at least* two languages and twenty with *at least* four languages.
  
  data.family.distribution.size <- as.numeric(names(table(as.numeric(table(data.affiliation))))) 
  data.family.distribution <- as.numeric(table(as.numeric(table(data.affiliation)))) 
  
  reference.family.distribution.capacity <- c()
  reference.capacity.names <- data.frame(name = 'test', capacity = 0) #just an empty first row
  for(u in 1:length(data.family.distribution)){
    if(u == length(data.family.distribution)){
      temp <- sum(reference.family.distribution[which(reference.family.distribution.size >= data.family.distribution.size[u])])
      temp.name <- names(which(table(reference.affiliation) >= data.family.distribution.size[u]))
    } else {
      temp <- sum(reference.family.distribution[which(reference.family.distribution.size >= data.family.distribution.size[u] & 
                                                        reference.family.distribution.size < data.family.distribution.size[u + 1])])
      temp.name <- names(which(table(reference.affiliation) >= data.family.distribution.size[u] & 
                                 table(reference.affiliation) < data.family.distribution.size[u + 1]))
    }
    
    reference.family.distribution.capacity <- c(reference.family.distribution.capacity, temp)
    reference.capacity.names = rbind(reference.capacity.names, data.frame(name = temp.name, capacity = data.family.distribution.size[u]))
  }
  reference.sample.family.distribution <-data.frame(family.samples =  (floor(min(reference.family.distribution.capacity/data.family.distribution)) * data.family.distribution),
                                                    family.size = data.family.distribution.size)
  
  #Take the distribution adjusted reference sample:
  reference.language.sample <- c()
  for(u in 1:nrow(reference.sample.family.distribution)){
    #First step: sample languages
    family.names.tobesampled <- sample(reference.capacity.names$name[which(reference.capacity.names$capacity == reference.sample.family.distribution$family.size[u])], 
                                       reference.sample.family.distribution$family.samples, replace = FALSE)
    #Second step: sample features from those languages
    for(k in family.names.tobesampled){
      temp.language.sample <- sample(reference.language.name[which(reference.affiliation == k)], reference.sample.family.distribution$family.size[u], replace = FALSE)
      reference.language.sample <- c(reference.language.sample, temp.language.sample)
    }
  }
  reference.sample <- data.frame(reference.language.sample, reference.feature[match(reference.language.sample, reference.language.name)])
  return(reference.sample)
}

subsample_max_no_families(wals$Passive.Constructions, reference.affiliation = wals$affiliation, data.affiliation = my_data$affiliation)
ref.sample <- subsample_max_no_languages(wals$Passive.Constructions, reference.affiliation = wals$affiliation, data.affiliation = my_data$affiliation)
dat <- my_data$Passive.Constructions[!is.na(my_data$Passive.Constructions)]


get_agreement <- function(reference.feature, reference.affiliation, data.affiliation, data.feature){
  data.feature <- data.feature[!is.na(data.feature)]
  reference.feature.sample <- subsample_max_no_families(wals$Passive.Constructions, reference.affiliation = wals$affiliation, data.affiliation = my_data$affiliation)
  
  for(u in 1:length(data.feature)){
    
  }
}
