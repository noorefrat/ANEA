#' ---
#' title: "*Arial Language Similarity Dependence*^[This document was `rmarkdown::render`'ed from an `R` script available at \textcolor{red}{ADD OSF URL}]"
#' author: "Anonymous"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output:
#'  pdf_document:
#'   fig_crop: true
#'   fig_caption: true
#'   latex_engine: xelatex
#'   df_print: kable
#'   toc: true
#'   toc_depth: 4
#'   number_section: true
#'   pandoc_args:
#'     - "--variable=lof"
#'     - "--variable=lot"
#'     - "--bibliography=references.bib"
#'     - "--csl=numeric.csl"
#' header-includes: \usepackage[width=\textwidth]{caption}
#' ---
#' 
#' 
#' 
#' Introduction
#' ==================================
#' 
#' We aim to investigate similarities between spatially connected languages in the ancient middle east.
#' To find out if similarity is beyond average, we compare it to similarity in apart languages.
#' 
#data_reading
my_data <- read.csv('~/ANEA/features.csv') #Read middle eastern ancient languages features


feature_names<-  c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n",
                   "Systems.of.Gender.Assignment",	"Nominal.Plurality",	"Definite.Articles",
                   "Indefinite.Articles",	"Distance.Contrasts.in.Demonstratives",	
                   "Pronominal.and.Adnominal.Demonstratives",	"Third.Person.Pronouns.and.Demonstratives",	
                   "Gender.Distinctions.in.Independent.Personal.Pronouns",	"Indefinite.Pronouns",	
                   "Intensifiers.and.Reflexive.Pronouns", "Person.Marking.on.Adpositions",	
                   "Comitatives.and.Instrumentals",	"Ordinal.Numerals",	"Distributive.Numerals",
                   "Position.of.Pronominal.Possessive.Affixes",	"Possessive.Classification",	
                   "Noun.Phrase.Conjunction",	"Nominal.and.Verbal.Conjunction",	
                   "Perfective.Imperfective.Aspect",	"The.Past.Tense",	"The.Future.Tense",	
                   "The.Morphological.Imperative",	"The.Prohibitive",	"Optative",	"WordOrderSOV",
                   "Order.of.Object..Oblique..and.Verb",	"Order.of.Adposition.and.Noun.Phrase",	
                   "Order.of.Genitive.and.Noun",	"Order.of.Adjective.and.Noun",	"Order.of.Demonstrative.and.Noun",	
                   "Order.of.Numeral.and.Noun",	"Order.of.Relative.Clause.and.Noun",	"Order.of.Degree.Word.and.Adjective",	
                   "Position.of.Polar.Question.Particles",	"Position.of.Interrogative.Phrases.in.Content.Questions",	
                   "Alignment.of.Case.Marking.of.Full.Noun.Phrases",	"Alignment.of.Case.Marking.of.Pronouns",	
                   "Expression.of.Pronominal.Subjects",	"Verbal.Person.Marking",	"Reciprocal.Constructions",	
                   "Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb", 
                   "NPMarking", "NPHeadlessness", "ClausePosition", "NumClass.n")

autotyp <- autotyp.feature(c(c("NP_structure","Clause_linkage", "Numeral_classifiers", "Register"))) 
colnames(autotyp)[which(colnames(autotyp) == 'Glottocode')] <- 'glottocode'
colnames(autotyp)[which(colnames(autotyp) == 'Area')] <- 'area'
colnames(autotyp)[which(colnames(autotyp) == 'MajorBranch')] <- 'affiliation'
autotyp$datasource <- factor('autotyp')


# np<- autotyp.feature(c("NP_structure") #Read features from autotyp
# clause<- autotyp.feature("Clause_linkage")
# num <- autotyp.feature("Numeral_classifiers")
# areas<- autotyp.feature("Register")
# 
# NPs<- select(np, Glottocode,NPMarking, NPHeadlessness)
# clausePosition<- select(clause, Glottocode, ClausePosition)
# numClass<- select(num, Glottocode, NumClass.n)
# autotypmeta<- select(areas, Glottocode, Area, MajorBranch)
# names(autotypmeta)[2]<- paste("area")
# names(autotypmeta)[3]<- paste("affiliation")
# 
# autotype<- merge(NPs, clausePosition, by= "Glottocode")
# autotyp1<- merge(autotype, numClass, by= "Glottocode")
# autotyp_f <- merge(autotyp1, autotypmeta, by= "Glottocode")
# names(autotyp_f)[1]<- paste("glottocode")
# autotyp_f <- distinct(autotyp_f)

areas<- autotyp.feature("Register")
autotypmeta<- select(areas, Glottocode, Area, MajorBranch)
names(autotypmeta)[1]<- paste("glottocode")
names(autotypmeta)[2]<- paste("area")
names(autotypmeta)[3]<- paste("affiliation")


f_names <- c("24a","27a","30a","32a","33a","37a","38a","41a","42a","43a","44a","46a",
             "47a","48a","52a","53a","54a","57a","59a","63a","64a","65a","66a","67a",
             "70a","71a","73a","81a","84a","85a","86a","87a","88a","89a","90a","91a",
             "92a","93a","98a", "99a","101a","102a","106a","107a","112a","143a")

temp_w <- wals.feature(f_names)
wals <- merge(temp_w, autotypmeta, by= "glottocode", all.x = T)
wals <- wals %>% distinct(language, .keep_all = T)
colnames(wals)[5:50] <- c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	
                          "Nominal.Plurality",	"Definite.Articles",	"Indefinite.Articles",	"Distance.Contrasts.in.Demonstratives",	
                          "Pronominal.and.Adnominal.Demonstratives",	"Third.Person.Pronouns.and.Demonstratives",	
                          "Gender.Distinctions.in.Independent.Personal.Pronouns",	"Indefinite.Pronouns",	"Intensifiers.and.Reflexive.Pronouns", 
                          "Person.Marking.on.Adpositions",	"Comitatives.and.Instrumentals",	"Ordinal.Numerals",	"Distributive.Numerals",	
                          "Position.of.Pronominal.Possessive.Affixes",	"Possessive.Classification",	"Noun.Phrase.Conjunction",	
                          "Nominal.and.Verbal.Conjunction",	"Perfective.Imperfective.Aspect",	"The.Past.Tense",	"The.Future.Tense",	
                          "The.Morphological.Imperative",	"The.Prohibitive",	"Optative",	"WordOrderSOV", "Order.of.Object..Oblique..and.Verb",	
                          "Order.of.Adposition.and.Noun.Phrase",	"Order.of.Genitive.and.Noun",	"Order.of.Adjective.and.Noun",	
                          "Order.of.Demonstrative.and.Noun",	"Order.of.Numeral.and.Noun",	"Order.of.Relative.Clause.and.Noun",	
                          "Order.of.Degree.Word.and.Adjective",	"Position.of.Polar.Question.Particles",	"Position.of.Interrogative.Phrases.in.Content.Questions",
                          "Alignment.of.Case.Marking.of.Full.Noun.Phrases",	"Alignment.of.Case.Marking.of.Pronouns",	"Expression.of.Pronominal.Subjects",	
                          "Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb")
wals$datasource <- factor('wals')


reference <- merge(wals, autotyp, all = T)
reference$type <- 'reference'

my_data$type <- 'data'

anea <- merge(reference, my_data, all = T, by = colnames(my_data)[which(colnames(my_data) %in% colnames(reference))])




subsample_max_no_families <- function(reference.feature, reference.affiliation, reference.name, reference.area, data.affiliation){
  df <- data.frame(reference.affiliation, reference.name, reference.area, reference.feature)
  df <- df %>% drop_na %>% filter(reference.affiliation != "")
  reference.affiliation <- df$reference.affiliation
  reference.area <- df$reference.area
  reference.name <- df$reference.name
  reference.feature <- df$reference.feature
  
  #preparation:
  # reference.affiliation <- reference.affiliation[!is.na(reference.feature)]
  # reference.name <- reference.name[!is.na(reference.feature)]
  # reference.area <- reference.area[!is.na(reference.feature)]
  # reference.feature <- reference.feature[!is.na(reference.feature)]
  # reference.feature <- reference.feature[!is.na(reference.affiliation)] #some affiliations in wals are unknown so <-
  # reference.name <- reference.name[!is.na(reference.affiliation)]
  # reference.area <- reference.area[!is.na(reference.affiliation)]
  # reference.affiliation <- reference.affiliation[!is.na(reference.affiliation)]
  # reference.feature <- reference.feature[!is.na(reference.area)] #some areas in wals are unknown so <-
  # reference.name <- reference.name[!is.na(reference.affiliation)]
  # reference.affiliation <- reference.affiliation[!is.na(reference.affiliation)]
  # reference.area <- reference.area[!is.na(reference.area)]
  
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
    print(u)
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
    family.names.tobesampled <- sample(reference.capacity.names$name[reference.capacity.names$capacity == reference.sample.family.distribution$family.size[u]], 
                                       reference.sample.family.distribution$family.samples[u], replace = FALSE)
    #Second step: sample features from those languages
    for(k in family.names.tobesampled){
      temp.language.sample <- sample(reference.name[reference.affiliation == k], reference.sample.family.distribution$family.size[u], replace = FALSE)
      reference.language.sample <- c(reference.language.sample, temp.language.sample)
    }
  }
  reference.sample <- data.frame(name = reference.language.sample, feature = reference.feature[match(reference.language.sample, reference.name)], 
                                 area = reference.area[match(reference.language.sample, reference.name)], 
                                 affiliation = reference.affiliation[match(reference.language.sample, reference.name)])
  return(reference.sample)
}



#A function to compute proportion of identical pairs of features. It allows to only compute pairs from different families (affiliations) and
#pairs from separate areas. feature, name, area and affiliation must correspond in their order, e.g. area[1] must belong to name[1].
pairwise_comparison <- function(feature, name, area, affiliation, separate_areas = FALSE, identical_areas = FALSE, separate_affiliations = FALSE){
  df <- data.frame(feature, name, area, affiliation) %>% drop_na
  area <- df$area
  feature <- df$feature
  affiliation <- df$affiliation
  name <- df$name
  pairwise.match = c()
  if(separate_areas == TRUE){
    for(u in 1:length(feature)){
      if(length(which(area != area[u])) > 0){
      pairwise.match <- c(pairwise.match, feature[u] == feature[area != area[u]])
    }
    }
  } 
  if(separate_affiliations == FALSE & separate_areas == FALSE & identical_areas == FALSE){
    for(u in 1:length(feature)){
      pairwise.match <- c(pairwise.match, feature[u] == feature[-u])
    }
  } 
  if(separate_affiliations == TRUE){
    for(u in 1:length(feature)){
      if(length(which(affiliation != affiliation[u])) > 0){
    pairwise.match <- c(pairwise.match, feature[u] == feature[affiliation != affiliation[u]])
    }
    }
  }
  if(identical_areas == TRUE){
    for(u in 1:length(feature)){
      if(length(which(area == area[u])) > 1){
      pairwise.match <- c(pairwise.match, feature[u] == feature[area == area[u] & name != name[u]])
    }
    }
  }
  return(sum(pairwise.match)/length(pairwise.match))
}





#Function to which a dataframe and feature name together with separate_area and _affiliation arguments can be passed.
#Does n.samples iterations of the distribution restriction of the reference dataset.
sample_pairwise_comparison <- function(reference.dataset, dataset, feature.name, n.samples, separate_areas = FALSE, 
                                       identical_areas = FALSE, separate_affiliations = FALSE){
  proportions <- c()
  for(u in 1:n.samples){
    sample.df <- subsample_max_no_families(reference.feature = reference.dataset[,feature.name], 
                                           reference.affiliation = reference.dataset[,'affiliation'], 
                                           data.affiliation = dataset[,'affiliation'], 
                                           reference.area = reference.dataset[,'area'], 
                                           reference.name = reference.dataset[,'language'])
    
    proportions <- c(proportions, pairwise_comparison(feature = sample.df$feature, area = sample.df$area, 
                                                      name = sample.df$name, affiliation = sample.df$affiliation, 
                                                      separate_affiliations = separate_affiliations, separate_areas = separate_areas,
                                                      identical_areas = identical_areas))
  }
  return(proportions)
}

#Plotting function for comparing feature parity: (note: using less than ~40000 n.samples will give some sampling error)

compare_proportions <- function(feature.name, n.samples, ylim){
  prop.sample <- sample_pairwise_comparison(reference.dataset = reference, dataset = my_data, 
                                            feature.name = feature.name, 
                                            n.samples = n.samples, separate_areas = T, separate_affiliations = F)
  prop.sample.separate_affiliation <- sample_pairwise_comparison(reference.dataset = reference, dataset = my_data, 
                                                                 feature.name = feature.name, 
                                                                 n.samples = n.samples, separate_areas = F, separate_affiliations = T)
  prop.sample.identical_areas <- sample_pairwise_comparison(reference.dataset = reference, dataset = my_data, 
                                                            feature.name = feature.name, 
                                                            n.samples = n.samples, separate_areas = F, separate_affiliations = F, identical_areas = T)
  
  passive.const.df <- my_data %>% select(feature.name, 'language', 'area', 'affiliation') %>% drop_na()
  
  plot(density(prop.sample.separate_affiliation), xlim= c(0, 1), ylim = c(0, ylim), col = 2, lwd = 1.5, main = feature.name)
  lines(density(prop.sample),  col = 1, lwd = 1.5)
  lines(density(prop.sample.identical_areas),  col = 3, lwd= 1.5)
  points(x = pairwise_comparison(feature = passive.const.df[,feature.name], area = passive.const.df$area, 
                                 name = passive.const.df$name, affiliation = passive.const.df$affiliation, 
                                 separate_affiliations = F, separate_areas = F), y = ylim/2.5, type = 'h', col = 5, lty = 2, lwd = 2)
  points(x = pairwise_comparison(feature = passive.const.df[,feature.name], area = passive.const.df$area, 
                                 name = passive.const.df$name, affiliation = passive.const.df$affiliation, 
                                 separate_affiliations = F, separate_areas = F), y = ylim/2.5, col = 5, lty = 2, lwd = 2)
  points(x = pairwise_comparison(feature = passive.const.df[,feature.name], area = passive.const.df$area, 
                                 name = passive.const.df$name, affiliation = passive.const.df$affiliation, 
                                 separate_affiliations = T, separate_areas = F), y = ylim/2.5, type = 'h', col = 4, lty = 2, lwd = 2)
  points(x = pairwise_comparison(feature = passive.const.df[,feature.name], area = passive.const.df$area, 
                                 name = passive.const.df$name, affiliation = passive.const.df$affiliation, 
                                 separate_affiliations = T, separate_areas = F), y = ylim/2.5, col = 4, lty = 2, lwd = 2)
  legend(legend = c('wals diff. affiliations', 'wals diff. areas', 'wals same area', 'anea', 'anea diff. affiliations'), 
         col = c(2,1,3,5,4), lwd = c(1.5, 1.5, 1.5, 2,2), lty = c(1,1,1,2,2), 'topright')
  
}


#The problem with using less than 40000 n.samples:

par(mfrow = c(1,2))
compare_proportions('Gender.n', n.samples = 100, ylim = 10)
compare_proportions('Gender.n', n.samples = 100, ylim = 10)

compare_proportions('Gender.n', n.samples = 100, ylim = 10)
compare_proportions('Gender.n', n.samples = 100, ylim = 10)

compare_proportions('NPAgrCat', n.samples = 500, ylim = 17)
compare_proportions('NPAgrCat', n.samples = 500, ylim = 17)

#Better, still not perfect (runtime ~20 min):
par(mfrow = c(1,2))
compare_proportions('Gender.n', n.samples = 20000, ylim = 4)
compare_proportions('Gender.n', n.samples = 20000, ylim = 4)

compare_proportions('Passive.Constructions', n.samples = 100, ylim = 40)
compare_proportions('Gender.n', n.samples = 1000, ylim = 4)




#Get a quick overview over percentage of proportions larger than the reference (same area):
#A lot of features habe to few data for the code to work:
colnames(my_data)[which(colnames(my_data) %in% colnames(reference))][c(59, 1,2,3,4,5,6,7,8,11,15,19,20,22,42,47,50,52,54,55,56,57)]
larger.prop <- c()
n.samples.overview <- 500
for(u in colnames(my_data)[which(colnames(my_data) %in% colnames(reference))][-c(59, 1,2,3,4,5,6,7,8,11,15,19,20,22,42,47,50,52,54,55,56,57)]){
  print(u)
  prop.sample <- sample_pairwise_comparison(reference.dataset = reference, dataset = my_data, 
                                            feature.name = u, n.samples = n.samples.overview, separate_areas = F, 
                                            separate_affiliations = F, identical_areas = T)
  prop.data <- pairwise_comparison(feature = my_data[,u], area = my_data$area, 
                      name = my_data$language, affiliation = my_data$affiliation, 
                      separate_affiliations = F, separate_areas = F)
  print(prop.data)
  print(mean(prop.sample))
  print(range(prop.sample))
  
  larger.prop <- c(larger.prop, length(which(prop.sample > prop.data))/n.samples.overview)
}
results <- data.frame(feature = colnames(my_data)[which(colnames(my_data) %in% colnames(reference))][-c(59, 1,2,3,4,5,6,7,8,11,15,19,20,22,42,47,50,52,54,55,56,57)], percentage = larger.prop*100)
results[38:74,]













#--------------------------------------------------------------------------------------------------------------#

passive.const.df <- my_data %>% select('Gender.n', 'language', 'area', 'affiliation') %>% drop_na()

pairwise_comparison(feature = passive.const.df[,'Gender.n'], area = passive.const.df$area, 
                    name = passive.const.df$name, affiliation = passive.const.df$affiliation, 
                    separate_affiliations = F, separate_areas = F)




# Investigate passive proportions with larger n.samples
prop.sample <- sample_pairwise_comparison(reference.dataset = wals, dataset = my_data,z
                                          feature.name = 'Gender.n',
                                          n.samples = 100, separate_areas = T, separate_affiliations = F)
prop.sample.separate_affiliation <- sample_pairwise_comparison(reference.dataset = wals, dataset = my_data,
                                                               feature.name = 'Passive.Constructions',
                                                               n.samples = 100, separate_areas = F, separate_affiliations = T)
prop.sample.identical_areas <- sample_pairwise_comparison(reference.dataset = wals, dataset = my_data,
                                                          feature.name = 'Gender.n',
                                                          n.samples = 100, separate_areas = F, separate_affiliations = F, identical_areas = T)

passive.const.df <- my_data %>% select(Passive.Constructions, Gender.n, language, area, affiliation) %>% drop_na()

# hist(prop.sample, nclass = 100, xlim= c(0, 1), col = rgb(0,.4,.4, .5), freq = F)
# hist(prop.sample.separate_affiliation, nclass = 100, xlim= c(0.25, 0.85), col = rgb(.4,0,.4, .5), add = T, freq = F)

plot(density(prop.sample.separate_affiliation), xlim= c(0.25, 1), ylim = c(0, 40), col = 2, lwd = 1.5, main = 'Passive Constructions')
lines(density(prop.sample), xlim= c(0.25, 0.85),  col = 1, lwd = 1.5)
lines(density(prop.sample.identical_areas), xlim= c(0.25, 0.85),  col = 3, lwd= 1.5)
points(x = pairwise_comparison(feature = passive.const.df$Passive.Constructions, area = passive.const.df$area,
                               name = passive.const.df$name, affiliation = passive.const.df$affiliation,
                               separate_affiliations = F, separate_areas = F), y = 15, type = 'h', col = 5, lty = 2, lwd = 2)
points(x = pairwise_comparison(feature = passive.const.df$Passive.Constructions, area = passive.const.df$area,
                               name = passive.const.df$name, affiliation = passive.const.df$affiliation,
                               separate_affiliations = F, separate_areas = F), y = 15, col = 5, lty = 2, lwd = 2)
points(x = pairwise_comparison(feature = passive.const.df$Passive.Constructions, area = passive.const.df$area,
                               name = passive.const.df$name, affiliation = passive.const.df$affiliation,
                               separate_affiliations = T, separate_areas = F), y = 15, type = 'h', col = 4, lty = 2, lwd = 2)
points(x = pairwise_comparison(feature = passive.const.df$Gender.n, area = passive.const.df$area,
                               name = passive.const.df$name, affiliation = passive.const.df$affiliation,
                               separate_affiliations = T, separate_areas = F), y = 15, col = 4, lty = 2, lwd = 2)
legend(legend = c('wals diff. affiliations', 'wals diff. areas', 'wals same area', 'anea', 'anea diff. affiliations'),
       col = c(2,1,3,5,4), lwd = c(1.5, 1.5, 1.5, 2,2), lty = c(1,1,1,2,2), 'topright')



