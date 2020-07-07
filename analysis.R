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

autotyp <- autotyp.feature(c("NP_structure","Clause_linkage", "Numeral_classifiers", "Register")) 
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
wals <- merge(temp_w, autotypmeta, by= "glottocode")
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
  reference.glottocode <- reference.glottocode[!is.na(reference.feature)]
  reference.feature <- reference.feature[!is.na(reference.feature)]
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

#This function adapts the distribution of families and keeps the maximal number of families as possible:

subsample_max_no_families <- function(reference.feature, reference.affiliation, data.affiliation){
  #preparation:
  reference.feature <- reference.feature[which(!is.na(reference.feature))]
  reference.affiliation <- reference.affiliation[which(!is.na(reference.feature))]
  data.feature <- data.feature[which(!is.na(data.feature))]
  data.affiliation <- data.affiliation[which(!is.na(data.affiliation))]
  data.distribution <- sort(as.numeric(table(data.affiliation)), decreasing = T) #Get data distribution (no. of family members)
  
  reference.family.distribution.size <- as.numeric(names(table(as.numeric(table(reference.affiliation))))) #Get family sizes
  reference.family.distribution <- as.numeric(table(as.numeric(table(reference.affiliation)))) #Get family size distribution
  
  #Instead of taking the maximal number of languages, we aim to take the maximal number of families.
  #Hence we have to rearrange the data in family size: e.g. if we have two families in the data with one language,
  #we have two families with size one. 
  # For example, if we'd have: one with 1 language, three with two language and two with four languages,
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
  reference.feature.sample <- c()
  for(u in 1:nrow(reference.sample.family.distribution)){
    #First step: sample languages
    family.names.tobesampled <- sample(reference.capacity.names$name[which(reference.capacity.names$capacity == reference.sample.family.distribution$family.size[u])], 
                                       reference.sample.family.distribution$family.samples, replace = FALSE)
    #Second step: sample features from those languages
    for(k in family.names.tobesampled){
      temp.feature.sample <- sample(reference.feature[which(reference.affiliation == k)], reference.sample.family.distribution$family.size[u], replace = FALSE)
      reference.feature.sample <- c(reference.feature.sample, temp.feature.sample)
    }
  }
  return(reference.feature.sample)
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

