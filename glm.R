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
  #preparation:
  reference.affiliation <- reference.affiliation[!is.na(reference.feature)]
  reference.name <- reference.name[!is.na(reference.feature)]
  reference.area <- reference.area[!is.na(reference.feature)]
  reference.feature <- reference.feature[!is.na(reference.feature)]
  reference.feature <- reference.feature[!is.na(reference.affiliation)] #some affiliations in wals are unknown so <-
  reference.name <- reference.name[!is.na(reference.affiliation)]
  reference.area <- reference.area[!is.na(reference.affiliation)]
  reference.affiliation <- reference.affiliation[!is.na(reference.affiliation)]
  reference.feature <- reference.feature[!is.na(reference.area)] #some areas in wals are unknown so <-
  reference.name <- reference.name[!is.na(reference.affiliation)]
  reference.affiliation <- reference.affiliation[!is.na(reference.affiliation)]
  reference.area <- reference.area[!is.na(reference.area)]
  
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
    #First step: sample families
    family.names.tobesampled <- sample(reference.capacity.names$name[reference.capacity.names$capacity == reference.sample.family.distribution$family.size[u]], 
                                       size = reference.sample.family.distribution$family.samples[u], replace = FALSE)
    #Second step: sample features from those families
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



pairwise_comparison_df <- function(feature, name, area, affiliation, separate_areas = FALSE, identical_areas = FALSE, separate_affiliations = FALSE){
  pairwise.match = c()
  pair.name <- c()
  pair.area <- c()
  if(separate_areas == TRUE){
    for(u in 1:length(feature)){
      if(length(which(area != area[u])) > 0){
      pairwise.match <- c(pairwise.match, feature[u] == feature[area != area[u]])
      pair.name <- c(pair.name, paste(name[u], name[area != area[u]]))
      pair.area <- c(pair.area, paste(area[u], area[area != area[u]]))
      }
    }
  }
  if(separate_affiliations == FALSE & identical_areas == FALSE & separate_areas == FALSE){
    for(u in 1:length(feature)){
      pairwise.match <- c(pairwise.match, feature[u] == feature[-u])
      pair.name <- c(pair.name, paste(name[u], name[-u]))
      pair.area <- c(pair.area, paste(area[u], area[-u]))
    }
  } 
  if(separate_affiliations == TRUE){
    for(u in 1:length(feature)){
      if(length(which(affiliation != affiliation[u])) > 0){
      pairwise.match <- c(pairwise.match, feature[u] == feature[affiliation != affiliation[u]])
      pair.name <- c(pair.name, paste(name[u], name[area != area[u]]))
      pair.area <- c(pair.area, paste(area[u], area[area != area[u]]))
      }
    }
  }
  if(identical_areas == TRUE){
    for(u in 1:length(feature)){
      if(length(which(area == area[u])) > 1){
        pairwise.match <- c(pairwise.match, feature[u] == feature[area == area[u] & name != name[u]])
        pair.name <- c(pair.name, paste(name[u], name[area == area[u] & name != name[u]]))
        if(length(pairwise.match) != length(pair.name)) print(u)
        pair.area <- c(pair.area, paste(area[u], area[area == area[u] & name != name[u]]))
      } 
    }
  }
  return(data.frame(match = pairwise.match, pair.name, pair.area))
}


test.ref <-  wals %>% select(Passive.Constructions, area, affiliation, language) %>% drop_na()
test.dat <- my_data %>% select(Passive.Constructions, area, affiliation, language) %>% drop_na() 

sample <- subsample_max_no_families(reference.feature = test.ref$Passive.Constructions, reference.affiliation = test.ref$affiliation, 
                                    reference.name = test.ref$language, reference.area = test.ref$area, data.affiliation = test.dat$affiliation)

pairwise_comparison(feature = sample$feature, name = sample$name, area = sample$area, 
                    affiliation = sample$affiliation, identical_areas = T, separate_areas = F, separate_affiliations = F)

ref.comparison <- cbind(pairwise_comparison_df(feature = sample$feature, name = sample$name, area = sample$area, 
                                            affiliation = sample$affiliation, identical_areas = T), type = factor('reference'))

dat.comparison <- cbind(pairwise_comparison_df(feature = test.dat$Passive.Constructions, name = test.dat$language, 
                                      area = test.dat$area, affiliation = test.dat$area, identical_areas = T), type = factor('data'))

set <- rbind(ref.comparison, dat.comparison)

mosaicplot(type ~ match, data = set)

glm(formula = match ~ type, family = binomial, data = set)




