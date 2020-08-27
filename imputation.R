require(tidyverse)
require(lingtypology)
require(pheatmap) #Missing data visualization
require(missMDA) #Imputation
require(FactoMineR) #MCA dimensionality reduction
require(brms) #To use rdirichlet() for posterior sampling.
require(factoextra) #plotting mca output
require(StatMatch) #to compute dissimilarity matrices


#------------------------------------ Data prep. ---------------------------------------#

my_data <- read.csv('~/ANEA/features.csv') #Read middle eastern ancient languages features

#categories 
semantics<- c("Perfective.Imperfective.Aspect",  "The.Past.Tense",  "The.Future.Tense",  
              "The.Morphological.Imperative",  "The.Prohibitive",  "Optative", "Reciprocal.Constructions",  
              "Passive.Constructions")
morphology<- c("Locus.of.Marking.in.Possessive.Noun.Phrases", "Reduplication", "Nominal.Plurality",  "Definite.Articles",
               "Indefinite.Articles", "Distance.Contrasts.in.Demonstratives",  
               "Pronominal.and.Adnominal.Demonstratives",  "Third.Person.Pronouns.and.Demonstratives",  
               "Gender.Distinctions.in.Independent.Personal.Pronouns",  "Indefinite.Pronouns",  
               "Intensifiers.and.Reflexive.Pronouns", "Person.Marking.on.Adpositions",  
               "Comitatives.and.Instrumentals", "Noun.Phrase.Conjunction", "Nominal.and.Verbal.Conjunction", 
               "Expression.of.Pronominal.Subjects",  "Verbal.Person.Marking", "Negative.Morphemes", "NPMarking", "NPHeadlessness")
syntax<- c("Gender.n", "Systems.of.Gender.Assignment", "Ordinal.Numerals",  "Distributive.Numerals",
          "Position.of.Pronominal.Possessive.Affixes",  "Possessive.Classification", "WordOrderSOV",  "Order.of.Object..Oblique..and.Verb",  "Order.of.Adposition.and.Noun.Phrase",  
          "Order.of.Genitive.and.Noun",  "Order.of.Adjective.and.Noun",  "Order.of.Demonstrative.and.Noun",  
          "Order.of.Numeral.and.Noun",  "Order.of.Relative.Clause.and.Noun",  "Order.of.Degree.Word.and.Adjective",  
          "Position.of.Polar.Question.Particles",  "Position.of.Interrogative.Phrases.in.Content.Questions",  
          "Alignment.of.Case.Marking.of.Full.Noun.Phrases",  "Alignment.of.Case.Marking.of.Pronouns",
          "Order.of.Negative.Morpheme.and.Verb", "ClausePosition", "NumClass.n")


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
colnames(autotyp)[which(colnames(autotyp) == 'Latitude')] <- 'latitude'
colnames(autotyp)[which(colnames(autotyp) == 'Longitude')] <- 'longitude'



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



reference <- merge(wals, autotyp, all = T, by = c('language', 'glottocode', 'area', 'affiliation', 'latitude', 'longitude')) 
reference$type <- 'reference'

my_data$type <- 'data'

anea <- merge(reference, my_data, all = T, by = colnames(my_data)[which(colnames(my_data) %in% colnames(reference))]) %>% 
  arrange(type)
cols_data_NA <- which(colSums(1*!is.na(anea[anea$type == 'data',])) == 0)
cols_reference_NA <- which(colSums(1*!is.na(anea[anea$type == 'reference',])) == 0)
# rows_NA <- which(rowSums(1*!is.na(anea)) < 10)
# cols_few_data <- which(colSums(1*!is.na(anea[anea$type == 'reference',])) < 2000)
anea <- anea[,-c(cols_data_NA, cols_reference_NA)]

anea %>% group_by(language) %>% mutate(c = n()) %>% filter(c > 1)


#------------------------------------ Imputation with MCA ---------------------------------------#

#Subsetting:
#Finding subsets by hand such that imputeMCA() converges. Possibly, there is a fully automated way
#to do this in an optimal way - as long as there is no clear idea about the minimal amount of data
#required for imputation with imputeMCA(), this is anyway not applicable.

#Visualize missing data:
cols_info <- -c(1:7,59)
anea.miss = as.matrix(1*is.na(anea[-which(duplicated(anea$language)),cols_info])) 
# rownames(anea.miss) <- anea$type[-which(duplicated(anea$language))]
# rownames(anea.miss) <- anea$language[-which(duplicated(anea$language))]
pheatmap(t(anea.miss),cluster_rows=T, cluster_cols=T, legend = FALSE, cex = .9,
         color = c("olivedrab", "chocolate1")) #red is missing
pheatmap(t(anea.miss),cluster_rows=T, cluster_cols=T, legend = FALSE, cex = .9,
         color = c(colors()[412], colors()[563])) #red is missing

#Try a simple clustering by amount of missingness of features into two groups and impute..
plot(hclust(dist(t(anea.miss))))
ngroups <- 3 #How many feature subsets there shall be.
memb <- cutree(tree = hclust(dist(t(anea.miss))), k = ngroups)

anea.g1 <- anea[, c(-cols_info, which(colnames(anea) %in% names(memb[memb == 1])))]
anea.g2 <- anea[, c(-cols_info, which(colnames(anea) %in% names(memb[memb == 2])))]
anea.g3 <- anea[, c(-cols_info, which(colnames(anea) %in% names(memb[memb == 3])))]

table(rowSums(as.matrix(1*!is.na(anea.g1[,-c(1:8)]))))
table(rowSums(as.matrix(1*!is.na(anea.g2[,-c(1:8)]))))
table(rowSums(as.matrix(1*!is.na(anea.g3[,-c(1:8)]))))

threshold <- 2 #How many features the languages in the subsets shall have at least.

length(unique(c(which(rowSums(as.matrix(1*!is.na(anea.g1[,-c(1:8)]))) <= threshold),
                which(rowSums(as.matrix(1*!is.na(anea.g2[,-c(1:8)]))) <= threshold),
                which(rowSums(as.matrix(1*!is.na(anea.g3[,-c(1:8)]))) <= threshold)))) #How many languages are lost in at least one subset due to threshold

length(intersect(intersect(which(rowSums(as.matrix(1*!is.na(anea.g1[,-c(1:8)]))) <= threshold),
                which(rowSums(as.matrix(1*!is.na(anea.g2[,-c(1:8)]))) <= threshold)),
                which(rowSums(as.matrix(1*!is.na(anea.g3[,-c(1:8)]))) <= threshold))) #How many languages are lost in all subsets due to threshold

stopifnot("no observations below threshold" = length(which(rowSums(as.matrix(1*!is.na(anea.g1[,-c(1:8)]))) <= threshold)) != 0)
anea.g1 <- anea.g1[-which(rowSums(as.matrix(1*!is.na(anea.g1[,-c(1:8)]))) <= threshold) ,]
pheatmap(t(as.matrix(1*is.na(anea.g1[,-c(1:8)]))), legend = FALSE, cluster_cols = F, cluster_rows = F) 

stopifnot("no observations below threshold" = length(which(rowSums(as.matrix(1*!is.na(anea.g2[,-c(1:8)]))) <= threshold)) != 0)
anea.g2 <- anea.g2[-which(rowSums(as.matrix(1*!is.na(anea.g2[,-c(1:8)]))) <= threshold),]
pheatmap(t(as.matrix(1*is.na(anea.g2[,-c(1:8)]))), legend = FALSE, cluster_cols = F, cluster_rows = F) 

stopifnot("no observations below threshold" = length(which(rowSums(as.matrix(1*!is.na(anea.g3[,-c(1:8)]))) <= threshold)) != 0)
anea.g3 <- anea.g3[-which(rowSums(as.matrix(1*!is.na(anea.g3[,-c(1:8)]))) <= threshold),]
pheatmap(t(as.matrix(1*is.na(anea.g3[,-c(1:8)]))), legend = FALSE, cluster_cols = F, cluster_rows = F) 


#Subsetting into conceptual categories
anea.semantics <- anea[, c(colnames(anea)[1:7],semantics)]
anea.morphology <- anea[, c(colnames(anea)[1:7],morphology)]
anea.syntax <- anea[, c(colnames(anea)[1:7],syntax)]

table(rowSums(as.matrix(1*!is.na(anea.semantics[,-c(1:7)]))))
table(rowSums(as.matrix(1*!is.na(anea.morphology[,-c(1:7)]))))
table(rowSums(as.matrix(1*!is.na(anea.syntax[,-c(1:7)]))))

threshold <- 15 #More than "threshold" values does a language have to have. Imputation further below fails unless its 

length(unique(c(which(rowSums(as.matrix(1*!is.na(anea.semantics[,-c(1:7)]))) <= threshold),
                which(rowSums(as.matrix(1*!is.na(anea.morphology[,-c(1:7)]))) <= threshold),
                which(rowSums(as.matrix(1*!is.na(anea.syntax[,-c(1:7)]))) <= threshold)))) #How many languages are lost in at least one susbet due to threshold

length(intersect(intersect(which(rowSums(as.matrix(1*!is.na(anea.semantics[,-c(1:7)]))) <= threshold),
                           which(rowSums(as.matrix(1*!is.na(anea.morphology[,-c(1:7)]))) <= threshold)),
                 which(rowSums(as.matrix(1*!is.na(anea.syntax[,-c(1:7)]))) <= threshold))) #How many languages are lost in all subsets due to threshold

stopifnot("no observations below threshold" = length(which(rowSums(as.matrix(1*!is.na(anea.semantics[,-c(1:8)]))) <= threshold)) != 0)
anea.semantics <- anea.semantics[-which(rowSums(as.matrix(1*!is.na(anea.semantics[,-c(1:7)]))) <= threshold) ,]
pheatmap(t(as.matrix(1*is.na(anea.semantics[,-c(1:8)]))), legend = FALSE, cluster_cols = F, cluster_rows = F) 

stopifnot("no observations below threshold" = length(which(rowSums(as.matrix(1*!is.na(anea.morphology[,-c(1:8)]))) <= threshold)) != 0)
anea.morphology <- anea.morphology[-which(rowSums(as.matrix(1*!is.na(anea.morphology[,-c(1:7)]))) <= threshold),]
pheatmap(t(as.matrix(1*is.na(anea.morphology[,-c(1:8)]))), legend = FALSE, cluster_cols = F, cluster_rows = F) 

stopifnot("no observations below threshold" = length(which(rowSums(as.matrix(1*!is.na(anea.syntax[,-c(1:8)]))) <= threshold)) != 0)
anea.syntax <- anea.syntax[-which(rowSums(as.matrix(1*!is.na(anea.syntax[,-c(1:7)]))) <= threshold),]
pheatmap(t(as.matrix(1*is.na(anea.syntax[,-c(1:8)]))), legend = FALSE, cluster_cols = F, cluster_rows = F) 

#Throwing away from complete anea
table(rowSums(as.matrix(1*!is.na(anea[,-c(1:7,29:32, 59)]))))
threshold <- 1
#putting away "NumClass.n", "NPHeadlessness", "NPMarking" ,"NPAgrCat" = cols 29:32
stopifnot("no observations below threshold" = length(which(rowSums(as.matrix(1*!is.na(anea[,-c(1:7, 59, 29:32)]))) <= threshold)) != 0)
anea.complete <- anea[-which(rowSums(as.matrix(1*!is.na(anea[,-c(1:7, 59, 29:32)]))) <= threshold),]
pheatmap(t(1*is.na(anea.complete[,-c(1:7, 59, 29:32)])),cluster_rows=T, cluster_cols=T, legend = FALSE, cex = .9,
         color = c("olivedrab", "chocolate4")) #red is missing
pheatmap(t(as.matrix(1*is.na(anea.complete[,-c(1:7, 59, 29:32)]))), legend = TRUE, cluster_cols = F, cluster_rows = F) 

#Impute missing values:
anea.g1.imp <- imputeMCA(anea.g1[,-c(1:8)], ncp = 2) #threshold 2 is enough to get these two working.
anea.g2.imp <- imputeMCA(anea.g2[,-c(1:8)])
anea.g3.imp <- imputeMCA(anea.g3[,-c(1:8)]) #fails for some reason.. it is the ominous autotyp set..

anea.semantics.imp <- imputeMCA(anea.semantics[,-c(1:7)], ncp =4) #works with threshold 0.
anea.morphology.imp <- imputeMCA(anea.morphology[,-c(1:7)], ncp = 2) #works with threshold ~8.
anea.syntax.imp <- imputeMCA(anea.syntax[,-c(1:7)], ncp = 2) #works with threshold 15

estim_ncpMCA(anea.complete[,-c(1:7,29:32, 55)]) #should give the number of ncp (dimensions to be used in imputation)
anea.complete.imp <- imputeMCA(anea.complete[,-c(1:7,29:32, 55)]) #works with threshold 15 AND putting away "NumClass.n", "NPHeadlessness", "NPMarking" ,"NPAgrCat" 




#------------------------------------ Imputation based on family proportions ---------------------------------------#

#A code that imputes missing values based on the family proportions.

#To incorporate the uncertainty of the family proportions, we use bayesian methodology.
#We use a dirichlet prior that is updated by the observations from the family. Then we use
#a sample from the posterior distribution of the proportions to again sample feature values.

#Because the dirichlet is the conjugate prior of the multinomial distribution, we can 
#easily compute the posterior (see: https://stats.stackexchange.com/questions/393629/how-to-use-the-dirichlet-prior-for-estimating-the-multinomial-parameters ).

#We set the prior alpha paremeters to be small in order to allow the data to take large influence.
#In absence of any information, the alphas shall furthermore reflect the global proportion (mean family proportion or global proporiton).


sample_feature_values <- function(feature, affiliation, family_prop_prior = TRUE, prior_constant = 1){
  
  data.set <- data.frame(affiliation, feature)
  
  #Impute an artificial affiliation for isolates (?)
  data.set <- data.set %>% rowwise() %>% filter(!is.na(affiliation)) %>% 
    mutate(affiliation = case_when(affiliation == '' ~ paste(cur_group_id()), 
                                   TRUE ~ affiliation),
           feature = as.character(feature)) #necessary because some features are counts
  
  #Make a dataset with families in the rows:
  family_counts <- data.set %>% filter(!is.na(feature)) %>%  group_by(affiliation, feature) %>% count() %>%
    mutate(feature = case_when(feature == '' ~ '-',   #Some feature values are declared empty (''), must be renamed for pivot_wider to work
                               TRUE ~ feature)) %>% 
    group_by(affiliation) %>%  pivot_wider(names_from = feature, values_from = n, values_fill = 0)
  
  
  if(family_prop_prior == FALSE){
    #Compute global proportion 
    props.languages <- table(data.set$feature)/sum(!is.na(data.set$feature))
    prior.proportions <- props.languages[colnames(family_counts)[-1]]
  } else{
    #Compute familywise proportions
    family_proportions <- family_counts %>% rowwise() %>% mutate(rowtotal = sum(c_across())) %>% 
      mutate(across(where(is.numeric), ~.x/rowtotal))
    props.families <- colSums(family_proportions[,-1])/nrow(family_counts)
    prior.proportions <- props.families[-length(props.families)]
  }
  
  #Props.families or props.languages make the dirichlet prior up to a constant.
  #The magnitude of the constant determines the amount of prior information.
  #large -> data is overwritten by the prior.
  #small -> prior is overwritten by data.
  
  prior.pars <- prior_constant * prior.proportions
  
  #Make a matrix with rows giving the family posterior distribution.
  #(prior.pars has got to be the same order as family_counts)
  prior.pars.matrix <- matrix(rep(prior.pars, nrow(family_counts)), ncol = length(prior.pars), byrow = T)
  
  posterior.pars.matrix <- as.matrix(family_counts[,-1]) + prior.pars.matrix
  
  #Sample from the posterior parameters for the multinomial distribution of the features.
  #Where there is data:
  posterior.pars.sample <- matrix(apply(X = posterior.pars.matrix, MARGIN = 1, FUN = function(x) rdirichlet(1,x)), 
                                  ncol = length(prior.pars), byrow = T)
  posterior.pars.sample <- cbind(affiliation = family_counts$affiliation, as.data.frame(posterior.pars.sample))
  colnames(posterior.pars.sample) = colnames(family_counts)
  #Where there is no data:
  families.w.missing.values <- data.set %>% group_by(affiliation) %>% filter(all(is.na(feature))) %>% distinct(affiliation)
  posterior.pars.sample.novalues <- matrix(rep(rdirichlet(1,prior.pars), nrow(families.w.missing.values)), byrow = T, ncol = length(prior.pars))
  posterior.pars.sample.novalues <- cbind(families.w.missing.values$affiliation, as.data.frame(posterior.pars.sample.novalues))
  colnames(posterior.pars.sample.novalues) <- colnames(family_counts)
  
  posterior.pars.sample <- rbind(posterior.pars.sample, posterior.pars.sample.novalues)
  
  #Sample for all the missing values a feature value based on the posterior multinomial parameters sampled:
  sample_value <- function(family.posterior.parameters, affiliation){
    sample(size = 1, colnames(family.posterior.parameters)[-1], 
           prob = family.posterior.parameters[which(family.posterior.parameters$affiliation == affiliation), -1])
  }
  
  data.set <- data.set %>% 
    mutate(feature_n = case_when(is.na(feature) ~ sample_value(family.posterior.parameters = posterior.pars.sample, affiliation = affiliation),
                                 TRUE ~ feature))
  return(data.set$feature_n)
}

#test: sample_feature_values(data.test$WordOrderSOV, data.test$affiliation, family_prop_prior = FALSE, prior_constant = 100)

anea.fam.imp <- anea[-which(is.na(anea$affiliation)),c(1:7,59)]
for(u in colnames(anea)[-c(1:7, 59)]){
  print(u)
  anea.fam.imp <- cbind(anea.fam.imp, sample_feature_values(anea[-which(is.na(anea$affiliation)),u], anea.fam.imp$affiliation))  
}



#-------------------------- Preprocessing prior to analysis ----------------------------#

#Dimensionality reduction prior to computation of dissimilarity (not looking good):
mca.anea.complete <- MCA(anea.complete.imp$completeObs, ncp = 10, graph = F)
fviz_screeplot(mca.anea.complete, addlabels = TRUE, ylim = c(0, 45)) #first ten eigenvalues are very small

mca.anea.complete.2 <- MCA(X = anea.complete[,-c(1:7,29:32, 59)], tab.disj=anea.complete.imp$tab.disj) 


mca.anea.g1 <- MCA(anea.g1[,-c(1:8)], tab.disj = anea.g1.imp$tab.disj, graph = F)
fviz_screeplot(mca.anea.g1, addlabels = TRUE, ylim = c(0, 45)) 

mca.anea.g2 <- MCA(anea.g2[,-c(1:8)], tab.disj = anea.g2.imp$tab.disj, graph = F)
fviz_screeplot(mca.anea.g2, addlabels = TRUE, ylim = c(0, 45)) #first ten eigenvalues are very small

# mca.anea.g3 <- MCA(anea.g3[,-c(1:8)], tab.disj = anea.g3.imp$tab.disj)
# fviz_screeplot(mca.anea.g1, addlabels = TRUE, ylim = c(0, 45)) #first ten eigenvalues are very small

mca.anea.semantics <- MCA(anea.semantics[,-c(1:7)], tab.disj = anea.semantics.imp$tab.disj)
fviz_screeplot(mca.anea.semantics, addlabels = TRUE, ylim = c(0, 45)) #Also not really promising

mca.anea.morphology <- MCA(anea.morphology[,-c(1:7)], tab.disj = anea.morphology.imp$tab.disj)
fviz_screeplot(mca.anea.semantics, addlabels = TRUE, ylim = c(0, 45)) #20 features, 8 e.v. make 70%

mca.anea.syntax <- MCA(anea.syntax[,-c(1:7)], tab.disj = anea.syntax.imp$tab.disj)
fviz_screeplot(mca.anea.syntax, addlabels = TRUE, ylim = c(0, 45)) #not working



#Dissimilarity of raw features:
anea.complete.dist <- gower.dist(anea.complete.imp$completeObs)
anea.complete.area <- gower.dist(anea.complete$area)
anea.complete.affiliation = gower.dist(anea.complete$affiliation)

rda(Y = anea.complete.dist, Z = anea.complete.affiliation, X = anea.complete.area)
rda(Y = anea.complete.imp$completeObs, Z = anea.complete$affiliation, X = anea.complete$area)







test.set <- anea.complete
test.set <- drop_na(test.set, affiliation, area) %>% rowwise() %>% 
  mutate(affiliation = case_when(affiliation == '' ~ as.character(cur_group_id()), 
                                 TRUE ~ affiliation))

test.imp <- imputeMCA(test.set[,-c(1:7)])

test.dist <- gower.dist(test.imp$completeObs)
area.dist <- gower.dist(test.set$area)
affiliation.dist <- gower.dist(test.set$affiliation)
for(i in 1:ncol(dim.red$ind$coord)){
  dim.red$ind$coord[,i] <- scale(dim.red$ind$coord[,i])*dim.red$eig[i,'percentage of variance']}

dim.red <- MCA(test.set[,-c(1:7)], tab.disj = test.imp$tab.disj)
mca.dist <- gower.dist(dim.red$ind$coord)


rda(test.dist ~ area.dist + Condition(affiliation.dist))
rda(Y = test.dist, X = area.dist, Z =affiliation.dist)

rda(mca.dist ~ area.dist + Condition(affiliation.dist))
dbrda(mca.dist ~ area.dist + Condition(affiliation.dist))




pc.mat <- dim.red$ind$coord
pc.dist <- as.dist(pc.mat, diag = FALSE, upper = FALSE)
area <- factor(test.set$area)
area.mat <- model.matrix(~area)
affiliation <- factor(test.set$affiliation)
affiliation.mat <- model.matrix(~affiliation)
prda <- rda(pc.mat ~ area.mat)
srda <- rda(pc.mat ~ area.mat + Condition(affiliation.mat))






