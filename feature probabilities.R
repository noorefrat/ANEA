library(dplyr)
library(lingtypology)

#ANEA proportions & binomial distributions per feature

fnames<-  list("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	"Nominal.Plurality",	"Definite.Articles",	"Indefinite.Articles",	"Distance.Contrasts.in.Demonstratives",	"Pronominal.and.Adnominal.Demonstratives",	"Third.Person.Pronouns.and.Demonstratives",	"Gender.Distinctions.in.Independent.Personal.Pronouns",	"Indefinite.Pronouns",	"Intensifiers.and.Reflexive.Pronouns", "Person.Marking.on.Adpositions",	"Comitatives.and.Instrumentals",	"Ordinal.Numerals",	"Distributive.Numerals",	"Position.of.Pronominal.Possessive.Affixes",	"Possessive.Classification",	"Noun.Phrase.Conjunction",	"Nominal.and.Verbal.Conjunction",	"Perfective.Imperfective.Aspect",	"The.Past.Tense",	"The.Future.Tense",	"The.Morphological.Imperative",	"The.Prohibitive",	"Optative",	"WordOrderSOV", "Order.of.Object..Oblique..and.Verb",	"Order.of.Adposition.and.Noun.Phrase",	"Order.of.Genitive.and.Noun",	"Order.of.Adjective.and.Noun",	"Order.of.Demonstrative.and.Noun",	"Order.of.Numeral.and.Noun",	"Order.of.Relative.Clause.and.Noun",	"Order.of.Degree.Word.and.Adjective",	"Position.of.Polar.Question.Particles",	"Position.of.Interrogative.Phrases.in.Content.Questions",	"Alignment.of.Case.Marking.of.Full.Noun.Phrases",	"Alignment.of.Case.Marking.of.Pronouns",	"Expression.of.Pronominal.Subjects",	"Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb", "NPMarking", "NPHeadlessness", "ClausePosition", "NumClass.n")
my_data<- features_all

#function to sample pairs of languages from the same/different area
pair_area <- function(x, sample_size=100, same_area=TRUE) {
  same_area_comparisons <- c()
  
  while (length(same_area_comparisons) < sample_size){
    for(lang in x) {
      
      sample_dialect<- sample(x[,1], 2, replace = TRUE)
      
      lang1 <- x[x$X.U.FEFF.Dialect == sample_dialect[1],][1,]
      lang2 <- x[x$X.U.FEFF.Dialect == sample_dialect[2],][1,]

      if (same_area == TRUE){
        if (lang1["area"] == lang2["area"]){
          
          if (lang1[,2] == lang2[,2]){
            value <- 1.0
          } else {
            if (grepl(lang1[,2], lang2[,2], fixed=TRUE) || grepl(lang2[,2], lang1[,2], fixed=TRUE)) {
              value <- 0.5
              
            } else{
              value <- 0.0
            }
            
          }
          
          same_area_comparisons <- c(same_area_comparisons, value)
        }  
      } else {
        if (lang1["area"] != lang2["area"]){
          
          if (lang1[,2] == lang2[,2]){
            value <- 1.0
          } else {
            if (grepl(lang1[,2], lang2[,2], fixed=TRUE) || grepl(lang2[,2], lang1[,2], fixed=TRUE)) {
              value <- 0.5
              
            } else{
              value <- 0.0
            }
            
          }
          
          
          same_area_comparisons <- c(same_area_comparisons, value)
        }
      }
      if (length(same_area_comparisons) == 100){
        break
      }
    }
  }
  
  return(same_area_comparisons)
}

#function to sample pairs of languages from the same/different affiliation
pair_affiliation <- function(x, sample_size=100, same_affiliation=TRUE) {
  same_affiliation_comparisons <- c()
  
  while (length(same_affiliation_comparisons) < sample_size){
    for(lang in x) {
      
      sample_dialect<- sample(x[,1], 2, replace = TRUE)
      
      lang1 <- x[x$X.U.FEFF.Dialect == sample_dialect[1],][1,]
      lang2 <- x[x$X.U.FEFF.Dialect == sample_dialect[2],][1,]
      
      if (same_affiliation == TRUE){
        if (lang1["affiliation"] == lang2["affiliation"]){
          
          if (lang1[,2] == lang2[,2]){
            value <- 1.0
          } else {
            if (grepl(lang1[,2], lang2[,2], fixed=TRUE) || grepl(lang2[,2], lang1[,2], fixed=TRUE)) {
              value <- 0.5
              
            } else{
              value <- 0.0
            }
            
          }
          
          same_affiliation_comparisons <- c(same_affiliation_comparisons, value)
        }  
      } else {
        if (lang1["affiliation"] != lang2["affiliation"]){
          
          if (lang1[,2] == lang2[,2]){
            value <- 1.0
          } else {
            if (grepl(lang1[,2], lang2[,2], fixed=TRUE) || grepl(lang2[,2], lang1[,2], fixed=TRUE)) {
              value <- 0.5
              
            } else{
              value <- 0.0
            }
            
          }
          
          
          same_affiliation_comparisons <- c(same_affiliation_comparisons, value)
        }
      }
      if (length(same_affiliation_comparisons) == 100){
        break
      }
    }
  }
  
  return(same_affiliation_comparisons)
}

#function to sample pairs of languages from the same area and affiliation
pair_both <- function(x, sample_size=2, same_both=TRUE){
  same_both_comparisons <- c()
  
  while (length(same_both_comparisons) < sample_size){
    for(lang in x) {

      sample_dialect<- sample(x[,1], 100, replace = TRUE)
      lang1 <- x[x$X.U.FEFF.Dialect == sample_dialect[1],][1,]
      lang2 <- x[x$X.U.FEFF.Dialect == sample_dialect[2],][1,]
      
      if (same_both == TRUE){
        if (lang1["affiliation"] == lang2["affiliation"] && lang1["area"] == lang2["area"]){
          #print("match!")
          if (lang1[,2] == lang2[,2]){
            value <- 1.0
          } else {
            if (grepl(lang1[,2], lang2[,2], fixed=TRUE) || grepl(lang2[,2], lang1[,2], fixed=TRUE)) {
              value <- 0.5
              
            } else{
              value <- 0.0
            }
          }
          same_both_comparisons <- c(same_both_comparisons, value)
        }  
      } else {
        if (lang1["affiliation"] != lang2["affiliation"]  && lang1["area"] != lang2["area"]){
          
          if (lang1[,2] == lang2[,2]){
            value <- 1.0
          } else {
            if (grepl(lang1[,2], lang2[,2], fixed=TRUE) || grepl(lang2[,2], lang1[,2], fixed=TRUE)) {
              value <- 0.5
              
            } else{
              value <- 0.0
            }
            
          }
          
          same_both_comparisons <- c(same_both_comparisons, value)
        }
      }
      if (length(same_both_comparisons) == 100){
        break
      }
      
    }
  }
  
  return(same_both_comparisons)
}


feature <- c()
proportion_area <- c()
proportion_affiliation <- c()
proportion_areaf <- c()
proportion_affiliationf <- c()
proportion_both <- c()


prob_area <- list()
prob_affiliation <- list()
prob_areaf <- list()
prob_affiliationf <- list()
prob_both <- list()

for (n in fnames){
  
  outcome_area <- c()
  outcome_affiliation <- c()
  outcome_areaf <- c()
  outcome_affiliationf <- c()
  outcome_both <- c()
 
  temp_w<- select(my_data, X.U.FEFF.Dialect, n, area, affiliation)
  temp_w <- na.omit(temp_w)
  
  outcome_area <- pair_area(temp_w)
  outcome_affiliation <- pair_affiliation(temp_w)
  outcome_areaf <- pair_area(temp_w, same_area=FALSE)
  outcome_affiliationf <- pair_affiliation(temp_w, same_affiliation = FALSE)
  outcome_both <- pair_both(temp_w)
  
  
  feature <- c(feature, n)
  proportion_area <- c(proportion_area, mean(outcome_area))
  proportion_affiliation <- c(proportion_affiliation, mean(outcome_affiliation))
  proportion_areaf <- c(proportion_areaf, mean(outcome_areaf))
  proportion_affiliationf <- c(proportion_affiliationf, mean(outcome_affiliationf))
  proportion_both <- c(proportion_both, mean(outcome_both))
  
  prob_area[n] <- list(rbinom(1000000, 100, proportion_area)/100)
  prob_affiliation[n] <- list(rbinom(1000000, 100, proportion_affiliation)/100)
  prob_areaf[n] <- list(rbinom(1000000, 100, proportion_areaf)/100)
  prob_affiliationf[n] <- list(rbinom(1000000, 100, proportion_affiliationf)/100)
  prob_both[n] <- list(rbinom(1000000, 100, proportion_both)/100)
  
  
}

prob_area_anea <- data.frame(prob_area)
prob_affiliation_anea <- data.frame(prob_affiliation)
prob_areaf_anea <- data.frame(prob_areaf)
prob_affiliationf_anea <- data.frame(prob_affiliationf)
prob_both_anea <- data.frame(prob_both)



#AUTOTYP proportions & binomial distributions per feature

np<- autotyp.feature("NP_structure")
clause<- autotyp.feature("Clause_linkage")
num <- autotyp.feature("Numeral_classifiers")
areas<- autotyp.feature("Register")

NPs<- select(np, Glottocode,NPMarking, NPHeadlessness)
clausePosition<- select(clause, Glottocode, ClausePosition)
numClass<- select(num, Glottocode, NumClass.n)
autotypmeta<- select(areas, Glottocode, Area, Stock)
names(autotypmeta)[2]<- paste("area")
names(autotypmeta)[3]<- paste("affiliation")

autotype<- merge(NPs, clausePosition, by= "Glottocode")
autotyp1<- merge(autotype, numClass, by= "Glottocode")
autotyp_f <- merge(autotyp1, autotypmeta, by= "Glottocode")
names(autotyp_f)[1]<- paste("glottocode")
autotyp_f <- distinct(autotyp_f)


f_names<- list("NPMarking", "NPHeadlessness", "ClausePosition", "NumClass.n")

#function to sample pairs of languages from the same/different area
pair_area <- function(x, sample_size=100, same_area=TRUE) {
  same_area_comparisons <- c()
  
  while (length(same_area_comparisons) < sample_size){
    for(lang in x) {
      
      na.omit(x)
      sample_glot<- sample(x[,1], 2, replace = FALSE)
      
      lang1 <- x[x$glottocode == sample_glot[1],][1,]
      lang2 <- x[x$glottocode == sample_glot[2],][1,]
    
      if (same_area == TRUE){
        if (lang1["area"] == lang2["area"]){
          value<- lang1[,2] == lang2[,2]

          same_area_comparisons <- c(same_area_comparisons, value)
        }  
      } else {
        if (lang1["area"] != lang2["area"]){
          
          value<- lang1[,2] == lang2[,2]
          
          same_area_comparisons <- c(same_area_comparisons, value)
        }
      }
      if (length(same_area_comparisons) == 100){
        break
      }
      
    }
  }
  
  return(same_area_comparisons)
}

#function to sample pairs of languages from the same/different affiliation
pair_affiliation<- function(x, sample_size=100, same_affiliation=TRUE) {
  same_affiliation_comparisons <- c()
  
  while (length(same_affiliation_comparisons) < sample_size){
    for(lang in x) {
      
      na.omit(x)
      sample_glot<- sample(x[,1], 2, replace = FALSE)
      
      #print(sample_glot)
      
      lang1 <- x[x$glottocode == sample_glot[1],][1,]
      lang2 <- x[x$glottocode == sample_glot[2],][1,]
      
      
      if (same_affiliation == TRUE){
        if (lang1["affiliation"] == lang2["affiliation"]){
          
          value<- lang1[,2] == lang2[,2]
          
          same_affiliation_comparisons <- c(same_affiliation_comparisons, value)
        }  
      } else {
        if (lang1["affiliation"] != lang2["affiliation"]){
          
          
          value<- lang1[,2] == lang2[,2]
          
          same_affiliation_comparisons <- c(same_affiliation_comparisons, value)
        }
      }
      if (length(same_affiliation_comparisons) == 100){
        break
      }
      
      
    }
  }

  return(same_affiliation_comparisons)
}

#function to sample pairs of languages from the same area and affiliation
pair_both <- function(x, sample_size=100, same_both=TRUE) {
  same_both_comparisons <- c()
  
  while (length(same_both_comparisons) < sample_size){
    for(lang in x) {
      
      na.omit(x)
      sample_glot<- sample(x[,1], 2, replace = TRUE)
      
      lang1 <- x[x$glottocode == sample_glot[1],][1,]
      lang2 <- x[x$glottocode == sample_glot[2],][1,]
      
      if (same_both == TRUE){
        if (lang1["affiliation"] == lang2["affiliation"] && lang1["area"] == lang2["area"]){
          
          value<- lang1[,2] == lang2[,2]
          
          same_both_comparisons <- c(same_both_comparisons, value)
        }  
      } else {
        if (lang1["affiliation"] != lang2["affiliation"]  && lang1["area"] != lang2["area"]){
          
          value<- lang1[,2] == lang2[,2]
          
          same_both_comparisons <- c(same_both_comparisons, value)
        }
      }
      if (length(same_both_comparisons) == 100){
        break
      }
      
    }
  }
  
  return(same_both_comparisons)
}

feature <- c()
proportion_area <- c()
proportion_affiliation <- c()
proportion_areaf <- c()
proportion_affiliationf <- c()
proportion_both <- c()


prob_area <- list()
prob_affiliation <- list()
prob_areaf <- list()
prob_affiliationf <- list()
prob_both <- list()

for (n in f_names){
  
  outcome_area <- c()
  outcome_affiliation <- c()
  outcome_areaf <- c()
  outcome_affiliationf <- c()
  outcome_both <- c()
  
  
  temp_w<- select(autotyp_f, glottocode, n, area, affiliation)
  
  temp_w <- na.omit(temp_w)

  outcome_area <- pair_area(temp_w)
  outcome_affiliation <- pair_affiliation(temp_w)
  outcome_areaf <- pair_area(temp_w, same_area=FALSE)
  outcome_affiliationf <- pair_affiliation(temp_w, same_affiliation = FALSE)
  outcome_both <- pair_both(temp_w)
  
  feature <- c(feature, n)
  proportion_area <- c(proportion_area, mean(outcome_area))
  proportion_affiliation <- c(proportion_affiliation, mean(outcome_affiliation))
  proportion_areaf <- c(proportion_areaf, mean(outcome_areaf))
  proportion_affiliationf <- c(proportion_affiliationf, mean(outcome_affiliationf))
  proportion_both <- c(proportion_both, mean(outcome_both))
  
  prob_area[n] <- list(rbinom(100000, 100, proportion_area)/100)
  prob_affiliation[n] <- list(rbinom(100000, 100, proportion_affiliation)/100)
  prob_areaf[n] <- list(rbinom(100000, 100, proportion_areaf)/100)
  prob_affiliationf[n] <- list(rbinom(100000, 100, proportion_affiliationf)/100)
  prob_both[n] <- list(rbinom(100000, 100, proportion_both)/100)
  
}

prob_area_autotyp <- data.frame(prob_area)
prob_affiliation_autotyp <- data.frame(prob_affiliation)
prob_areaf_autotyp <- data.frame(prob_areaf)
prob_affiliationf_autotyp <- data.frame(prob_affiliationf)
prob_both_autotyp <- data.frame(prob_both)



#WALS proportions & binomial distributions per feature

f_names <- list("24a","27a","30a","32a","33a","37a","38a","41a","42a","43a","44a","46a","47a","48a","52a","53a","54a","57a","59a","63a","64a","65a","66a","67a","70a","71a","73a","81a","84a","85a","86a","87a","88a","89a","90a","91a","92a","93a","98a", "99a","101a","102a","106a","107a","112a","143a")

areas<- autotyp.feature("Register")
autotypmeta<- select(areas, Glottocode, Area, Stock)
names(autotypmeta)[1]<- paste("glottocode")
names(autotypmeta)[2]<- paste("area")
names(autotypmeta)[3]<- paste("affiliation")

#function to sample pairs of languages from the same/different area

pair_area <- function(x, sample_size=100, same_area=TRUE) {
  same_area_comparisons <- c()
  
  while (length(same_area_comparisons) < sample_size){
    for(lang in x) {

      sample_glot<- sample(x[,1], 2, replace = FALSE)
      
      lang1 <- x[x$glottocode == sample_glot[1],][1,]
      lang2 <- x[x$glottocode == sample_glot[2],][1,]
      
      if (same_area == TRUE){
        if (lang1["area"] == lang2["area"]){
          value<- lang1[,2] == lang2[,2]
          
          same_area_comparisons <- c(same_area_comparisons, value)
        }  
      } else {
        if (lang1["area"] != lang2["area"]){
          value<- lang1[,2] == lang2[,2]
          same_area_comparisons <- c(same_area_comparisons, value)
        }
      }
      if (length(same_area_comparisons) == 100){
        break
      }
    }
  }
  
  return(same_area_comparisons)
}


#function to sample pairs of languages from the same/different affiliation

pair_affiliation<- function(x, sample_size=100, same_affiliation=TRUE) {
  same_affiliation_comparisons <- c()
  
  while (length(same_affiliation_comparisons) < sample_size){
    for(lang in x) {

      sample_glot<- sample(x[,1], 2, replace = FALSE)
      
      lang1 <- x[x$glottocode == sample_glot[1],][1,]
      lang2 <- x[x$glottocode == sample_glot[2],][1,]
      
      if (same_affiliation == TRUE){
        if (lang1["affiliation"] == lang2["affiliation"]){
          value<- lang1[,2] == lang2[,2]
          
          same_affiliation_comparisons <- c(same_affiliation_comparisons, value)
        }  
      } else {
        if (lang1["affiliation"] != lang2["affiliation"]){
          value<- lang1[,2] == lang2[,2]
          
          same_affiliation_comparisons <- c(same_affiliation_comparisons, value)
        }
      }
      if (length(same_affiliation_comparisons) == 100){
        break
      }
    }
  }
  
  return(same_affiliation_comparisons)
}


#function to sample pairs of languages from the same area and affiliation

pair_both <- function(x, sample_size=100, same_both=TRUE) {
  same_both_comparisons <- c()
  
  while (length(same_both_comparisons) < sample_size){
    for(lang in x) {

      sample_glot<- sample(x[,1], 2, replace = TRUE)
      
      lang1 <- x[x$glottocode == sample_glot[1],][1,]
      lang2 <- x[x$glottocode == sample_glot[2],][1,]
      
      if (same_both == TRUE){
        if (lang1["affiliation"] == lang2["affiliation"] && lang1["area"] == lang2["area"]){
          value<- lang1[,2] == lang2[,2]
          
          same_both_comparisons <- c(same_both_comparisons, value)
        }  
      } else {
        if (lang1["affiliation"] != lang2["affiliation"]  && lang1["area"] != lang2["area"]){
          
          value<- lang1[,2] == lang2[,2]
          
          same_both_comparisons <- c(same_both_comparisons, value)
        }
      }
      if (length(same_both_comparisons) == 100){
        break
      }
      
    }
  }

  return(same_both_comparisons)
}


feature <- c()
proportion_area <- c()
proportion_affiliation <- c()
proportion_areaf <- c()
proportion_affiliationf <- c()
proportion_both <- c()

prob_area <- list()
prob_affiliation <- list()
prob_areaf <- list()
prob_affiliationf <- list()
prob_both <- list()


for (n in f_names){
  
  outcome_area <- c()
  outcome_affiliation <- c()
  outcome_areaf <- c()
  outcome_affiliationf <- c()
  outcome_both <- c()
  
  temp_w <- wals.feature(n)
  temp_wg <- merge(temp_w, autotypmeta, by= "glottocode")
  temp_w <- select(temp_wg, -wals.code, -latitude.x, -longitude.x, -language.x,  -latitude.y, -longitude.y, -iso)
  temp_w <- na.omit(temp_w)

  outcome_area <- pair_area(temp_w)
  outcome_affiliation <- pair_affiliation(temp_w)
  outcome_areaf <- pair_area(temp_w, same_area=FALSE)
  outcome_affiliationf <- pair_affiliation(temp_w, same_affiliation = FALSE)
  outcome_both <- pair_both(temp_w)
  
  print(mean(outcome_area))
  print(mean(outcome_affiliation))
  print(mean(outcome_areaf))
  print(mean(outcome_affiliationf))
  print(mean(outcome_both))
  
  feature <- c(feature, n)
  proportion_area <- c(proportion_area, mean(outcome_area))
  proportion_affiliation <- c(proportion_affiliation, mean(outcome_affiliation))
  proportion_areaf <- c(proportion_areaf, mean(outcome_areaf))
  proportion_affiliationf <- c(proportion_affiliationf, mean(outcome_affiliationf))
  proportion_both <- c(proportion_both, mean(outcome_both))
  
  prob_area[n] <- list(rbinom(100000, 100, proportion_area)/100)
  prob_affiliation[n] <- list(rbinom(100000, 100, proportion_affiliation)/100)
  prob_areaf[n] <- list(rbinom(100000, 100, proportion_areaf)/100)
  prob_affiliationf[n] <- list(rbinom(100000, 100, proportion_affiliationf)/100)
  prob_both[n] <- list(rbinom(100000, 100, proportion_both)/100)
  
}

prob_area_wals <- data.frame(prob_area)
prob_affiliation_wals <- data.frame(prob_affiliation)
prob_areaf_wals <- data.frame(prob_areaf)
prob_affiliationf_wals <- data.frame(prob_affiliationf)
prob_both_wals <- data.frame(prob_both)

colnames(prob_area_wals) <-  c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	"Nominal.Plurality",	"Definite.Articles",	"Indefinite.Articles",	"Distance.Contrasts.in.Demonstratives",	"Pronominal.and.Adnominal.Demonstratives",	"Third.Person.Pronouns.and.Demonstratives",	"Gender.Distinctions.in.Independent.Personal.Pronouns",	"Indefinite.Pronouns",	"Intensifiers.and.Reflexive.Pronouns", "Person.Marking.on.Adpositions",	"Comitatives.and.Instrumentals",	"Ordinal.Numerals",	"Distributive.Numerals",	"Position.of.Pronominal.Possessive.Affixes",	"Possessive.Classification",	"Noun.Phrase.Conjunction",	"Nominal.and.Verbal.Conjunction",	"Perfective.Imperfective.Aspect",	"The.Past.Tense",	"The.Future.Tense",	"The.Morphological.Imperative",	"The.Prohibitive",	"Optative",	"WordOrderSOV", "Order.of.Object..Oblique..and.Verb",	"Order.of.Adposition.and.Noun.Phrase",	"Order.of.Genitive.and.Noun",	"Order.of.Adjective.and.Noun",	"Order.of.Demonstrative.and.Noun",	"Order.of.Numeral.and.Noun",	"Order.of.Relative.Clause.and.Noun",	"Order.of.Degree.Word.and.Adjective",	"Position.of.Polar.Question.Particles",	"Position.of.Interrogative.Phrases.in.Content.Questions",	"Alignment.of.Case.Marking.of.Full.Noun.Phrases",	"Alignment.of.Case.Marking.of.Pronouns",	"Expression.of.Pronominal.Subjects",	"Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb")
colnames(prob_affiliation_wals) <-  c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	"Nominal.Plurality",	"Definite.Articles",	"Indefinite.Articles",	"Distance.Contrasts.in.Demonstratives",	"Pronominal.and.Adnominal.Demonstratives",	"Third.Person.Pronouns.and.Demonstratives",	"Gender.Distinctions.in.Independent.Personal.Pronouns",	"Indefinite.Pronouns",	"Intensifiers.and.Reflexive.Pronouns", "Person.Marking.on.Adpositions",	"Comitatives.and.Instrumentals",	"Ordinal.Numerals",	"Distributive.Numerals",	"Position.of.Pronominal.Possessive.Affixes",	"Possessive.Classification",	"Noun.Phrase.Conjunction",	"Nominal.and.Verbal.Conjunction",	"Perfective.Imperfective.Aspect",	"The.Past.Tense",	"The.Future.Tense",	"The.Morphological.Imperative",	"The.Prohibitive",	"Optative",	"WordOrderSOV", "Order.of.Object..Oblique..and.Verb",	"Order.of.Adposition.and.Noun.Phrase",	"Order.of.Genitive.and.Noun",	"Order.of.Adjective.and.Noun",	"Order.of.Demonstrative.and.Noun",	"Order.of.Numeral.and.Noun",	"Order.of.Relative.Clause.and.Noun",	"Order.of.Degree.Word.and.Adjective",	"Position.of.Polar.Question.Particles",	"Position.of.Interrogative.Phrases.in.Content.Questions",	"Alignment.of.Case.Marking.of.Full.Noun.Phrases",	"Alignment.of.Case.Marking.of.Pronouns",	"Expression.of.Pronominal.Subjects",	"Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb")
colnames(prob_areaf_wals) <- c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	"Nominal.Plurality",	"Definite.Articles",	"Indefinite.Articles",	"Distance.Contrasts.in.Demonstratives",	"Pronominal.and.Adnominal.Demonstratives",	"Third.Person.Pronouns.and.Demonstratives",	"Gender.Distinctions.in.Independent.Personal.Pronouns",	"Indefinite.Pronouns",	"Intensifiers.and.Reflexive.Pronouns", "Person.Marking.on.Adpositions",	"Comitatives.and.Instrumentals",	"Ordinal.Numerals",	"Distributive.Numerals",	"Position.of.Pronominal.Possessive.Affixes",	"Possessive.Classification",	"Noun.Phrase.Conjunction",	"Nominal.and.Verbal.Conjunction",	"Perfective.Imperfective.Aspect",	"The.Past.Tense",	"The.Future.Tense",	"The.Morphological.Imperative",	"The.Prohibitive",	"Optative",	"WordOrderSOV", "Order.of.Object..Oblique..and.Verb",	"Order.of.Adposition.and.Noun.Phrase",	"Order.of.Genitive.and.Noun",	"Order.of.Adjective.and.Noun",	"Order.of.Demonstrative.and.Noun",	"Order.of.Numeral.and.Noun",	"Order.of.Relative.Clause.and.Noun",	"Order.of.Degree.Word.and.Adjective",	"Position.of.Polar.Question.Particles",	"Position.of.Interrogative.Phrases.in.Content.Questions",	"Alignment.of.Case.Marking.of.Full.Noun.Phrases",	"Alignment.of.Case.Marking.of.Pronouns",	"Expression.of.Pronominal.Subjects",	"Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb")
colnames(prob_affiliationf_wals) <-  c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	"Nominal.Plurality",	"Definite.Articles",	"Indefinite.Articles",	"Distance.Contrasts.in.Demonstratives",	"Pronominal.and.Adnominal.Demonstratives",	"Third.Person.Pronouns.and.Demonstratives",	"Gender.Distinctions.in.Independent.Personal.Pronouns",	"Indefinite.Pronouns",	"Intensifiers.and.Reflexive.Pronouns", "Person.Marking.on.Adpositions",	"Comitatives.and.Instrumentals",	"Ordinal.Numerals",	"Distributive.Numerals",	"Position.of.Pronominal.Possessive.Affixes",	"Possessive.Classification",	"Noun.Phrase.Conjunction",	"Nominal.and.Verbal.Conjunction",	"Perfective.Imperfective.Aspect",	"The.Past.Tense",	"The.Future.Tense",	"The.Morphological.Imperative",	"The.Prohibitive",	"Optative",	"WordOrderSOV", "Order.of.Object..Oblique..and.Verb",	"Order.of.Adposition.and.Noun.Phrase",	"Order.of.Genitive.and.Noun",	"Order.of.Adjective.and.Noun",	"Order.of.Demonstrative.and.Noun",	"Order.of.Numeral.and.Noun",	"Order.of.Relative.Clause.and.Noun",	"Order.of.Degree.Word.and.Adjective",	"Position.of.Polar.Question.Particles",	"Position.of.Interrogative.Phrases.in.Content.Questions",	"Alignment.of.Case.Marking.of.Full.Noun.Phrases",	"Alignment.of.Case.Marking.of.Pronouns",	"Expression.of.Pronominal.Subjects",	"Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb")
colnames(prob_both_wals) <- c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	"Nominal.Plurality",	"Definite.Articles",	"Indefinite.Articles",	"Distance.Contrasts.in.Demonstratives",	"Pronominal.and.Adnominal.Demonstratives",	"Third.Person.Pronouns.and.Demonstratives",	"Gender.Distinctions.in.Independent.Personal.Pronouns",	"Indefinite.Pronouns",	"Intensifiers.and.Reflexive.Pronouns", "Person.Marking.on.Adpositions",	"Comitatives.and.Instrumentals",	"Ordinal.Numerals",	"Distributive.Numerals",	"Position.of.Pronominal.Possessive.Affixes",	"Possessive.Classification",	"Noun.Phrase.Conjunction",	"Nominal.and.Verbal.Conjunction",	"Perfective.Imperfective.Aspect",	"The.Past.Tense",	"The.Future.Tense",	"The.Morphological.Imperative",	"The.Prohibitive",	"Optative",	"WordOrderSOV", "Order.of.Object..Oblique..and.Verb",	"Order.of.Adposition.and.Noun.Phrase",	"Order.of.Genitive.and.Noun",	"Order.of.Adjective.and.Noun",	"Order.of.Demonstrative.and.Noun",	"Order.of.Numeral.and.Noun",	"Order.of.Relative.Clause.and.Noun",	"Order.of.Degree.Word.and.Adjective",	"Position.of.Polar.Question.Particles",	"Position.of.Interrogative.Phrases.in.Content.Questions",	"Alignment.of.Case.Marking.of.Full.Noun.Phrases",	"Alignment.of.Case.Marking.of.Pronouns",	"Expression.of.Pronominal.Subjects",	"Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb")

