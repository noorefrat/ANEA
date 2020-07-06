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

reference <- merge(wals, autotyp, all = T)







