library(lingtypology)
library(dplyr)
library(htmlwidgets)


wals1 <- wals.feature(c("24a","27a","30a","32a","33a","37a","38a","41a","42a","43a","44a","46a","47a","48a","52a","53a","54a","57a","59a","63a","64a","65a","66a","67a","70a","71a","73a","81a","84a","85a","86a","87a","88a","89a","90a","91a","92a","93a","98a", "99a","101a","102a","106a","107a","112a","143a"))
colnames(wals1)[4:49] <- c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	
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
wals1<- select(wals1, -wals.code)

anea_df<- select(features, "latitude", "longitude", "Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	
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
                 "Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb", "glottocode", "language")


full_df<- rbind(wals1, anea_df, by=c("latitude", "longitude", "Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	
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
                                 "Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb", 
                                 "glottocode", "language" ))
full_df$latitude<- as.numeric(full_df$latitude)
full_df$longitude<-as.numeric(full_df$longitude)



f_maps<- c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	
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

for(n in f_maps){
  
  print(n)
  
  mystring <- sprintf("C:\\Users/NourEfrat-Kowalsky/Desktop/code/Feature maps/%s.html", n)
  
  n_map<- map.feature(full_df$language, 
                       longitude = full_df$longitude, 
                       latitude = full_df$latitude, 
                       features = full_df[[n]], 
                       label = full_df$language, 
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

