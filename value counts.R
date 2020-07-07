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



ane_df<- select(features, "latitude", "longitude", "Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	
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
                 "glottocode", "language")


ful_df<- rbind(wals1, ane_df, by=c("latitude", "longitude", "Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",	
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
ful_df$latitude<- as.numeric(ful_df$latitude)
ful_df$longitude<-as.numeric(ful_df$longitude)



anea_df<- select(features, "latitude", "longitude", "NPHeadlessness", "ClausePosition", "NumClass.n", "glottocode", "language")
df_anea<- merge(ane_df, anea_df)
  
np<- autotyp.feature("NP_structure")
clause<- autotyp.feature("Clause_linkage")
num <- autotyp.feature("Numeral_classifiers")
areas<- autotyp.feature("Register")

NPs<- select(np, Glottocode, NPHeadlessness)
clausePosition<- select(clause, Glottocode, ClausePosition)
numClass<- select(num, Glottocode, NumClass.n)
autotypmet<- select(areas, Glottocode, Language, Longitude, Latitude)

autotype<- merge(NPs, clausePosition, by= "Glottocode")
autotyp1<- merge(autotype, numClass, by= "Glottocode")
autotyp_f <- merge(autotyp1, autotypmet, by= "Glottocode")
names(autotyp_f)[1]<- paste("glottocode")
names(autotyp_f)[5]<- paste("language")
names(autotyp_f)[6]<- paste("longitude")
names(autotyp_f)[7]<- paste("latitude")
autotyp_f <- distinct(autotyp_f)
df_uni<- merge(wals1, autotyp_f)

full_df<- rbind(anea_df, autotyp_f, by=c("latitude", "longitude", "NPHeadlessness", "ClausePosition", "NumClass.n", "glottocode", "language"))
full_df$latitude<- as.numeric(full_df$latitude)
full_df$longitude<-as.numeric(full_df$longitude)


full_df$NPHeadlessness<- as.character(full_df$NPHeadlessness)
full_df$NPHeadlessness[full_df$NPHeadlessness==""] <- NA

full_df$ClausePosition<- as.character(full_df$ClausePosition)
full_df$ClausePosition[full_df$ClausePosition==""] <- NA


fdf<- merge(ful_df, full_df, by= c("latitude", "longitude", "glottocode", "language"))
df<- select(fdf, -"latitude", -"longitude", -"glottocode", -"language")


value.count <- function(x) {
  u <- unique.default(x)
  out <- list(n = u, freq = .Internal(tabulate(match(x, u), length(u))))
  class(out) <- "data.frame"
  attr(out, "row.names") <- seq_along(u)
  out
}

counts_world<- lapply(df, value.count)
counts_uni<- lapply(df_uni, value.count)
counts_anea<- lapply(df_anea, value.count)

