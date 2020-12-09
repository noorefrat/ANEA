
setwd("C:/Users/NourEfrat-Kowalsky/Desktop/code")
#data<- read.csv("C:/Users/NourEfrat-Kowalsky/Desktop/code/DATA/wordorder.csv", encoding="UTF-8")
data<- read.csv("C:/Users/NourEfrat-Kowalsky/Desktop/code/DATA/data.csv", encoding="UTF-8")

library(brms)
library(tidyverse)
library(haven)
library(ROCR)
library(devtools)
library(sjstats)
library(modelr)
library(tidybayes)

###########################
#correct
ex.prior <- c(prior_string("normal(0,1)", class="b"))
fit <- brm(
  formula= WordOrderSOV ~ area + (1|family),
  family= categorical (link="logit"),
  prior= ex.prior,
  data= data,
  iter = 2000,
  cores=4,
  control = list(adapt_delta = 0.9)
)

print(fit)
plot(fit)
summary(fit)
exp(fixef(fit)[,-2])

################################


#fnames<- list("Alignment.of.Case.Marking.of.Full.Noun.Phrases",	"Alignment.of.Case.Marking.of.Pronouns",	"Comitatives.and.Instrumentals",	"Noun.Phrase.Conjunction",	"Nominal.and.Verbal.Conjunction",	"Definite.Articles",	"Indefinite.Articles",	"Expression.of.Pronominal.Subjects",	"Indefinite.Pronouns",	"Third.Person.Pronouns.and.Demonstratives",	"Distance.Contrasts.in.Demonstratives",	"Pronominal.and.Adnominal.Demonstratives",	"Intensifiers.and.Reflexive.Pronouns",	"Gender.Distinctions.in.Independent.Personal.Pronouns",	"Systems.of.Gender.Assignment",	"Gender.n",	"Negative.Morphemes",	"Nominal.Plurality",	"Ordinal.Numerals",	"Distributive.Numerals",	"Order.of.Adjective.and.Noun",	"Order.of.Adposition.and.Noun.Phrase",	"Order.of.Degree.Word.and.Adjective",	"Order.of.Demonstrative.and.Noun",	"Order.of.Genitive.and.Noun",	"Order.of.Negative.Morpheme.and.Verb",	"Order.of.Numeral.and.Noun",	"Order.of.Object..Oblique..and.Verb",	"Order.of.Relative.Clause.and.Noun",	"Passive.Constructions",	"Reciprocal.Constructions",	"Optative",	"The.Prohibitive",	"The.Morphological.Imperative",	"Person.Marking.on.Adpositions",	"Position.of.Interrogative.Phrases.in.Content.Questions",	"Position.of.Polar.Question.Particles",	"Possessive.Classification",	"Position.of.Pronominal.Possessive.Affixes",	"Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication",	"The.Future.Tense",	"The.Past.Tense",	"Perfective.Imperfective.Aspect",	"Verbal.Person.Marking",	"WordOrderSOV")

fnames<- list("WordOrderSOV","Alignment.of.Case.Marking.of.Full.Noun.Phrases","Alignment.of.Case.Marking.of.Pronouns")


#flists <- list( brmsformula(WordOrderSOV ~ area + family) , brmsformula(Alignment.of.Case.Marking.of.Full.Noun.Phrases ~ area + family) )
flists <- list( brmsformula(Alignment.of.Case.Marking.of.Full.Noun.Phrases ~ area + family) )


#feature <- c()
regression_results <- c()

ex.prior <- c(prior_string("normal(0,1)", class="b"))

for (l in flists){

  fit<- brm(
    formula = l,
    family= categorical (link="logit"),
    prior= ex.prior,
    data= data,
    iter = 2000,
    cores=4,
    control = list(adapt_delta = 0.9)
  )
  
  extracted <- fit[grep("area", rownames(fit)), ]
  
  regression_results<- c(regression_results, extracted)
}


print(fit[12])
fit_df <- fit[12]
print(typeof(fit_df))
print(as.data.frame(fit_df$fit)[0])

print(as.data.frame(fit))

print(get_variables(fit))




library(bayesplot)
posterior <- as.array(fit)
dim(posterior)
dimnames(posterior)
color_scheme_set("red")
mcmc_intervals(posterior)
mcmc_areas(
  posterior, 
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

#tring to plot only the relevant coef
plot(fit, type = "credibleinterval", coef = "muOVS_area")
coefplot(fit)

m<-fixef(fit)
coef_m <- coef(m)
coefficients(fit)

stanplot(fit, 
         type = "areas",
         prob = 0.95)


stanplot(fit, 
         type = "areas",
         prob = 0.95,
         transformations = "exp") +
  geom_vline(xintercept = 1, color = "grey")

