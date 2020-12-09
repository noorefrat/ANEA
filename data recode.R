#!/usr/bin/env Rscript --vanilla


library(tidyverse)
library(testthat)


# read in the WALS language catalogue, unfortunately there is no dedicated file for this
wals_languages <- read.csv("wals-dataset/wals.named.data.csv", na.strings=c(""," ","NA","\n"))
expect_false(any(duplicated(wals_languages)))
expect_false(any(duplicated(wals_languages$wals_code)))
names(wals_languages)<- gsub("X","", names(wals_languages))
names(wals_languages)<- gsub("\\."," ", names(wals_languages))
which(colnames(wals_languages) == "65A Perfective Imperfective Aspect")
names(wals_languages)[50]<-paste("65A Perfective/Imperfective Aspect")

# read in the WALS sources
wals_source_data<-wals_languages 

# load the list of recodings
recode_patterns <- read.csv("recode-patterns.csv", stringsAsFactors=FALSE)

# extract the features that we don't need to recode
retained_wals_features <- filter(recode_patterns, is.na(recode.pattern))$wals.fname
recode_patterns <- filter(recode_patterns, !is.na(recode.pattern))

##########################################################################
## Take out features that we don't have ##
discard<-c(setdiff(retained_wals_features, names(wals_source_data)))
discard_indeces <- c()

# Remove the features from retained_wals_features
for(i in 1:length(retained_wals_features)){
  n = retained_wals_features[i]
  if(n %in% discard){
    discard_indeces <- c(discard_indeces, i)
  }
}
retained_wals_features <- retained_wals_features[-discard_indeces]

# Remove the features from recode_patterns
row_indeces<-c()
for(n in discard){
  local_indeces <- c(which(grepl(n, recode_patterns$wals.fname)))
  for(i in local_indeces){
    row_indeces <-c(row_indeces, i)
  }
}
recode_patterns <- recode_patterns[-row_indeces,]
#print(recode_patterns$wals.fname)
##########################################################################


#looking for duplicates in new.fnames- expecting none. if there are any, print 'Duplicated feature names:' and what they are.
expect_false(any(duplicated(recode_patterns$new.fname)), info=paste0(
  "Duplicated feature names:\n", 
  paste0("  ", recode_patterns$new.fname[ duplicated(recode_patterns$new.fname) ], collapse="\n")
))

recoded_wals_fnames <- c(retained_wals_features, recode_patterns$new.fname) #all the feature names: old (that aren't going to be recoded) and new

expect_false(any(duplicated(retained_wals_features)), info=paste0(
  "Duplicated retained feature names:\n", 
  paste0("  ", retained_wals_features[duplicated(retained_wals_features)], collapse="\n")
))

#we expect to find all the old WALS features in the source data - if not, print which ones aren't in there (with setdiff: what is in x but not in y)
expect_true(all(retained_wals_features %in% names(wals_source_data)), info=paste0(
  "Feature not found in WALS:\n", 
  paste0("  ", setdiff(retained_wals_features, names(wals_source_data)), collapse="\n")
))

# recode all the patterns
wals_recoded <- rowwise(recode_patterns) %>% do({
  cat("Processing ", .$new.fname, "(", .$wals.fname, " recoded as ", .$recode.pattern, ")\n", sep="")
  
  # check that the original variable is present in wals
  expect_true(.$wals.fname %in% names(wals_source_data)[-1])
  original_data <- as.character(wals_source_data[[.$wals.fname]])
  
  # make a table of original values
  expected_levels <- unlist(strsplit(.$wals.levels, "\n"))
  expected_levels <- data.frame(
    i = as.integer(gsub("^([0-9])+.+$", "\\1", expected_levels)),
    level = gsub("^[0-9]+\\.? +", "", expected_levels),
    stringsAsFactors=FALSE
  )
  
  expect_true(all(!is.na(expected_levels$i)))
  expect_true(all(!is.na(expected_levels$level)))
  
  # make sure that the WALS values are what we have in the table
  # NEW LINE
  print(typeof(original_data))
  print(original_data)
  #print(original_data$wals.fname)
  #original_data<-na.omit(original_data$wals.fname)
  
  
  #expect_true(setequal(expected_levels$level, na.omit(original_data)), info=
  #              paste0("Expected:\n", paste0("  ", (expected_levels$level), collapse="\n"), "\n",
  #                     "Got:\n",  paste0("  ", (unique(original_data)), collapse="\n"))
  #)
  
  # parse the recoding pattern
  
  recoding_groups <- unlist(strsplit(.$recode.pattern, "-"))
  recoding_groups <- strsplit(recoding_groups, "/") %>% lapply(as.integer)
  
  # sanity checks
  expect_true(length(recoding_groups)>1) # must have at least 2 recoding groups
  expect_true(all(!is.na(unlist(recoding_groups)))) # can't have NA's
  expect_true(all(unlist(recoding_groups) %in% expected_levels$i)) # must correspond to wals values
  expect_false(any(duplicated(unlist(recoding_groups)))) # can't have any duplicates
  
  # build the recodign table
  recoded_levels <- unlist(strsplit(.$new.levels, "\n"))
  expect_true(length(recoding_groups)==length(recoded_levels)) # must have at least 2 recoding groups
  
  recoded_levels <- bind_rows(mapply(recoded_levels, recoding_groups, FUN=function(value, ii) {
    data.frame(i = ii, new_level=as.character(value), stringsAsFactors=FALSE)
  }, SIMPLIFY=FALSE))
  
  level_table <- full_join(expected_levels, recoded_levels, by="i")
  
  # sanity checks
  expect_true(all(!is.na(level_table$level)))
  
  # recode the data
  recoded_data <- level_table$new_level[match(original_data, level_table$level)]
  
  data.frame(
    feature = .$new.fname, 
    wals_code = wals_source_data$glottocode, 
    value = recoded_data,  
    stringsAsFactors=FALSE)  
}) %>%
  spread(feature, value)


#  add the non-recoded variables
retained_data <- wals_source_data[c("Language.Code", retained_wals_features)]
wals_recoded <- full_join(wals_recoded, retained_data)

# and merge it with the language list
wals_recoded <- full_join(wals_languages, wals_recoded)

# check that all variable names are present
expect_true(setequal(names(wals_recoded)[-c(1:ncol(wals_languages))], recoded_wals_fnames))


# run the languae cleanup script
source("R/fix-languages.R")

# save the data
writeLines(names(wals_recoded))
write.csv(wals_recoded, "wals-recoded46.csv", row.names=FALSE)



