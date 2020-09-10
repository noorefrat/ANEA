df <- read.csv("C:/Users/NourEfrat-Kowalsky/Desktop/code/pRDA/test.set.csv", encoding="UTF-8") #both anea and reference data
#df <- read.csv("C:/Users/NourEfrat-Kowalsky/Desktop/code/pRDA/anea.data.csv", encoding="UTF-8") # only anea
#df <- read.csv("C:/Users/NourEfrat-Kowalsky/Desktop/code/pRDA/reference.data.csv", encoding="UTF-8") #only reference


value.count <- function(x) {
  u <- unique.default(x)
  out <- list(n = u, freq = .Internal(tabulate(match(x, u), length(u))))
  class(out) <- "data.frame"
  attr(out, "row.names") <- seq_along(u)
  out
}

counts<- lapply(df, value.count) #counts of all data
counts<- lapply(counts, na.omit) #take out all missing values so they don't influence the variable proportions
counts
capture.output(summary(counts), file = "C:\\Users/NourEfrat-Kowalsky/Desktop/code/pRDA\\counts.csv")



write.csv(counts, "C:\\Users/NourEfrat-Kowalsky/Desktop/code/pRDA\\counts.csv")
