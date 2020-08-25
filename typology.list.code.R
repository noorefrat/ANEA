typology.list <- readRDS("~/ANEA/typology.list.RDS")
my_data <- read.csv('~/ANEA/features.csv') 

df.vector <- c()
for(u in colnames(my_data)[-c(1:7, 75, 76)]) df.vector <- c(df.vector, which(grepl(pattern = u, names(typology.list))))

typology.df <- do.call(rbind, lapply(typology.list[df.vector],  function(df) {
  df$data = as.character(df$data) 
  return(df)}))

#arrange the data in language rows and column features. 
#Some languages have multiple feature values 
#(e.g. 'amel1241' and 'autotyp$Clause_linkage$ClausePosition' has 4, 3x fixed:pre-main and once flexible-relational.
#With values_fn, we sample randomly one of those.

typology.matrix <- typology.df %>% pivot_wider(names_from = variable.ID, values_from = data, 
                                               values_fn =function(x) sample(x, size = 1))

cols_info <- c(1:41)
typ.miss <- as.matrix(1*is.na(typology.matrix[-which(duplicated(typology.matrix$glottocode)), -cols_info])) 
pheatmap(t(typ.miss),cluster_rows=T, cluster_cols=F, legend = FALSE, cex = .9) 



anea.miss = as.matrix(1*is.na(anea[-which(duplicated(anea$language)),cols_info])) 
rownames(anea.miss) <- anea$type[-which(duplicated(anea$language))]
rownames(anea.miss) <- anea$language[-which(duplicated(anea$language))]
pheatmap(t(anea.miss),cluster_rows=T, cluster_cols=F, legend = FALSE, cex = .9) 
             
             
        
       
    