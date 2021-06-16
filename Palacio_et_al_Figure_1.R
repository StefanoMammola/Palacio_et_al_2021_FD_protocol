## ------------------------------------------------------------------------
## 'A protocol for conducting trait-based analyses and maximize their reproducibility in ecology'
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
# 'R script to reproduce the analyses'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.0.3) and R studio (v. 1.4.1103)
# Authors: Stefano Mammola

# Loading R package -------------------------------------------------------

library("dplyr")   # A Grammar of Data Manipulation
library("ggplot2") # Create Elegant Data Visualisations Using the Grammar of Graphics
library("wosr")    # Clients to the 'Web of Science' and 'InCites' APIs

# Loading data on number of pubblication  ---------------------------------

#Dimension database
db_numbers  <- read.table(file = "/Users/stefanomammola/Desktop/WARNING ACCESSIBILITY/Database to provide/db_numbers.txt",header = TRUE)
db_numbers <- db_numbers[db_numbers$PY>1999 & db_numbers$PY<2021,]

# WoS queries -------------------------------------------------------------

# Data source: Web of Science, accessed on 10.6.2021
# [Helsinki, Google Chrome, macOS High Sierra 10.13.6]

#Setting sid
sid <- auth(NULL, password = NULL) #change with your WoS access

#Setting WoS collection
coll <-  c("SCI", "SSCI", "AHCI", "ISTP", "ISSHP","BSCI", "BHCI", "ESCI")

# Extracting N° publication by year ---------------------------------------
range_year <- c(2000:2020)

functional_diversity   <- c()
taxonomic_diversity    <- c()
phylogenetic_diversity <- c()

for (i in 1:length(range_year))  {
  
  query_1 <- query_wos(paste("TS = (\"functional diversity\") AND PY = (",range_year[i],")",sep=''), editions = coll, sid = sid)
  query_2 <- query_wos(paste("TS = (\"taxonomic diversity\") AND PY = (",range_year[i],")",sep=''), editions = coll, sid = sid)  
  query_3 <- query_wos(paste("TS = (\"phylogenetic diversity\") AND PY = (",range_year[i],")",sep=''), editions = coll, sid = sid)  
  
  functional_diversity   <- append(functional_diversity, query_1$rec_cnt)
  taxonomic_diversity    <- append(taxonomic_diversity, query_2$rec_cnt)
  phylogenetic_diversity <- append(phylogenetic_diversity, query_3$rec_cnt)
  
}

#Storing the result
db <- data.frame(year = rep(range_year,3),
                  Tot = rep(db_numbers$N,3),
                  N = c(functional_diversity,taxonomic_diversity,phylogenetic_diversity),
                  term = c(rep("functional diversity",length(range_year)),
                           rep("taxonomic_diversity",length(range_year)),
                           rep("phylogenetic_diversity",length(range_year))))

# Plotting ----------------------------------------------------------------

(p1 <- ggplot(db) +
    
    geom_line(aes(x=year, y=N/Tot, color=term),size=1.5,linetype = 1) + 
    
    scale_color_manual(values=c("orange","black","blue"),
                       labels = c( paste("Functional diversity [n= ",sum(functional_diversity),"]", sep=''),
                                   paste("Taxonomic diversity [n= ",sum(taxonomic_diversity),"]", sep=''),
                                   paste("Phylogenetic diversity [n= ",sum(phylogenetic_diversity),"]", sep=''))))+
    labs(x = NULL, 
         y = "Relative number of published papers in the Web of Science",
         caption = NULL)+
    
    scale_x_continuous(breaks = c(seq(from=min(range_year),to=max(range_year),by=5)))+ 
    
    theme_bw()+
    
    theme(
      legend.position = c(0.4, 0.7),
      legend.title = element_blank(),
      legend.text = element_text(size=10),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      panel.grid = element_blank(),
      plot.caption = element_text(size = 10, color = "gray30")
)