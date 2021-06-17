## ------------------------------------------------------------------------
## 'A protocol for conducting trait-based analyses and maximize their reproducibility in ecology'
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
# 'R script to reproduce the analyses'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.0.3) and R studio (v. 1.4.1103)
# Authors: Stefano Mammola

# Loading R package -------------------------------------------------------

library("dplyr")     # A Grammar of Data Manipulation
library("ggplot2")   # Create Elegant Data Visualisations Using the Grammar of Graphics
library("gridExtra")
library("wosr")      # Clients to the 'Web of Science' and 'InCites' APIs

# Set working directory ---------------------------------------------------

setwd("") #change me

# Loading data on number of pubblication  ---------------------------------

#Dimension database
db_numbers  <- read.csv(file = "/Users/stefanomammola/Desktop/Facundo FD protocol/dimension.csv",
                        sep = ";", header = TRUE)

db_numbers <- db_numbers[db_numbers$year > 1989 & db_numbers$year < 2021,] #selecting the range of year

# WoS queries -------------------------------------------------------------

# Data source: Web of Science, accessed on 10.6.2021
# [Helsinki, Google Chrome, macOS High Sierra 10.13.6]

#Setting sid
sid <- auth(NULL, password = NULL) #change with your WoS access

#Setting WoS collection
coll <-  c("SCI", "SSCI", "AHCI", "ISTP", "ISSHP","BSCI", "BHCI", "ESCI")

# Extracting N° publication by year ---------------------------------------
range_year <- c(1990:2020)

functional_diversity   <- c()
taxonomic_diversity    <- c()
phylogenetic_diversity <- c()

#Extracting WoS data 
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
                  Tot = rep(db_numbers$publications,3),
                  N = c(functional_diversity,taxonomic_diversity,phylogenetic_diversity),
                  term = c(rep("functional diversity",length(range_year)),
                           rep("taxonomic_diversity",length(range_year)),
                           rep("phylogenetic_diversity",length(range_year))))

# Plotting ----------------------------------------------------------------

(p1 <- ggplot(db) +
   
   geom_line(aes(x=year, y=N, color=term),size=1.5,linetype = 1) + 
   
   scale_color_manual(values=c("orange", "black", "grey70"),
                      labels = c( paste("Functional diversity [n= ",sum(functional_diversity),"]", sep=''),
                                  paste("Phylogenetic diversity [n= ",sum(phylogenetic_diversity),"]", sep=''),
                                  paste("Taxonomic diversity [n= ",sum(taxonomic_diversity),"]", sep='')))+
   labs(x = NULL, 
        y = "Number of published papers",
        title = "A",
        subtitle =  "Absolute values")+
   
   scale_x_continuous(breaks = c(seq(from=min(range_year),to=max(range_year),by=5)))+ 
   
   scale_y_continuous(breaks = c(0,500,1000,1500,2000,2500),
                      labels = c("0","500","1000","1500","2000","2500"))+
   
   theme_bw()+
   
   theme(
     legend.position = c(0.4, 0.7),
     legend.title = element_blank(),
     legend.text = element_text(size=10),
     plot.title = element_text(color="black", size=14, face="bold"),
     axis.title = element_text(size = 12),
     axis.text.x = element_text(size = 11),
     axis.text.y = element_text(size = 11),
     panel.grid = element_blank(),
     plot.caption = element_text(size = 10, color = "gray30")))

(p2 <- ggplot(db) +
    
    geom_line(aes(x=year, y=N/Tot, color=term),size=1.5,linetype = 1) + 
    
    scale_color_manual(values=c("orange", "black", "grey70"),
                       labels = c( paste("Functional diversity [n= ",sum(functional_diversity),"]", sep=''),
                                   paste("Taxonomic diversity [n= ",sum(taxonomic_diversity),"]", sep=''),
                                   paste("Phylogenetic diversity [n= ",sum(phylogenetic_diversity),"]", sep='')))+
  labs(x = NULL, 
       title = "B",
       subtitle = paste( "Relative to the annual number of published papers"),
       y = "Relative number of published papers")+
  
    scale_x_continuous(breaks = c(seq(from=min(range_year),to=max(range_year),by=5)))+ 
    
    theme_bw()+theme(
      legend.position = 'none',
      legend.title = element_blank(),
      legend.text = element_text(size=10),
      plot.title = element_text(color="black", size=14, face="bold"),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      panel.grid = element_blank(),
      plot.caption = element_text(size = 10, color = "gray30"))
)



plots <- list(p1,p2) ; grobs <- list() ; widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)

for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}

pdf("/Users/stefanomammola/Desktop/Figure_1.pdf", width = 10, height = 4)
do.call("grid.arrange", c(grobs, nrow = 1, ncol = 2))
dev.off()
