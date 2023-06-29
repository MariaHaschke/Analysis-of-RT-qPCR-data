# Your functions can go here. Then, when you want to call those functions, run
# `source('R/functions.R')` within a code block in the RMarkdown notebook.


# Check NTCs --------------------------------------------------------------
# Upload data set and delete unneeded information define a function to read in 
# and process each file, here filter for NTC is added.
# Note that the qTower program is giving European csv standards (columns 
# separated by ; and decimals as ,)

process_csv <- function(file) {
  dataset <- read.csv(file,
            skip = 19, sep = ";", dec = "," )
  dataset[dataset == "" | dataset == " "] <- NA

# Exclude unnecessary columns, make a dataset containing only the data used.
  
dataset <- subset(dataset, select = -c(Well, X, Mean.Conc., 
                                       Std.Dev..Mean.Conc.))
results_gene <- dataset %>% filter(!is.na(Gene))
NTC <- filter(results_gene, results_gene$`Sample.type` == "NTC")
}


# # Filter and plot all data ------------------------------------------------

plot_csv <- function(file) {
  dataset <- read.csv(file,

                      # Note that the qTower program is giving European csv
                      # standards (columns separated by ; and decimals as ,)
                      skip = 19,  sep = ';', dec = ',')
  dataset[dataset == "" | dataset == " "] <- NA

#Exclude unnecessary columns, make a dataset containing only the datat used
dataset <- subset(dataset, select = -c(Well, X, Mean.Conc.,
                                       Std.Dev..Mean.Conc.))
results_gene <- dataset %>% filter(!is.na(Gene))

  ggplot(results_gene,
          aes(x = Sample.name, y = Mean.Ct)) +
          labs(title = file) +
          geom_point() +
          geom_errorbar(aes(ymin = Mean.Ct - Std.Dev..Ct,
                            ymax = Mean.Ct + Std.Dev..Ct),
                        width = .2,
                  position = position_dodge(0.05))
}

