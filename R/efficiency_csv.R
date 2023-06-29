efficiency_csv <- function(file) {
  dataset <- read.csv(file,
                      
                      # Note that the qTower program is giving European csv
                      # standards (columns separated by ; and decimals as ,)
                      skip = 19,  sep = ';', dec = ',')
  dataset[dataset == "" | dataset == " "] <- NA
  
  #Exclude unnecessary columns, make a dataset containing only the datat used
  dataset <- subset(dataset, select = -c(Well, X, Mean.Conc.,
                                         Std.Dev..Mean.Conc.))
  results_gene <- dataset %>% filter(!is.na(Gene))
  
  # Calculate 2 predicted Ct values -----------------------------------------
  # Ct_mean = slope*(log(Qty)) + intercept
  y1 <- slope*log10(results$Conc..Std.[1]) + intercept
  y2 <- slope*log10(results$Conc..Std.[5]) + intercept
  dCt <- y2 - y1
  dlogCN <- log10(results$Conc..Std.[5]) - log10(results$Conc..Std.[1])
  ratio <- dCt/dlogCN ## this is the slope of the linear regression
  efficiency <- 10^(-1/slope)
  return(efficiency)
  efficiency_percentage <- efficiency * 100 / 2
  efficiency_percentage
  
  }