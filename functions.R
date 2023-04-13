# Your functions can go here. Then, when you want to call those functions, run
# `source('R/functions.R')` within a code block in the RMarkdown notebook.

#── Check NTCs ─────────────────────────
# Upload data set and delete unneeded information define a function to read in and process each file, here filter for NTC is added
process_csv <- function(file) {
  dataset <- read.csv(file,
                      # Note that the qTower program is giving European csv standards (columns separated by ; and decimals as ,)
                      skip = 19,  sep = ';', dec = ',')
  dataset[dataset == "" | dataset == " "] <- NA
  
  #Exclude unnecessary columns, make a dataset containing only the datat used
  dataset <- subset(dataset, select = -c(Well, X, Mean.Conc., Std.Dev..Mean.Conc.))
  results_gene <- dataset %>% filter(!is.na(Gene))
  
  NTC <- filter(results_gene, results_gene$`Sample.type` =="NTC") 
}


#── Filter and plot all data ─────────────────────────
plot_csv <- function(file) {
  dataset <- read.csv(file,
                      
                      # Note that the qTower program is giving European csv standards (columns separated by ; and decimals as ,)
                      skip = 19,  sep = ';', dec = ',')
  dataset[dataset == "" | dataset == " "] <- NA
  
  #Exclude unnecessary columns, make a dataset containing only the datat used
  dataset <- subset(dataset, select = -c(Well, X, Mean.Conc., Std.Dev..Mean.Conc.))
  results_gene <- dataset %>% filter(!is.na(Gene))
  
  ggplot(results_gene, 
         aes(x=Sample.name, y=Mean.Ct)) + 
    labs(title= file) +
    geom_point() +
    geom_errorbar(aes(ymin=Mean.Ct-Std.Dev..Ct, ymax=Mean.Ct+Std.Dev..Ct), width=.2,
                  position=position_dodge(0.05)) 
}



#── Regression analysis, visualisatio and calculation of efficiency ─────────────────────────
fit_csv <- function(file) {
  dataset <- read.csv(file,
                      
                      # Note that the qTower program is giving European csv standards (columns separated by ; and decimals as ,)
                      skip = 19,  sep = ';', dec = ',')
  dataset[dataset == "" | dataset == " "] <- NA
  
  #Exclude unnecessary columns, make a dataset containing only the datat used
  dataset <- subset(dataset, select = -c(Well, X, Mean.Conc., Std.Dev..Mean.Conc.))
  results_gene <- dataset %>% filter(!is.na(Gene))
  
  #── Based on the graph, select dilutions that fit the standard curve
  results <- results_gene %>% filter(Sample.name%in% c("Std01", "Std02","Std03", "Std04","Std05", "Std06", "Std07", "Std08"))
  
  #── Adding a linear regression
  ggplot(results,
         aes(x = Conc..Std. , y = Mean.Ct)) +
    geom_point() +
    geom_errorbar(aes(ymin=Mean.Ct-Std.Dev..Ct, ymax=Mean.Ct+Std.Dev..Ct), width=.2,
                  position=position_dodge(0.05)) +
    scale_x_continuous(trans = 'log10') +
    stat_smooth(method = "lm",
                geom = "smooth")
  
  #── Getting linear regression parameters
  regression_mean <- lm(Mean.Ct ~ log10(Conc..Std.) , data = results)
  summary(regression_mean)
  
  #──  Calculate the efficiency 
  intercept <- as.numeric(regression_mean$coefficients[1])
  slope <- as.numeric(regression_mean$coefficients[2])
  
  #──  Calculate 2 predicted Ct values
  # Ct_mean = slope*(log(Qty)) + intercept
  y1=slope*log10(results$Conc..Std.[1]) + intercept
  y2=slope*log10(results$Conc..Std.[5]) + intercept
  dCt=y2 - y1
  dlogCN= log10(results$Conc..Std.[5]) - log10(results$Conc..Std.[1])
  ratio = dCt/dlogCN ## this is the slope of the linear regression
  efficiency = 10^(-1/slope)
  efficiency
  efficiency_percentage = efficiency * 100 / 2
  efficiency_percentage
  
  label_efficiency <- "Efficiency: "
  label_efficiency <- paste0(label_efficiency, as.character(format(efficiency, digit=4)))
  
  label_efficiency_percentage <- "Efficiency: "
  label_efficiency_percentage <- paste0(label_efficiency_percentage, as.character(format(efficiency_percentage, digit=4)), "%")
  
  label_r<-"R-squared: "
  label_r <- paste0(label_r, as.character(format(summary(regression_mean)$r.squared), digit=3))
  
  label_slope<-"Slope: "
  label_slope <- paste0(label_slope, as.character(format(ratio, digit=4)))
  
  ggplot(results, 
         aes(x = Conc..Std. , y = Mean.Ct)) + 
    labs(title= file) +
    geom_point(color = "black",  size = 2) +
    geom_errorbar(aes(ymin=Mean.Ct-Std.Dev..Ct, ymax=Mean.Ct+Std.Dev..Ct), width=.2,
                  position=position_dodge(0.05), color = "gray") +
    theme_prism(base_size = 14)  +
    theme(aspect.ratio = 0.8) +
    scale_shape_prism() + 
    scale_x_continuous(trans = 'log10') +
    stat_smooth(method = "lm",
                geom = "smooth", color = "#FF9999", size=0.75) +
    annotate("text", x = 0.06, y = 28, label = label_r) + 
    annotate("text", x = 0.06, y = 30, label = label_slope) + 
    annotate("text", x = 0.06, y = 26, label = label_efficiency) +
    annotate("text", x = 0.06, y = 24, label = label_efficiency_percentage)
}

