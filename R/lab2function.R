#function 2
#Example: stats('mean')
library(tidyverse)
stats <- function(x){
  if(!is.element(x,
                 c("mean",
                   "median",
                   "sd"))){
    stop("Argument statistics should be either 'mean', 'median', 'sd'.") #check if input is included in mean, median, and standard deviation
  }
  drg_summary <- drg %>%
    group_by(DRG.Definition) %>% #group by DRG codes
    summarise(x = get(x)(Average.Medicare.Payments)) #calculate either the mean, median, or standard deviation
  colnames(drg_summary)[2] <- x #change column name into corresponding calculation
  return(drg_summary) #return summary
}


# Function 1 code below
## load in the libraries
library(readr)
library(ggplot2)
library(dplyr)
library(janitor)
library(stringr)
library(snakecase)
boxplotter <- function(payment_type){
  plot <- drg_data %>%
    clean_names() %>% # Convert names to snakecase first
    mutate(code = str_sub(drg_definition, 1, 3)) %>% # Extract the code from DRG
    ggplot( aes(x = code, y = get(to_snake_case(payment_type)), fill = code)) + # Create the boxplot
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "DRG Code", y = payment_type,
         title = paste0("Boxplot of ",  payment_type,
                        " by DRG Code")) +
    theme(legend.position = "none")

  return (plot) # Output the developed boxplot
}

boxplotter('Average Covered Charges')
