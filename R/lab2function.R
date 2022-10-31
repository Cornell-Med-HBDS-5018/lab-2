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
  drg_summary <- drg_data %>%
    group_by(`DRG Definition`) %>% #group by DRG codes
    summarise(x = get(x)(`Average Medicare Payments`)) #calculate either the mean, median, or standard deviation
  colnames(drg_summary)[2] <- x #change column name into corresponding calculation
  return(drg_summary) #return summary
}
