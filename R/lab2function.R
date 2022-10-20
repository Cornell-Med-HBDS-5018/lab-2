#function 2
#Example: stats(mean, 'mean')
stats <- function(x, y){
  if(!is.element(y,
                 c("mean",
                   "median",
                   "sd"))){
    stop("Argument statistics should be either 'mean', 'median', 'sd'.") #check if input is included in mean, median, and sd
  }
  drg_summary <- drg %>%
    group_by(DRG.Definition) %>% #group by DRG codes
    summarise(y = x(Average.Medicare.Payments)) #calculate either the mean, median, or standard deviation
  colnames(drg_summary)[2] <- y #change column name into corresponding calculation
  return(drg_summary) #return summary
}

