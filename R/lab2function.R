## load the data
load('data/drg_data.RData')

#function 2

#' Statistics over all of the DRG codes for average Medicare payments
#'
#' This function calculates statistics over all of the DRG codes for average Medicare payments.
#'
#' @param x  a string for type of statistics calculated
#'
#' @return Statistics for average Medicare payments
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#'
#' @examples
#' stats('mean')
#'
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


# Function 1 code below

#' Boxplot of payments by DRG code
#'
#' This function makes a boxplot of payments by DRG code.
#'
#' @param payment_type a string for type of payments
#'
#' @return A boxplot of payments by DRG code
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate
#' @importFrom stringr str_sub
#' @importFrom snakecase to_snake_case
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#'
#' @examples
#' boxplotter('Average Covered Charges')
#'
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


