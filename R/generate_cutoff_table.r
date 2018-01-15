require('ggplot2')

generate_cutoff_table <- function(input, decimals = 1){
  input_min = min(input)
  input_max = max(input)
  input_range = input_max - input_min
  input_range_step = (1 / (10^decimals))
  floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
  ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
  cutoff  = seq(from = floor_dec(input_min, level=decimals), to = ceiling_dec(input_max, level =decimals), by = input_range_step)
  output = data.frame(cutoff)
  output$n_small_than_cutoff <- lapply(output$cutoff, function(x) sum(input < x))
  output$n_equal_or_greater_than_cutoff <- lapply(output$cutoff, function(x) sum(input >= x))
  output$n_equal_or_greater_than_cutoff <- unlist(output$n_equal_or_greater_than_cutoff)
  output$total_n <- length(input)
  output$fraction_equal_or_greater_than_cutoff <-  output$n_equal_or_greater_than_cutoff / output$total_n
  output$fraction_smaller_than_cutoff <-  1 - (output$n_equal_or_greater_than_cutoff / output$total_n)
  output <- output[sum(output$fraction_equal_or_greater_than_cutoff == 1) :nrow(output),]
  output <- output[1:which(output$n_equal_or_greater_than_cutoff == 0)[1],]
  output <- output[ , -which(names(output) %in% c("total_n"))]
  return(output)
}

# cutoff_table <- generate_cutoff_table(preds$model_2_pred)

plot_cutoff_table <- function(cutoff_table){
  p <- ggplot(cutoff_table, aes(x=cutoff, y=fraction_smaller_than_cutoff, group=1)) + 
    geom_line() + 
    xlab('Cutoff') + 
    ylab('Fraction of input > cutoff') +
    ggtitle('Cumulative percentage of input > cutoff') + theme_bw()
  return(p)
}