require('ggplot2')

plot_cutoff_table <- function(cutoff_table){
  p <- ggplot(cutoff_table, aes(x=cutoff, y=fraction_smaller_than_cutoff, group=1)) +
    geom_line() +
    xlab('Cutoff') +
    ylab('Fraction of input > cutoff') +
    ggtitle('Cumulative percentage of input > cutoff') + theme_bw()
  return(p)
}
