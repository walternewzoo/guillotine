#' @export
plot_cutoff_table <- function(cutoff_table){
  p <- ggplot2::ggplot(cutoff_table, ggplot2::aes(x=cutoff, y=fraction_smaller_than_cutoff, group=1)) +
    ggplot2::geom_line() +
    ggplot2::xlab('Cutoff') +
    ggplot2::ylab('Fraction of input > cutoff') +
    ggplot2::ggtitle('Cumulative percentage of input > cutoff') + ggplot2::theme_bw()
  return(p)
}
