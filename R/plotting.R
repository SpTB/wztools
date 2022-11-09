
#' Set GG options
#'
#' @import ggplot2
#' @param base letter size
#' @param line_wid line width
#'
#' @return
#' @export
#'
setGGoptions <- function(base=10, line_wid=1) {

  ggplot2::theme_set(theme_classic(base_size = base) +
              theme(strip.background = element_blank()) +
              theme(axis.ticks      = element_line(color='black'),
                    #axis.text       = element_blank(),
                    axis.text = element_text(color='black'),
                    axis.title = element_blank(),
                    axis.line = element_line(colour = 'black', size = line_wid),
                    #strip.text.x = element_blank(),
                    legend.position = 'none')
  )

}



#' Calculate sd's for each group parameter
#'
#' @param mu_list named list of group-level  parameters (names should start with 'mu_')
#' @param sim_data simulated datasets with varying levels of parameter sets
#' @return
#' @export
#'
#' @examples
get_sim_mu_sds <- function(mu_list, sim_data) {
  sums=list()
  for (mu in seq_along(1:length(mu_list))) {
    grouping_col = paste0(names(mu_list)[mu],'_true')
    dots=lapply(grouping_col, as.symbol)
    sums[[mu]] = sim_data |> group_by(subjID, .dots=dots) |>
      dplyr::summarise(acc=mean(acc,na.rm=T)) |>
      dplyr::group_by(.dots=dots) %>% dplyr::summarise(accuracy=mean(acc), sd=sd(acc, na.rm=T))
  }
  return(sums)
}




#' Interatively plots task accuracies based on different levels of simulated mu parameters
#'
#' @param mu_list named list of group-level  parameters (names should start with 'mu_')
#' @param sim_data simulated datasets with varying levels of parameter sets
#'
#' @return
#' @export
#'
#' @examples
plot_mu_on_acc <-function(mu_list, sim_data) {
  sums = get_sim_mu_sds(mu_list, sim_data)
  grouping_cols = lapply(paste0(names(mu_list),'_true'), as.symbol)
  for (mu in seq_along(1:length(mu_list))) {
    p=sim_data |>
      group_by(subjID, .dots=as.symbol(paste0(names(mu_list)[[mu]],'_true'))) |>
      dplyr::summarise(accuracy=mean(acc,na.rm=T)) |>
      ggplot(aes(y=accuracy, x=as.factor(get(grouping_cols[[mu]])))) +
      geom_violin() +
      geom_point(position = position_jitter(.1), alpha=.1) +
      geom_pointrange(data=sums[[mu]],aes(ymin=accuracy-sd, ymax=accuracy+sd, color=as_factor(get(grouping_cols[[mu]]))), size=1.2,)+
      ylab('Accuracy') +
      xlab(names(mu_list)[[mu]])
    print(p)
  }
}
