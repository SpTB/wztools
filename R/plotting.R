
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



