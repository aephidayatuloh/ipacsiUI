#
#
#
#' @title Plot for Customer Satisfaction Index and Importance Performance Analysis
#' @description Display Customer Satisfaction Index and Importance Performance Analysis from survey data using ggplot2 output.
#' @param ipacsi List. Output from \code{ipacsi} function.
#' @param legend.pos Character. Available value is \code{"none"}, \code{"right"}, \code{"bottom"}.
#' @param ... Argument to be passed to \code{ggplot2::theme()} function.
#' @return plot
#'
#'
#' @import ggplot2
#'
#' @export
#' @examples \dontrun{
#' ic <- ipacsi(files)
#' require(magrittr)
#' ic %>% quadplot()
#' }
#'

quadplot <- function(ipacsi, legend.pos = "right", ...){
  if(!is.list(ipacsi)) stop("'ipacsi' is not a list.")
  dt <- ipacsi$Result
  avg <- ipacsi$Average
  dt$Conformity <- round(dt$Conformity*100, 2)
  dt$Performance <- round(dt$Performance, 2)
  dt$Importance <- round(dt$Importance, 2)
  ggplot(data = dt, aes_string(x = "Performance", y = "Importance")) +
    geom_point(aes_string(color = "Attribute", size = "Conformity")) +
    geom_hline(yintercept = avg$Importance, linetype = 2, color = "coral") +
    geom_vline(xintercept = avg$Performance, linetype = 2, color = "coral") +
    geom_text(mapping = aes_string(label = "Attribute"), vjust = -0.9, size = 3) +
    xlim(floor(min(dt$Performance)), 5) +
    ylim(floor(min(dt$Importance)), 5) +
    labs(title = "Conformity of Performance and Importance (%)",
         x = "Performance Index",
         y = "Importance Index") +
    theme_light() +
    theme(legend.position = legend.pos, ...)
}
