#
#
#
#' @title Perform calculation for Customer Satisfaction Index and Importance Performance Analysis
#' @description Measures Customer Satisfaction Index and Importance Performance Analysis from survey data.
#' @param files Input file from questionnaire. Only support Excel (\*.xls or \*.xlsx) file.
#' @param performance Character. Performance data sheet name in excel file
#' @param importance Character. Importance data sheet name in excel file
#' @return list
#'
#' @details \code{Result} contains percentage of suitability between performance and importance of a service or whatever you measure using quesionnaire, index of performance according to respondents and index for customer satisfactions. \code{Average} is overall average value of suitability, satisfaction and importance.
#'
#'
#' @import readxl
#'
#' @export
#' @examples \dontrun{
#' ic <- ipacsi(files)
#' ic}
#'

ipacsi <- function(files, performance = "performance", importance = "importance", firstrow = TRUE, firstcol = TRUE) {
  questionnaire <- lapply(list(performance, importance), function(i)read_excel(files, sheet = i))
  names(questionnaire) <- list(performance, importance)

  nq <- unlist(lapply(questionnaire, ncol))
  nr <- unlist(lapply(questionnaire, nrow))
  if(nq[1] != nq[2] | length(unique(nr)) != 1){
    stop("Number of questions (column) in performance and importance sheet does not equal.")
  }
  if(length(unique(nr)) == 1) nr <- unique(nr)
  if(length(unique(nq)) == 1) nq <- unique(nq)

  colsum <- lapply(questionnaire, 
                   function(i){
                     if(firstrow){
                          colSums(i[,-1], na.rm = TRUE)
                       } else {
                          colSums(i, na.rm = TRUE)
                       }
                     }
                   )
  colmean <- lapply(questionnaire, 
                    function(i){
                      if(firstrow){
                        colMeans(i[,-1], na.rm = TRUE)
                      } else {
                        colMeans(i, na.rm = TRUE)
                      }
                    }
                    )


  conformity <- colsum[[performance]]/colsum[[importance]]
  avg_conformity <- sum(colsum[[performance]], na.rm = TRUE)/sum(colsum[[importance]], na.rm = TRUE)
  avg_perf <- sum(colmean[[performance]])/nq
  avg_imp <- sum(colmean[[importance]])/nq

  result <- data.frame(Attribute = names(conformity),
                       Conformity = conformity,
                       Performance = colmean[[performance]],
                       Importance = colmean[[importance]])
  result$Attribute <- factor(result$Attribute, levels = names(conformity))
  avg <- data.frame(Conformity = avg_conformity,
                    Performance = avg_perf,
                    Importance = avg_imp)
  list(Result = result, Average = avg)
}

