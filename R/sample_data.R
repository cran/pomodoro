#' @title Sample data for analysis.
#'
#' A dataset containing information of access to credit.
#'
#' @format A \code{data_frame} with 53940 rows and 10 variables:
#' \describe{
#'   \item{x1}{hhid, household id number}
#'      \item{x2}{swgt, survey weight}
#'      \item{x3}{region, 3 factor level, west, east, and center}
#'       \item{x4}{No.Loan, if the household has no loan}
#'       \item{x5}{Formal, if the household has formal loan}
#'       \item{x6}{Both, if the household has both loan}
#'       \item{x7}{Informal, if the household has informal loan}
#'        \item{x8}{sex, if the household has male}
#'   \item{y1}{Loan.Type, 4 factor level type of the loan}
#'   \item{y2}{multi.level, 2 factor level if the household has access to loan or not}
#'   ...
#' }
"sample_data"
