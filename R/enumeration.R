permutations <- function(n, k) {
  factorial(n) / factorial(n - k)
}

combinations <- function(n, k) {
  factorial(n) / (factorial(n - k) * factorial(k))
}

#' @title Enumerations
#' @description Calculates the total number of enumerations, \cr given a superset of size '\bold{n}' and subsets of size '\bold{k}', \cr where \emph{enumeration} means: \cr \cr a) Permutation (\bold{isOrdered} = TRUE) \cr b) Combination (\bold{isOrdered} = FALSE)
#' @param n Number of elements in the whole set
#' @param k Number of elements in each subset
#' @param isOrdered Does the ordering matter?
#' @return Total number of enumerations
#' @details \describe{\item{~Permutation:}{\eqn{n! / (n - k)!}}\item{~Combination:}{\eqn{n! / [(n - k)! * k!]}}}
#' @section Warning:
#' The function does \emph{not} assume a default value for \bold{isOrdered}.
#' \cr Therefore, all three arguments are required in order to calculate the result.
#' @examples
#' ## Permutations with a 35 element set, and 3 elements per subset:
#' enumerations(35, 3, T)
#'
#' ## Combinations with a 255 element set, and 5 elements per subset:
#' enumerations(255, 5, FALSE)
#' @export
enumerations <- function(n, k, isOrdered) {
  if (isOrdered == TRUE) {
    permutations(n, k)
  }
  else {
    combinations(n, k)
  }
}
