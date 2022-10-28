#' Brute Force
#'
#' @param x - is a data frame that should contain two variables in space i(0,1,2...N): w - weight, and v - value. Theese should be positive values only.
#' @param W - is the maximum weight for the knapsack. Should be a positive integer.
#'
#' @return maximum value for weight
#' @export
#'
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
#'                               v <- runif(n = n, 0, 10000))
brute_force_knapsack <- function(x, W){
  if(!is.data.frame(x)){stop("x is not data frame")}
  if(length(x) > 2){stop("only two variables allowed")}
  if(any(x < 0)){stop("only positive values is allowed")}

  w <- x$w
  v <- x$v
  n <- nrow(x)
  limit_combinations <- (2^n)-1
  result_v <- 0

  for (i in 1:limit_combinations) {
    sum_w <- 0
    sum_v <- 0
    elements <- c()
    bin_rep <- intToBits(i)
    for (j in 1:length(bin_rep)) {
      if(bin_rep[j] == T){
        sum_w <- sum_w + w[j]
        sum_v <- sum_v + v[j]
        elements <- c(elements,j)
      }
    }
    if(sum_v > result_v & sum_w <= W){
      result_v <- sum_v
      result_e <- elements
    }
  }

  result <- list('value' = round(result_v), 'elements' <- result_e)
  return(result)
}


#' Greedy
#'
#' @param x - is a data frame that should contain two variables in space i(0,1,2...N): w - weight, and v - value. Theese should be positive values only.
#' @param W - is the maximum weight for the knapsack. Should be a positive integer.
#'
#' @return maximum value for weight
#' @export
#'
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
#'                               v <- runif(n = n, 0, 10000))
greedy_knapsack <- function(x, W){
  n <- nrow(x)
  w <- x$w
  v <- x$v
  ratio <- v/w

  ratio <- sort(ratio, decreasing = T, index.return = T)
  index <- ratio[[2]]
  w_sorted <- w[index]
  v_sorted <- v[index]

  pack_value <- 0
  pack_weight <- 0
  elemts <- c()
  i <- 1

  while (pack_weight <= W & i < n) {
    if(pack_weight + w_sorted[i] < W){
      pack_value <- pack_value + v_sorted[i]
      pack_weight <- pack_weight + w_sorted[i]
      elemts <- append(elemts, index[i])
      i <- i + 1
    }else{
      i <- i + 1
      next
    }

  }
  return_list <- list('value' = round(pack_value), 'elements' = elemts)
  return(return_list)
}

#' knapsack_objects
#'
#' @description - Loads an example data frame to use with the functions of the knapsack.problem package. w is made by sample(1:400, 2000, T) with seed(42) and v is a sample from the uniform distribution of the same size.
#' @return - an example data frame for the knapsack.problem package
#' @export
#'
knapsack_objects <- {set.seed(42)
  data.frame(w = sample(1:4000, size = 2000, replace = TRUE),
             v = runif(n = 2000, 0, 10000))}
