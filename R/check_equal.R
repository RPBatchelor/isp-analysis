

check_equal <- function(a, b, epsilon = epsilon){
  value <- abs(a-b)
  value <= epsilon
}