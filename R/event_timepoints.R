#' Given a numeric array containing only integers return the start and ending indices of all the possible continguous subsequences
#'
#' @param array the array to be input
#' @param m a positive integer
#'
#' @return Record, a list of tuples (x,y) where x and y are the starting and the ending indices of any possible continguous subsequence
.event_timepoints <- function(array, m){
  Record <- list()
  size <- length(array) # length of the array stored in size
  if (size == 0) { # check if the array is empty; return NA if true

    return(list())

  } else if (m == 1) { # check if m supplied is 1

    return(list(c(1,size))) # the indices simply refer to all

  } else {

    Pointer <- 2
    comparend <- array[Pointer-1] #initialisation condition
    threshold <- 1

    while(Pointer < size){
      check <- .event_timepoints.helper(array, comparend, Pointer, size)

      comparend <- check[1]
      Pointer <- check[2]
      threshold <- check[3]

      if (threshold >= (m-1)) {
        comparend_position <- which(array %in% comparend)
        result <- c(comparend_position, Pointer-1) # result is the position of the comparend and the pointer moved 1 step backward
        Record <- c(Record, list(result)) # append result to list
        comparend <- array[Pointer] # update position of the comparend
        Pointer <- Pointer + 1
      } else {
        comparend <- array[Pointer]
        Pointer <- Pointer + 1
      }
    }
    return(Record)
  }
}
#' helper function for comparison
#'
.event_timepoints.helper <- function(array, comparend, Pointer, size){
  threshold <- 1
  check <- my_iterator(seq_len(size))
  while(((comparend + call_iterator(check)) == array[Pointer]) & (Pointer <= size)) { # check if we are still in a continguous sequence
    Pointer <- Pointer + 1
    threshold <- threshold + 1
  }
  return(c(comparend, Pointer, threshold))
}
