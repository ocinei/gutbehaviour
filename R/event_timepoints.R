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
#' Given a numeric containing only integers return the start and ending indices of all the possible continguous subsequences (including that of only length 1)
#'
#' @param array the array to be input
#'
#' @return Record, a list of tuples (x,y) where x and y are the starting and the ending indices of any possible continguous subsequence (including that of only length 1; in such case the indices are repeated)
#'
.event_timepoints_all <- function(array) {
  Record <- list()
  term_index <- length(array)
  if (length(array) == 1) {
    result <- c(1, 1)
    Record <- c(Record, list(result))
    return(Record)
  }
  else if (length(array) == 2){
    if (array[2] == array[1] + 1){
      result <- c(1,2)
      Record <- c(Record, list(result))
    }
    else {
      Record <- c(Record, list(c(1,1)), list(c(2,2)))
    }
    return(Record)
  }
  else {
    Pointer_a <- 1
    Pointer_b <- 2
    comparend <- array[Pointer_a]
    comparer <- array[Pointer_b]
    while (Pointer_a < term_index & Pointer_b <= term_index) {
      result_pointers <- .event_timepoints_helper_all(Pointer_a, Pointer_b, array, term_index)
      Pointer_a <- result_pointers[1]
      Pointer_b <- result_pointers[2]
      if (Pointer_a == Pointer_b) {
        result <- c(Pointer_a, Pointer_b)
        Record <- c(Record, list(result))
        Pointer_a <- Pointer_a + 1
        Pointer_b <- Pointer_b + 2
      } else {
        if ((Pointer_b + 1) == term_index) {
          result <- c(Pointer_a, Pointer_b)
          Record <- c(Record, list(result))
          result <- c(Pointer_b+1, Pointer_b+1)
          Record <- c(Record, list(result))
          return(Record)
        } else {
          result <- c(Pointer_a, Pointer_b)
          Record <- c(Record, list(result))
          Pointer_a <- Pointer_b + 1
          Pointer_b <- Pointer_b + 2
        }
      }
  }
  return(Record)
  }
}
#' helper function for .event_timepoints_all
.event_timepoints_helper_all <- function(Pointer_a, Pointer_b, array, term_index){
  counter <- my_iterator(seq_len(term_index))
  comparend <- array[Pointer_a]
  comparer <- array[Pointer_b]
  computed <- (comparend + call_iterator(counter))
  if (computed != comparer) {
    Pointer_b <- Pointer_a
    return(c(Pointer_a,Pointer_b))
  }
  else {
    Pointer_b <- Pointer_b + 1
    comparer <- array[Pointer_b]
    while ((comparend + call_iterator(counter)) == comparer) {
      Pointer_b <- Pointer_b + 1
      comparer <- array[Pointer_b]
      if (Pointer_b >= term_index) {
        if (((comparend + call_iterator(counter)) == comparer) | is.na(comparer)) {
          if (Pointer_b == term_index) {
            return(c(Pointer_a, Pointer_b))
          } else {
            return(c(Pointer_a, Pointer_b-1))
          }
        }
        else {
          return(c(Pointer_a, Pointer_b - 1))
        }
      }
      else {
        next
      }
    }
    return(c(Pointer_a, Pointer_b - 1))
  }
}
