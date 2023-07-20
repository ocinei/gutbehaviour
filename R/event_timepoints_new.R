#' helper functions for mapping index to sequence
map_index_to_seq <- function(index, seq){
  vec <- c(seq[index[1]], seq[index[2]])
  return(vec)
}
#' helper function for filter off elements that represent contiguous subsequences not long enough
#' @export
filter_not_long <- function(element, min_length = 0){
  length_subseq <- element[2] - element[1]
  if (length_subseq >= min_length){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
#' helper functions for finding the index of all the elements that represent the start and the end of contiguous subsequences
#' @export
all_contig_sequence <- function(sequence){
  size_of_seq = length(sequence)
  if (size_of_seq == 0){
    return(list())
  } else if (size_of_seq == 1){
    return(list(c(sequence[1],sequence[1])))
  } else {
      diff <- rep(1, size_of_seq)
      contig_sequence <- list()
      for (i in seq(2,size_of_seq)){
        diff[i] <- ((sequence[i] - sequence[i-1]) == 1)
      }
      map_indices = which(diff == 0)
      if (length(map_indices) == 0){
        contig_sequence <- list(c(1,size_of_seq))
      } else{
        diff_long <- length(map_indices) + 1
        for (i in seq_len(diff_long)){
          if (i == 1){
            contig_sequence[[i]] <- c(1,map_indices[i]-1)
          } else if (i == diff_long) {
            contig_sequence[[i]] <- c(map_indices[i-1],size_of_seq)
          } else {
            contig_sequence[[i]] <- c(map_indices[i-1],map_indices[i]-1)
          }
        }
      }
      Contigs <- lapply(contig_sequence, function(x){map_index_to_seq(x, sequence)})
    }
  return(Contigs)
}

