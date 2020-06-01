#' Get all relevant info pertaining to known TFs.
#' @return dataframe containing relevant info for each TF.
get_tf_annotations <- function(){
  # Given: A list of mixed gene annotations (AOX1 and GQ annotations)
  # Return: A dataframe containing all relevant annotation info from the master annotation file, with row order identical to
  #         the input list
  annot <- get_gene_annotations(get_tf_list())

  return(annot)
}
