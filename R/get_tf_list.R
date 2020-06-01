#' Get all relevant info pertaining to genes in a gene list.
#' @param this.list: A list of mixed gene names and GQ accessions
#' @return dataframe containing relevant info for each gene.
get_tf_list <- function(){
  # Given: A list of mixed gene annotations (AOX1 and GQ annotations)
  # Return: A dataframe containing all relevant annotation info from the master annotation file, with row order identical to
  #         the input list
  annot <- annotation
  tf.annot <- annot[which(annot$DBDs!=""),]
  tf.list <- get_mixed_from_GQ(rownames(tf.annot))
  return(tf.list)
}
