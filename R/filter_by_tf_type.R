#' Get all relevant info pertaining to known TFs.
#' @return dataframe containing relevant info for each TF.
filter_by_tf_type <- function(df,domain){
  # Given: A list of mixed gene annotations (AOX1 and GQ annotations)
  # Return: A dataframe containing all relevant annotation info from the master annotation file, with row order identical to
  #         the input list
  tf.annot <- get_tf_annotations()
  tf.annot <- tf.annot[which(tf.annot$DBDs==domain),]

  return(df[rownames(tf.annot),])
}
