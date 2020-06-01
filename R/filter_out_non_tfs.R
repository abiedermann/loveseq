#' Get all relevant info pertaining to known TFs.
#' @return dataframe with only TFs in rows
filter_out_non_tfs <- function(df){
  # Given: A list of mixed gene annotations (AOX1 and GQ annotations)
  # Return: A dataframe containing all relevant annotation info from the master annotation file, with row order identical to
  #         the input list
  tf.df <-  df[which(as.character(rownames(df)) %in% get_tf_list()),]

  return(tf.df)
}
