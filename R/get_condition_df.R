#' Generates condition matrix for DESeq2 given a replicate regex suffix
#' Column names of condition matrix are returned as idx (original df column
#' names) and value (1-hot-encoding)
get_condition_df <- function(df,suffix="\\.[0-9]$"){
  basenames <- colnames(df)
  basenames <- unique(gsub(suffix,"",basenames))

  cols <- colnames(df)
  cond <- rep(0,length(cols))
  for(i in 1:length(basenames)){
    cond[grepl(paste0("^",basenames[i]),cols)] = as.character(i)
  }
  cond <- data.frame(cols,cond)
  colnames(cond) <- c("idx","value")
  return(cond)
}

