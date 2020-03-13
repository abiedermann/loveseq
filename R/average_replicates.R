#' This function averages columns in a dataframe which are identical except
#' for a given regex suffix.
average_replicates <- function(df,suffix="\\.[0-9]$"){
  names(df) <- gsub(suffix,'',names(df))
  grouped.df <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(df)), # for each unique column name
       function(col) rowMeans(df[names(df) == col]) # calculate row means
    )
  )
  return(grouped.df)
}

