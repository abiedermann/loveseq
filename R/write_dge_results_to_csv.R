#' Writes annotated dataframe to csv file based on rownames
write_dge_results_to_csv <- function(df,file.name){
    this.annot <- get_gene_annotations(as.character(rownames(df)))

    write.csv(cbind(df,this.annot),file.name)
}

