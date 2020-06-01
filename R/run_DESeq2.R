#' Runs DESeq2, given your dataframe, conditions matrix, and comparison numbers
#' @param df: data.frame containing counts data
#' @param cond: data.frame representing the condition matrix. Column 1 is
#'              sample names corresponding to column names in df, column 2
#'              is 1-hot-encoding of each condition
#' @param comp: vector with two values, corresponding to the conditions from
#'              column 2 of cond that you want to compare. The resulting
#'              comparison is comp(1)/comp(2)
#' @param alpha: adjusted p-value threshold for DESeq2 analysis
#' @param lfcThreshold: log fold change threshold for DESeq2 analysis
#' Returns:
#' res: List containing DESeq2 results with and without the thresholds. The
#'      first should be used for differential gene analysis, the second should
#'      be used as input for gene set analysis.
run_DESeq2 <- function(df, cond, comp, alpha=0.05, lfcThreshold=1.5){
    library(DESeq2)
    # Ensure both columns of cond are factors
    cond[[1]] <- as.factor(cond[[1]])
    cond[[2]] <- as.factor(cond[[2]])

    # Subsetting larger dataframe and conditions matrix to parts relevant
    # for this comparison
    this.cond <- cond[cond[[2]] %in% comp,]
    this.df <- df[,as.character(this.cond[[1]])]


    # Running DESeq2
    this.des <-DESeqDataSetFromMatrix(this.df,colData=this.cond,design=~value)
    this.des <- DESeq(this.des)

    # Applying lfc and alpha thresholds to find DEGs
    this.res <- results(this.des,lfcThreshold=lfcThreshold,alpha=alpha,
                        contrast=c(colnames(cond)[2],as.character(comp[1]),
                                   as.character(comp[2])))
    # Relaxing thresholds for the sake of gene set analysis
    this.gsa_input <- results(this.des,lfcThreshold=0,alpha=0.9999999,
               contrast=c(colnames(cond)[2],as.character(comp[1]),as.character(comp[2])))

    # order results by Wald statistic
    this.dge_results <- data.frame(na.omit(this.res[order(this.res$stat),]))
    this.gsa_input <- data.frame(na.omit(this.gsa_input[order(this.gsa_input$stat),]))
    return(list(this.dge_results,this.gsa_input))
}



