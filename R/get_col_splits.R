#' Given a matrix of samples and genes, this returns a matrixs with rows and 
#' columns ordered via hierarchical clustering.
#' @param df: dataframe with rows corresponding to gene names and columns
#'            corresponding to sample names. Values are l2cpm data.
#' @return splits: vector of names and cluster IDs to use in ComplexHeatmap
get_col_splits <- function(df,nclusts){
    library(fastcluster)
    library(parallelDist)
    this.hres <- fastcluster::hclust(parDist(t(as.matrix(df))))

    library(hash)
    clusts <- cutree(this.hres,nclusts)

    this.clusts <- clusts[this.hres$order]

    initial.nums <- hash(unique(this.clusts),1:nclusts)

    this.clusts <- sapply(this.clusts,function(x){return(initial.nums[[as.character(x)]])})
    splits <- as.factor(as.numeric(this.clusts))
    return(splits)
}

