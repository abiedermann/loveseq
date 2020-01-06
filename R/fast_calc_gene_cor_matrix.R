#' Given a matrix of l2cpm data, this function calculates gene-to-gene
#' Pearson correlation co-efficients and returns a matrixs with rows and 
#' columns ordered via hierarchical clustering.
#' @param df: dataframe with rows corresponding to gene names and columns
#'            corresponding to sample names. Values are l2cpm data.
#' @return list(gene.cors,this.hres): correlation data.frame with rows and columns ordered
#'                    via hierarchical clustering. this.hres = hierarchical clustering
#'                    object.
fast_calc_gene_correlation_matrix <- function(df,percent.var=0.99){
    library(fastcluster)
    library(parallelDist)
    library(PCAtools)
    # Dropping rows with zero variance
    this.df <- df[which(apply(df,1,var) > 0),]
    # Calculating gene-to-gene correlation matrix
    gene.cors <- cor(t(this.df),t(this.df))
    print("Running dimensionality reduction...")
    pca.res <- pca(gene.cors,center=T,scale=T)
    n.pcs <- min(which(cumsum(pca.res$variance)/sum(pca.res$variance)>percent.var))
    # Note: by default, parDist used the maximum number of cpu threads available
    print(paste0("Keeping ",as.character(n.pcs)," PCs..."))
    print("Calculating sample distances in PCA space and clustering...")
    this.hres <- fastcluster::hclust(parDist(gene.cors[,1:n.pcs]))
    gene.cors <- gene.cors[this.hres$order,this.hres$order]
    return(list(gene.cors,this.hres))
}

