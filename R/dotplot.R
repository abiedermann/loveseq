#' Returns a dotplot of fGSEA results
#' @param df: data.frame containing fgsea results
#' @param alpha: cutoff for adjp
#' @return ggplot object containing dotplot of results
dotplot <- function(df,title="",alpha=0.05){
  library(forcats)
  library(ggplot2)
  df$type <- "Upregulated"
  df$type[df$NES<0] = "Downregulated"

  geneRatio <- c()
  for(i in 1:dim(df)[1]){
    geneRatio <- c(geneRatio,length(df$leadingEdge[[i]])/df$size[i])
  }
  df$geneRatio <- geneRatio

  this.kegg.fgseaRes <- df[df$padj<alpha,]
  p <- ggplot(this.kegg.fgseaRes,aes(x=geneRatio,y=fct_reorder(pathway, geneRatio))) +
      geom_point(aes(size=size, color=this.kegg.fgseaRes$padj)) +
      theme_bw(base_size = 14) +
      scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1),labels=c("0","0.25","0.5","0.75","1"),limits=c(0,1))+
      scale_colour_gradient(limits=c(0, alpha), low="red") +
        labs(y=NULL,x="Fraction of genes in leading edge",size="Gene set size",color="Adjusted p-value") +
        ggtitle(title)
  p <- p + facet_grid(.~type)
  return(p)
}
