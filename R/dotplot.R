#' Returns a dotplot of fGSEA results
#' @param df: data.frame containing fgsea results
#' @param alpha: cutoff for adjp
#' @return ggplot object containing dotplot of results
dotplot <- function(df,title="",alpha=0.05){
  library(forcats)
  df$type <- "upregulated"
  df$type[df$NES<0] = "downregulated"

  geneRatio <- c()
  for(i in 1:dim(df)[1]){
    geneRatio <- c(geneRatio,length(df$leadingEdge[[i]])/df$size[i])
  }
  df$geneRatio <- geneRatio

  this.kegg.fgseaRes <- df[df$padj<alpha,]
  p <- ggplot(this.kegg.fgseaRes,aes(x=geneRatio,y=fct_reorder(pathway, geneRatio))) +
      geom_point(aes(size=size, color=this.kegg.fgseaRes$padj)) +
      theme_bw(base_size = 14) +
        scale_colour_gradient(limits=c(0, alpha), low="red") +
        ylab(NULL) + labs(size="Gene set size") +
        ggtitle(title)
  p <- p + facet_grid(.~type)
  return(p)
}
