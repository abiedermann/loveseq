#' Given a named vector (names=GQ and gene names, values=statistic for fgsea),
#' run fgsea and use path view to visualize statistically significant
#' pathways (padj < alpha). Genes can be in any format. Output pathview files
#' are written to the subdir directory.
run_kegg_fgsea_and_visualization <- function(input.data,alpha=0.05,
			            fgsea.result.fname='kegg_gsea_results.csv',
				    subdir=".",
                                    excluded.genes=c("Zeocin","P8","2KD1")){
  library(fgsea)
  library(pathview)

  # Filtering out experiment-specific genes that are not in the annotation file
  input.data = input.data[!(names(input.data) %in% excluded.genes)]

  # convert rownames to PAS format
  names(input.data) <- get_PAS_from_mixed(names(input.data))
  
  # Save current directory
  base.dir <- getwd()
  
  # Make the required working directory
  dir.create(subdir)
  setwd(subdir)
  # Creating a subdirectory for the other data output by pathview
  dir.create("./kegg_results")
  
  # Run fgsea on input.data
  kegg.fgseaRes <- fgsea(pathways=kegg.gmt$gsc,stats=input.data,minSize=5,nperm=100000)
  kegg.fgseaRes <- kegg.fgseaRes[order(kegg.fgseaRes$padj),]
  
  # Output fgsea results
  if(length(fgsea.result.fname) > 0){
    write.csv(kegg.fgseaRes[,c('pathway','pval','padj','ES','NES','nMoreExtreme','size')],fgsea.result.fname)
  }
  
  # Pulling pngs for each significant pathway found by fgsea
  for(i in 1:length(kegg.fgseaRes[which(kegg.fgseaRes$padj<alpha),]$pathway)){
    this.pathid <- kegg.pathinfo$pathway_ids[which(kegg.fgseaRes$pathway[i]==kegg.pathinfo$pathway_names)]
    print(this.pathid)
    pathview(gene.data = input.data, pathway.id = this.pathid, species = "ppa",
             gene.idtype="kegg",low="red",high="green",kegg.dir="./kegg_results")
  }
  
  # return to original working directory
  setwd(base.dir)
  return(kegg.fgseaRes)
}

