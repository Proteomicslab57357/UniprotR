#' Connect and parse UniProt information.
#'
#' This Function is used to plot different taxas found of the accessions.
#'
#' @usage PlotProteinTaxa(ProteinDataObject , directorypath = NULL)
#'
#' @param ProteinDataObject input a Dataframe of proteins as rownames.
#'
#' @param directorypath path to save excel file containig results returened by the function.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
PlotProteinTaxa <- function(ProteinDataObject , directorypath = NULL)
{
  ChromoCount <- ddply(ProteinDataObject, .(ProteinDataObject$Organism), nrow)
  ChromoCount %>%
    mutate(freq = percent(ChromoCount$V1 / sum(ChromoCount$V1))) -> ChromoCount

  TaxaPlot <- ggplot(ChromoCount , aes(x = reorder(ChromoCount$`ProteinDataObject$Organism` , ChromoCount$V1) , y = ChromoCount$V1)) +
    geom_bar(fill = "#0073C2FF", stat = "identity" , alpha = 0.7) + xlab("Organsims") + ylab("frequency") + theme(axis.text.x  = element_text(angle = 90 , hjust = 1 , vjust = 0.5) , axis.title.y = element_blank() , axis.text.y = element_blank()) + xlab("Organism Accession")+
    geom_text(aes(label = ChromoCount$freq), vjust = -0.3) + theme(axis.text.x = element_text(angle = 90 , hjust = 1 , vjust = 0.5))
  plot(TaxaPlot)
  print(ChromoCount)
  if (!is.null(directorypath)){
  ggsave(paste0(directorypath, "//" ,"Proteins Taxonomy.jpeg") ,plot = TaxaPlot , device = "jpeg" , dpi = 320 , width =  10 , height = 12)
  }
}
