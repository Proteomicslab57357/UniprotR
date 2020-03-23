#' Connect and parse UniProt information.
#'
#' This Function is used to plot protein scores in the data of the accession/s.
#'
#' @usage PlotproteinScore (ProteinDataObject,directorypath = NULL)
#'
#' @param ProteinDataObject input a Dataframe returned from GetMiscellaneous function
#'
#' @param directorypath path to save files returened by the function.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
PlotproteinScore <- function(ProteinDataObject,directorypath = NULL)
{
  ProteineScore <- c("1 out of 5" , "2 out of 5" , "3 out of 5" , "4 out of 5" , "5 out of 5")
  ProteinDataObject$Annotation <- factor(ProteinDataObject$Annotation , levels = ProteineScore)

  Annotation <- ggplot(data = ProteinDataObject) +
    geom_bar(mapping = aes(x = ProteinDataObject$Annotation, fill = ProteinDataObject$Annotation))+
    xlab("Annotation score") + ylab("Protein Count") + guides(fill=guide_legend(title="Annotation")) + coord_trans() +
    theme_bw()+theme(text = element_text(size=17, face="bold", colour="black"),axis.text.x = element_text(vjust=2))+
    scale_fill_brewer(palette="Blues")
  plot(Annotation)

  if(!is.null(directorypath))
  {
    ggsave(paste0(directorypath , "/"  , "Protein annotaion.jpeg") , plot = Annotation , device = "jpeg" , dpi = 320, height =  11 , width = 10)
    ggsave(paste0(directorypath , "/"  , "Protein annotaion.tiff") , plot = Annotation , device = "tiff" , dpi = 320, height =  11 , width = 10)

  }
}
