#' Connect and parse UniProt information.
#'
#' This Function is used to plot protein status in the data of the accession/s.
#'
#' @usage PlotproteinStatus (ProteinDataObject,directorypath = NULL)
#'
#' @param ProteinDataObject input a Dataframe returned from GetMiscellaneous function
#'
#' @param directorypath path to save files returened by the function.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
PlotproteinStatus <- function(ProteinDataObject,directorypath = NULL)
{
  Status <- ggplot(data = ProteinDataObject) +
    geom_bar(mapping = aes(x = ProteinDataObject$Status, fill = ProteinDataObject$Status)) +
    xlab("Status") + ylab("Protein Count") + guides(fill=guide_legend(title="Groups")) + coord_trans() +
    theme_bw()+theme(text = element_text(size=17, face="bold", colour="black"),axis.text.x = element_text(vjust=2))+
    scale_fill_brewer(palette="Blues" , direction = -1)


  if(!is.null(directorypath))
  {
    ggsave(paste0(directorypath , "/"  , "Protein Status.jpeg") , plot = Status , device = "jpeg" , dpi = 320, height =  11 , width = 10)
    ggsave(paste0(directorypath , "/"  , "Protein Status.tiff") , plot = Status , device = "tiff" , dpi = 320, height =  11 , width = 10)
  }
}
