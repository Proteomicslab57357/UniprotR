#' Connect and parse UniProt information.
#'
#' This Function is used to plot protein status in the data of the accession/s.
#'
#' @usage PlotproteinExist(ProteinDataObject,directorypath = NULL)
#'
#' @param ProteinDataObject input a Dataframe returned from GetMiscellaneous function
#'
#' @param directorypath path to save files returened by the function.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
PlotproteinExist <- function(ProteinDataObject , directorypath = NULL)
{
  Proteinexistence <- c("Evidence at protein level" , "Evidence at transcript level" , "Inferred from homology" , "Predicted" , "Uncertain")
  ProteinDataObject <- ProteinDataObject[!is.na(ProteinDataObject$Protein.existence),]
  ProteinDataObject$Protein.existence <- factor(ProteinDataObject$Protein.existence , levels = Proteinexistence)

  Exist <- ggplot(data = ProteinDataObject) +
    geom_bar(mapping = aes(x = ProteinDataObject$Protein.existence, fill = ProteinDataObject$Protein.existence)) +
    xlab("level of protein existence") + ylab("Protein Count") + guides(fill=guide_legend(title="Groups")) + coord_trans() +
    theme_bw()+theme(text = element_text(size=14, face="bold", colour="black"),axis.text.x = element_text(hjust = 1 , angle = 45))+
    scale_fill_brewer(palette="Blues" , direction = -1)

  plot(Exist)
  if(!is.null(directorypath))
  {
    ggsave(paste0(directorypath , "/"  , "Protein existence.jpeg") , plot = Exist , device = "jpeg" , dpi = 320, height =  11 , width = 10)
  }
}
