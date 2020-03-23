#' Connect and parse UniProt information.
#'
#' This Function is used to plot location's frequency in the data of the accession/s in the chromosomes.
#'
#' @usage PlotSummaryInfo (ProteinDataObject,directorypath = NULL)
#'
#' @param ProteinDataObject input a Dataframe returned from GetNamesTaxa function
#'
#' @param directorypath path to save excel file containig results returened by the function.
#'
#' @author Mohmed Soudy and Ali Mostafa
#'
#' @export
PlotSummaryInfo <- function(ProteinDataObject,directorypath = NULL)
{
  ChromoCount <- ddply(ProteinDataObject, .(ProteinDataObject$Proteomes), nrow)
  ChromoCount %>%
    mutate(freq = percent(ChromoCount$V1 / sum(ChromoCount$V1))) -> ChromoCount

  ChromoSummary <- ggplot(ChromoCount, aes(x = ChromoCount$`ProteinDataObject$Proteomes`, y = ChromoCount$V1)) +
    geom_bar(fill = "#0073C2FF", stat = "identity") + xlab("Chromosomes") + ylab("frequency") +
    geom_text(aes(label = ChromoCount$freq), vjust = -0.3) + theme(axis.text.x = element_text(angle = 90 , hjust = 1 , vjust = 0.5))+
    theme_bw()

  plot(ChromoSummary)
 if(!is.null(directorypath))
 {
 write.csv(ChromoCount , paste0(directorypath , "/" , "Chromosomes Info Summary.csv"))
 ggsave(paste0(directorypath , "/"  , "Chromosoes Summary.jpeg") , plot = ChromoSummary , device = "jpeg" , dpi = 320, height = (max(ChromoCount$V1) + 1))

 }
}
