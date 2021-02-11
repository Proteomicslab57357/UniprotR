#' Connect and parse UniProt information.
#'
#' This Function is used to plot proteins acidity retrieved from "GetSequences" Function.
#'
#' @usage PlotAcidity(SeqDataObjPath , directorypath = NULL)
#'
#' @param SeqDataObjPath Dataframe retrieved from UniprotR Function "GetSequences"
#'
#' @param directorypath path to save Output plot.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#' 
PlotAcidity <- function(SeqDataObjPath, directorypath = NULL)
{
  seqdata <- select(SeqDataObjPath , "Sequence")
  result = aminoAcidProperties(seqdata , seq="Sequence")
  #Plot acidic vs Basic Groups
  AcidicBasic <- c(result$Sequence_aa_acidic , result$Sequence_aa_basic)
  AcidicBasicGroup <- rep("Acidic" , length(result$Sequence_aa_acidic))
  AcidicBasicGroup <- c(AcidicBasicGroup , rep("Basic" , length(result$Sequence_aa_basic)))
  AcidicBasicframe <- data.frame(x = AcidicBasic , y = AcidicBasicGroup)
  
  
  p <- ggplot(AcidicBasicframe, aes(x=AcidicBasicframe$y, y=AcidicBasicframe$x, fill=AcidicBasicframe$y)) +
    geom_violin(alpha = 0.3)+ geom_boxplot(width=0.1) +
    guides(fill=guide_legend(title="Groups"))+ xlab("Groups") + ylab("Hydrophobicity")+
    theme_bw() + ggtitle("Hydrophobicity") + theme(plot.title = element_text(hjust = 0.5))
  plot(p)
  if (!is.null(directorypath))
  {
    ggsave(plot = p , filename = paste0(directorypath ,"//" ,"Sequences acidity.jpeg") , dpi = 320 , width = 11 , height = 10)
    ggsave(plot = p , filename = paste0(directorypath ,"//" ,"Sequences acidity.tiff") , dpi = 320 , width = 11 , height = 10)
  }
}