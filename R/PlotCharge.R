#' Connect and parse UniProt information.
#'
#' This Function is used to plot proteins charge retrieved from "GetSequences" Function.
#'
#' @usage PlotCharge(SeqDataObjPath , directorypath = NULL)
#'
#' @param SeqDataObjPath Dataframe retrieved from UniprotR Function "GetSequences"
#'
#' @param directorypath path to save Output plot.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#' 
PlotCharge <- function(SeqDataObjPath, directorypath = NULL)
{
  seqdata <- select(SeqDataObjPath , "Sequence")
  result = aminoAcidProperties(seqdata , seq="Sequence")
  #Group data frame by charge
  ChargeGroup <- rep("Negative" , dim(result)[1])
  positive_index <- which(result$Sequence_aa_charge > 0)
  ChargeGroup[positive_index] <- "Positive"
  result <- cbind(result , ChargeGroup)
  
  #Get charge Sign ratios
  Chargecount <- table(sign(result$Sequence_aa_charge))
  if (length(Chargecount) == 1){
    Chargecount <- as.table(cbind(Chargecount , 0));
  }
  Chargeratio <- table(sign(result$Sequence_aa_charge))/dim(result)[1]*100
  if (length(Chargeratio) == 1){
    Chargeratio <- as.table(cbind(Chargeratio , 0));
  }
  #Construct Charge dataframe
  Chargedf = data.frame(x = c("Negative" , "Positive") , y = Chargeratio , z = Chargecount)
  #Charge plot
  Chargebarplot <- ggplot(result, aes(x=as.numeric(reorder(rownames(result) , result$Sequence_aa_charge)), y=result$Sequence_aa_charge, label=rownames(result))) +
    geom_bar(stat='identity', aes(fill=result$ChargeGroup)) + theme_classic() + theme(axis.title.x = element_blank()  , axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5))+ylab("Protein charge")+
    guides(fill=guide_legend(title="Groups"))+ggtitle("Sequence Charge") + scale_x_continuous(limits = c(0,  dim(result)[1]), expand = c(0, 0))
  Chargebarplot
  
  Chargeframeplot <- ggplot(Chargedf , aes(x = Chargedf$x , y = Chargedf$z.Freq))+
    geom_bar(stat = "identity" , aes(fill = Chargedf$y.Freq)) + geom_text(aes(label = paste0(as.character(round(Chargedf$y.Freq),2), "%")) , size = 4, vjust = -1)+theme_bw()+
    theme(legend.position = "none" , plot.title = element_text(hjust = 0.5))+xlab("Groups") + ylab("Protein count") + ggtitle("Sequence Charge") + scale_y_continuous(limits = c(0,  dim(result)[1]), expand = c(0, 1))
  
  ChargeTotal <- ggarrange(Chargeframeplot, Chargebarplot , nrow = 1 , ncol = 2, align = "hv")
  plot(ChargeTotal)
  if (!is.null(directorypath))
  {
    ggsave(plot = ChargeTotal , filename = paste0(directorypath ,"//" ,"Sequences charge.jpeg") , dpi = 320 , width = 11 , height = 10)
    ggsave(plot = ChargeTotal , filename = paste0(directorypath ,"//" ,"Sequences charge.tiff") , dpi = 320 , width = 11 , height = 10)
  }
}