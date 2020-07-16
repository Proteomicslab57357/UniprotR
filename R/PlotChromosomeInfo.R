#' Connect and parse UniProt information.
#'
#' This Function is used to plot location's frequency in the data of the accession/s in the chromosomes.
#'
#' @usage PlotChromosomeInfo(ProteinDataObject,directorypath = NULL)
#'
#' @param ProteinDataObject input a Dataframe returned from GetNamesTaxa function
#'
#' @param directorypath path to save files returened by the function.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
PlotChromosomeInfo <- function(ProteinDataObject,directorypath = NULL)
{
  ProteinDataObject <- ProteinDataObject[!is.na(ProteinDataObject$Proteomes),]
  Chromosmoe_library <- ProteinDataObject %>% select("Proteomes" , "Organism")
  Chromosmoe_librarySplit <-  data.frame(do.call('rbind', strsplit(as.character(Chromosmoe_library$Proteomes),':',fixed=TRUE)))
  Chromosmoe_library <- cbind(Chromosmoe_library , Chromosmoe_librarySplit)
  Chromosmoe_dict <- select(Chromosmoe_library , "Organism" , "X1")
  Chromosmoe_dict <- unique(Chromosmoe_dict)


  ChromoCount <- ddply(ProteinDataObject, .(ProteinDataObject$Proteomes), nrow)
  ChromoCount %>%
    mutate(freq = percent(ChromoCount$V1 / sum(ChromoCount$V1))) -> ChromoCount
  ChromoCount <- na.omit(ChromoCount)

  Organism <-  data.frame(do.call('rbind', strsplit(as.character(ChromoCount$`ProteinDataObject$Proteomes`),':',fixed=TRUE)))
  OrganismName <- Organism$X1
  ChromoCount <- cbind(ChromoCount , OrganismName)
  identifier <- NULL
  for (i in 1:dim(ChromoCount)[1])
  {
    for (j in 1:dim(Chromosmoe_dict)[1])
    {
      if (ChromoCount$OrganismName[i] == Chromosmoe_dict$X1[j])
      {
        identifier <- c(identifier , as.character(Chromosmoe_dict$Organism[j]));
      }
    }
  }
  ChromoCount <- cbind(ChromoCount , identifier)
  ChromoCount <- cbind(ChromoCount , Organism$X2)

  Organism <- ChromoCount$identifier
  Chromosome <- ChromoCount$`ProteinDataObject$Proteomes`
  value <- ChromoCount$V1
  df <- cbind(as.character(Organism) , as.character(Chromosome))
  df <- cbind(df , value)
  colnames(df) <- c("Organism","Chromosome","value")
  df <- data.frame(df)
  df$value <- as.numeric(df$value)
  ChromoSummary <- ggplot(df %>% arrange(df$Organism , desc(df$value)) %>%
                            mutate(Chromosome=factor(Chromosome, levels=Chromosome)),
                          aes(x=Chromosome,y=value, fill = Organism)) +
    geom_bar(stat="identity", show.legend=FALSE , alpha = 0.7) + xlab("Chromosome") + ylab("Frequency")+
    facet_grid(. ~ Organism, scales="free_x", space="free_x") +
    theme_classic() +
    theme(text = element_text(size=14, face="bold", colour="black") , axis.text.x = element_text(angle = 90), panel.spacing=unit(0,"pt"),
          panel.border=element_rect(colour="grey50", fill=NA))
  if(!is.null(directorypath))
  {
    write.csv(ChromoCount , paste0(directorypath , "/" , "Chromosomes Info Summary.csv"))
    ggsave(paste0(directorypath , "/"  , "Chromosoes Summary.jpeg") , plot = ChromoSummary , device = "jpeg" , dpi = 320, height =  11 , width = 10)
    ggsave(paste0(directorypath , "/"  , "Chromosoes Summary.tiff") , plot = ChromoSummary , device = "tiff" , dpi = 320, height =  11 , width = 10)
  }
  plot(ChromoSummary)
}
