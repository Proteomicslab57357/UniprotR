#' Connect and parse UniProt information.
#'
#' This Function is used to plot Gene ontolgy summary in the data of the accession/s.
#'
#' @usage plotGoannotation(ProteinDataObject,directorypath = NULL)
#'
#' @param ProteinDataObject input a Dataframe returned from GetProteinGOInfo function
#'
#' @param directorypath path to save files returened by the function.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
plotGoannotation <- function(ProteinDataObject,directorypath = NULL)
{
  #Get Biologica process Data
  BiologicalDF <- Goparse(ProteinDataObject , 3)
  #Plot top 5
  BiologicalDF <- BiologicalDF[1:10,]

  #Get molecuar function
  MolecularDF <- Goparse(ProteinDataObject , 4)
  #Plot top 5
  MolecularDF <- MolecularDF[1:10,]

  #Get cellular component
  CellularDF <- Goparse(ProteinDataObject , 5)
  #Plot top 5
  CellularDF <- CellularDF[1:10,]

  group <- rep("Biological process" , 10)
  group <- c(group , rep("Molecular function" , 10))
  group <- c(group , rep("Subcelluar localization" , 10))
  GoOntolgy <- rbind(BiologicalDF , MolecularDF)
  GoOntolgy <- rbind(GoOntolgy , CellularDF)
  GoOntolgy <- cbind(GoOntolgy , group)

  individual <- GoOntolgy$Goterm
  group <- GoOntolgy$group
  value <- GoOntolgy$Frequences
  # Create dataset
  data <- cbind(as.character(individual) , as.character(group))
  data <- cbind(data , as.character(value))
  data <- data.frame(data)
  colnames(data) <- c("individual" , "group" , "value")

  data = data %>% arrange(group, value)

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=3
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$group=rep(levels(data$group), each=empty_bar)
  data=rbind(data, to_add)
  data=data %>% arrange(group)
  data$id=seq(1, nrow(data))
  id <- data$id
  # Get the name and the y position of each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  hjust <- label_data$hjust
  # prepare a data frame for base lines
  base_data=data %>%
    group_by(group) %>%
    summarize(start=min(id), end=max(id) - empty_bar) %>%
    rowwise() %>%
    mutate(title=mean(c(start, end)))

  # prepare a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  grid_data=grid_data[-1,]

  # Make the plot
  p = ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar

    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +

    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 4, xend = start, yend = 4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),3), y = c(4, 15, 30), label = c("4", "15", "30") , color="grey", size=4 , angle=0, fontface="bold", hjust=1) +

    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm")
    ) +
    coord_polar() +
    geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +

    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,0.7,0), colour = "black", alpha=0.8, size=5, fontface="bold", inherit.aes = FALSE)

  p
  if (!is.null(directorypath)){
    ggsave(filename =paste0(directorypath , "/" ,"Go annotation.jpeg") , plot = p , device = "jpeg" , width = 19  , height = 15.5)
    ggsave(filename =paste0(directorypath , "/" ,"Go annotation.tiff") , plot = p , device = "tiff" , width = 19  , height = 15.5)

  }
}
