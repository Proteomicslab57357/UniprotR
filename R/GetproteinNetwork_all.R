#' Connect and parse stringdb information.
#'
#' This function is connecting to stringdb and retrieve PPI between input list 
#'
#' @usage GetproteinNetwork_all(ProteinAccList , directorypath = NULL, SpeciesID = 9606)
#'
#' @param ProteinAccList input a vector of UniProt Accession/s
#' 
#' @param SpeciesID Taxonomic id of accession's species ex. homo sapines 9606
#'
#' @param directorypath path to save excel file containig results returened by the function.
#'
#' @usage GetproteinNetwork_all(ProteinAccList , directorypath = NULL,SpeciesID = 9606)
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
GetproteinNetwork_all <- function(ProteinAccList , directorypath = NULL, SpeciesID = 9606)
{
  pdf(paste0(directorypath , "/","Whole Protin Network.pdf"))
  baseUrl <- "https://string-db.org/api/image/network?identifiers="
  ProteinAccList <- unique(ProteinAccList)
  ProteinList <- paste0(ProteinAccList , collapse = "%0d")
  ProteinList <- paste0(baseUrl,ProteinList)
  ProteinList <- paste0(ProteinList , "&network_flavor=actions&block_structure_pics_in_bubbles=1", "&species=", SpeciesID)
  WholeNetwork <- image_read(ProteinList)
  plot(WholeNetwork)
  title(list("Whole Protein Network", cex = 1,
             col = "black", font = 1))
  dev.off()
}