#' Connect and parse UniProt information.
#'
#' This function can be used to get a list of UniProt Accession/s from a csv file.
#'
#' @usage GetAccessionList(DataObjPath)
#'
#' @param DataObjPath input path of excel file
#'
#' @return a vector of UniProt Accession/s
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
GetAccessionList <- function(DataObjPath)
{
  DataSet <- read.csv(DataObjPath)
  AccessionList <- as.array(as.character(DataSet[,1]))
  return(AccessionList)
}

