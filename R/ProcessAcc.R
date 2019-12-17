#' Connect and parse UniProt information.
#'
#' This Function is used to check validty of input accessions the data of the accession/s.
#'
#' @usage ProcessAcc(Accessions)
#'
#' @param Accessions acession list returened from GetAccession function
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
ProcessAcc <- function(Accessions)
{
  Accession <- NULL
  for (i in 1: length(Accessions))
  {
    if (!grepl("-", Accessions[i]))
    {
      Accession <- c(Accession , Accessions[i]);
    }
  }
  return(Accession)
}
