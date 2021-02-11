#' Connect and parse UniProt information.
#'
#' This Function is used to download sequences of given protein list in a fasta format
#'
#' @usage GETSeqFastaUniprot(Accessions, FileName = NULL)
#'
#' @param Accessions  list of Uniprot accessions 
#'
#' @param FileName OUtput file name 
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
GETSeqFastaUniprot <- function(Accessions, FileName = NULL)
{
  OutNumber <- 0
  for (Acc in Accessions)
  {
    Request <- tryCatch(
      {
        GET(paste0("https://www.uniprot.org/uniprot/" , Acc , ".Fasta"))
      },error = function(cond)
      {
        message("Internet connection problem occurs and the function will return the original error")
        message(cond)
      }
    )
    if (Request$status_code == 200)
    {
      OutNumber <<- OutNumber + 1
      Fastadata <- read.csv(paste0("https://www.uniprot.org/uniprot/" , Acc , ".Fasta") , header = F , sep = "\t")
      Sequences <- paste0(as.character(unlist(Fastadata)) , collapse = "\n")
      if (!is.null(FileName))
      {
        write.table(x = Sequences , file = paste0(FileName ,".fasta") , quote = F , row.names = F , col.names = F, append = T)
      }
    }
    
  }
  return(OutNumber)
}