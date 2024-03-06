#' Connect and parse UniProt information.
#'
#' This Function is used to get Sequence information of accession/s from Uniprot as a Fasta file.
#'
#' @usage GETSeqFastaUniprot(Accessions,FilePath = NULL, FileName = NULL)
#'
#' @param Accessions Vector of UniProt Accession/s
#'
#' @param FilePath path of directory to save the output fasta.
#' 
#' @param FileName Name of the fasta file.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
GETSeqFastaUniprot <- function(Accessions, FilePath = NULL ,FileName = NULL)
{
  message("Please wait we are processing your accessions ...")
  pb <- progress::progress_bar$new(total = length(Accessions))
  
  for (Acc in Accessions)
  {
    Request <- tryCatch(
      {
        GET(paste0("https://rest.uniprot.org/uniprotkb/" , Acc , ".fasta") , timeout(10))
      },error = function(cond)
      {
        message("Internet connection problem occurs and the function will return the original error")
        message(cond)
      }
    )
    if (Request$status_code == 200)
    {
      Fastadata <- tryCatch(read.csv(paste0("https://rest.uniprot.org/uniprotkb/" , Acc , ".fasta"),
                            header = F , sep = "\t"), error=function(e) NULL)
      Sequences <- paste0(as.character(unlist(Fastadata)) , collapse = "\n")
      write.table(x = Sequences , file = paste0(FileName ,".fasta") , quote = F , row.names = F , col.names = F, append = T)
      pb$tick()
    }
    
  }
}
