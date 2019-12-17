#' Connect and DOWNLOAD Proteome info.
#'
#'The function is work to retrieve proteome information in FASTA format based on proteome id.
#'
#' @usage GetProteomeFasta(ProteomeID , directorypath = NULL)
#'
#' @param ProteomeID Proteome ID from UniProt
#'
#' @param directorypath path to save FASTA file containig results returened by the function.
#'
#' @note The function Download fasta format of proteome.
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
GetProteomeFasta <- function(ProteomeID , directorypath = NULL)
{
  baseUrl <- "https://www.uniprot.org/uniprot/?query=proteome:"
  Request <- GET(paste0(baseUrl , ProteomeID,"&format=fasta"))
  if (Request$status_code == 200)
  {
    download.file(Request$url ,paste0(directorypath ,"/" , ProteomeID, ".fasta"))
  }
  else {
    HandleBadRequests(Request$status_code)
  }
}



