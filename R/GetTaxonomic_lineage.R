#' Connect and parse UniProt Taxonomic_lineage information.
#'
#' The function is work to retrieve Taxonomic_lineage data from UniProt for a list of proteins accessions.
#' For more information about what included in the Taxonomic_lineage
#' data see https://www.uniprot.org/help/uniprotkb_column_names.
#'
#' @usage GetTaxonomic_lineage(ProteinAccList, directorypath = NULL)
#'
#' @param ProteinAccList Vector of UniProt Accession/s
#'
#' @param directorypath path to save excel file containig results returened by the function.
#'
#' @return DataFrame where rows names are the accession
#'      and columns contains the information retrieved from the UniProt
#'
#' @note The function also, Creates a csv file with the retrieved information.
#'
#' @examples Obj <- GetTaxonomic_lineage("O14520")
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
#'
GetTaxonomic_lineage <- function(ProteinAccList , directorypath = NULL){
  if(!has_internet())
  {
    message("Please connect to the internet as the package requires internect connection.")
    return()
  }

  # Sequences information to be collected
  columns <- c("lineage(all),lineage(SUPERKINGDOM),lineage(KINGDOM),lineage(SUBKINGDOM),lineage(SUPERPHYLUM),lineage(PHYLUM),lineage(SUBPHYLUM),lineage(SUPERCLASS),lineage(CLASS),lineage(SUBCLASS),lineage(INFRACLASS),lineage(SUPERORDER),lineage(ORDER),lineage(SUBORDER), lineage(INFRAORDER),lineage(PARVORDER),lineage(SUPERFAMILY),lineage(FAMILY),lineage(SUBFAMILY),lineage(TRIBE),lineage(SUBTRIBE),lineage(GENUS),lineage(SUBGENUS),lineage(SPECIES GROUP),lineage(SPECIES SUBGROUP),lineage(SPECIES),lineage(SUBSPECIES),lineage(VARIETAS),lineage(FORMA)")

  message("Please wait we are processing your accessions ...")
  pb <- progress::progress_bar$new(total = length(ProteinAccList))
  
  baseUrl <- "http://www.uniprot.org/uniprot/"
  ProteinInfoParsed_total = data.frame()
  for (ProteinAcc in ProteinAccList)
  {
    #to see if Request == 200 or not
    Request <- tryCatch(
      {
        GET(paste0(baseUrl , ProteinAcc,".xml") , timeout(7))
      },error = function(cond)
      {
        message("Internet connection problem occurs and the function will return the original error")
        message(cond)
      }
    )
    #this link return information in tab formate (format = tab)
    #columns = what to return from all of the information (see: https://www.uniprot.org/help/uniprotkb_column_names)
    ProteinName_url <- paste0("?query=accession:",ProteinAcc,"&format=tab&columns=",columns)

    RequestUrl <- paste0(baseUrl , ProteinName_url)
    RequestUrl <- URLencode(RequestUrl)
    if (length(Request) == 0)
    {
      message("Internet connection problem occurs")
      return()
    }
    if (Request$status_code == 200){
      # parse the information in DataFrame
      ProteinDataTable <- tryCatch(read.csv(RequestUrl, header = TRUE, sep = '\t'), error=function(e) NULL)
      if (!is.null(ProteinDataTable))
      {
        ProteinDataTable <- ProteinDataTable[1,]
        ProteinInfoParsed <- as.data.frame(ProteinDataTable,row.names = ProteinAcc)
        # add Dataframes together if more than one accession
        ProteinInfoParsed_total <- rbind(ProteinInfoParsed_total, ProteinInfoParsed)
      }
    }else {
      HandleBadRequests(Request$status_code)
    }
    pb$tick()
    
    }

  if (!is.null(directorypath)){
  write.csv(ProteinInfoParsed_total , paste0(directorypath , "/" , "Taxonomic_lineage Information.csv"))
  }
  return(ProteinInfoParsed_total)
}
