library(httr)
library(dplyr)

isJobReady <- function(jobId) {
  pollingInterval = 5
  nTries = 20
  for (i in 1:nTries) {
    url <- paste("https://rest.uniprot.org/idmapping/status/", jobId, sep = "")
    r <- GET(url = url, accept_json())
    status <- content(r, as = "parsed")
    if (!is.null(status[["results"]]) || !is.null(status[["failedIds"]])) {
      return(TRUE)
    }
    if (!is.null(status[["messages"]])) {
      print(status[["messages"]])
      return (FALSE)
    }
    Sys.sleep(pollingInterval)
  }
  return(FALSE)
}

GetIDInfo <- function(protein_acc, ID_from, ID_to) {
  id_info <- lapply(ID_to, function(id) {
    files <- list(ids = protein_acc, from = ID_from, to = id)
    r <- POST(url = "https://rest.uniprot.org/idmapping/run", body = files, encode = "multipart", accept_json())
    submission <- content(r, as = "parsed", encoding = 'UTF-8')
    
    if (isJobReady(submission$jobId)) {
      details <- GetJobDetails(submission$jobId)
      resultsTable <- GetResultsTable(details$redirectURL)
      
      if (!is.null(resultsTable)) {
        colnames(resultsTable)[1] <- id
        return(resultsTable)
      }
    }
    return(NULL)
  })
  
  id_info <- Filter(Negate(is.null), id_info)
  id_frame <- do.call(cbind, id_info)
  return(id_frame)
}

GetJobDetails <- function(jobId) {
  url <- paste("https://rest.uniprot.org/idmapping/details/", jobId, sep = "")
  r <- GET(url = url, accept_json())
  details <- content(r, as = "parsed", encoding = 'UTF-8')
  return(details)
}

GetResultsTable <- function(redirectURL) {
  url <- paste(redirectURL, "?format=tsv", sep = "")
  r <- GET(url = url, accept_json())
  resultsTable <- read.table(text = content(r, encoding = 'UTF-8'), sep = "\t", header = TRUE)[, 1:2]
  resultsTable$From <- NULL
  
  if (dim(resultsTable)[1] > 1) {
    resultsTable <- data.frame(Id_to = paste(resultsTable$To, collapse = ","))
  }
  
  return(resultsTable)
}
#' Connect and parse UniProt database identifiers information.
#'
#' The function is work to convert the UniProtKB AC/ID to any database identifiers
#' available by the UniProtKB.
#' For more information about available database identifiers
#' see https://www.uniprot.org/help/id_mapping.
#' see https://raw.githubusercontent.com/MohmedSoudy/UniprotR/master/uniprot_ids.csv
#'
#' @usage ConvertID(ProteinAccList , ID_from = "UniProtKB_AC-ID" , ID_to = NULL
#'  , directorypath = NULL)
#'
#' @param ProteinAccList  Vector of UniProt Accession/s
#'
#' @param ID_from string of database identifier abbreviation, from which the Accession/ID will be converted
#'
#' @param ID_to string of database identifier abbreviation, to which the Accession/ID will be converted.
#'              default is all database identifier available in UniProtKB
#'
#' @param directorypath path to save excel file containig results returened by the function.
#'
#' @return DataFrame where column one contains the Accession/ID before conversion
#'      and other columns contains the Accession/ID after conversion
#'
#'
#' @note The function also, Creates a csv file with the retrieved information.
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#' 
ConvertID <- function(ProteinAccList, ID_from = "UniProtKB_AC-ID", ID_to = NULL, directorypath = NULL) {
  if (is.null(ID_to)) {
    ID_to <- c(
      "UniParc", "UniRef50", "UniRef90", "UniRef100", "Gene_Name", "CRC64", "CCDS", 
      "EMBL-GenBank-DDBJ", "EMBL-GenBank-DDBJ_CDS", "GI_number", "PIR", "RefSeq_Nucleotide", 
      "RefSeq_Protein", "PDB", "BioGRID", "ComplexPortal", "DIP", "STRING", "ChEMBL", 
      "DrugBank", "GuidetoPHARMACOLOGY", "SwissLipids", "Allergome", "CLAE", "ESTHER", 
      "MEROPS", "PeroxiBase", "REBASE", "TCDB", "GlyConnect", "BioMuta", "DMDM", "World-2DPAGE", 
      "CPTAC", "ProteomicsDB", "DNASU", "Ensembl", "Ensembl_Genomes", "Ensembl_Genomes_Protein", 
      "Ensembl_Genomes_Transcript", "Ensembl_Protein", "Ensembl_Transcript", "GeneID", "KEGG", 
      "PATRIC", "WBParaSite_Transcript_Protein", "ArachnoServer", "Araport", "CGD", "ConoServer", 
      "dictyBase", "EchoBASE", "euHCVdb", "FlyBase", "GeneCards", "GeneReviews", "HGNC", "LegioList", 
      "Leproma", "MaizeGDB", "MGI", "MIM", "neXtProt", "OpenTargets", "Orphanet", "PharmGKB", "PomBase", 
      "PseudoCAP", "RGD", "SGD", "TubercuList", "VEuPathDB", "VGNC", "WormBase-Protein", 
      "WormBase-Transcript", "Xenbase", "ZFIN", "eggNOG", "GeneTree", "HOGENOM", "OMA", "OrthoDB", 
      "TreeFam", "BioCyc", "PlantReactome", "Reactome", "UniPathway", "ChiTaRS", "GeneWiki", 
      "GenomeRNAi", "PHI-base", "CollecTF", "DisProt", "IDEAL"
    )
  }
  
  id_info_list <- lapply(ProteinAccList, function(protein_acc) {
    GetIDInfo(protein_acc, ID_from, ID_to)
  })
  
  id_frame <- bind_rows(id_info_list)
  rownames(id_frame) <- ProteinAccList
  if (!is.null(directorypath)){
    write.csv(id_frame, paste0(directorypath, "Mapped_ids.csv"))
  }
  return(id_frame)
}
