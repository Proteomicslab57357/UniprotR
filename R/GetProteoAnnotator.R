#' Connect and parse UniProt information.
#'
#' The function is work to retrieve user-defined information data from UniProt
#' for a list of proteins accessions For more
#' information see https://www.uniprot.org/help/uniprotkb_column_names
#'
#' @usage GetProteinAnnontate(ProteinAccList , columns)
#'
#' @param ProteinAccList a vector of UniProt Accession/s
#'
#' @param columns a vector of UniProtKB column names
#'
#'
#' @return DataFrame where rows names are the accession
#'      and columns contains the information retrieved from the UniProt
#'
#' @examples Obj <- GetProteinInteractions("O14520")
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export


GetProteinAnnontate <- 
  function (ProteinAccList, columns) 
  {
    if (!has_internet()) {
      message("Please connect to the internet as the package requires internect connection.")
      return()
    }
    baseUrl <- "https://rest.uniprot.org/uniprotkb/"
    ProteinInfoParsed_total_col = data.frame(x = "x")
    for (filed in columns) {
      ProteinInfoParsed_total <- data.frame()
      for (ProteinAcc in ProteinAccList) {
        Request <- tryCatch({
          GET(paste0(baseUrl, ProteinAcc, ".xml"), timeout(10))
        }, error = function(cond) {
          message("Internet connection problem occurs and the function will return the original error")
          message(cond)
        })
        ProteinName_url <- paste0("/search?query=accession:", ProteinAcc, 
                                  "&format=tsv&fields=", filed)
        RequestUrl <- paste0(baseUrl, ProteinName_url)
        if (length(Request) == 0) {
          message("Internet connection problem occurs")
          return()
        }
        if (Request$status_code == 200) {
          parse_true <- function() {
            ProteinInfoParsed <- as.data.frame(read.csv(RequestUrl, 
                                                        sep = "\t", header = TRUE), row.names = ProteinAcc)
            return(ProteinInfoParsed)
          }
          parse_false <- function() {
            ProteinInfoParsed <- read.csv(RequestUrl, 
                                          sep = "\t", header = TRUE)
            names <- names(ProteinInfoParsed)
            ProteinInfoParsed <- data.frame(name_col = "NA", 
                                            row.names = ProteinAcc)
            colnames(ProteinInfoParsed) <- names
            return(ProteinInfoParsed)
          }
          ProteinInfoParsed <- tryCatch(parse_true(), 
                                        error = function(e) parse_false())
          ProteinInfoParsed_total <- rbind(ProteinInfoParsed_total, 
                                           ProteinInfoParsed)
        }
        else {
          HandleBadRequests(Request$status_code)
        }
      }
      ProteinInfoParsed_total_col <- cbind(ProteinInfoParsed_total_col, 
                                           ProteinInfoParsed_total)
      remove(ProteinInfoParsed_total)
    }
    ProteinInfoParsed_total_col <- ProteinInfoParsed_total_col[, 
                                                               !(names(ProteinInfoParsed_total_col) %in% c("x"))]
    return(ProteinInfoParsed_total_col)
  }
