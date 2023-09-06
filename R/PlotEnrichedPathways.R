#' Connect and parse UniProt information
#'
#' This function is used to generate a combined plot for the enriched pathways from KEGG and REACTOME 
#'
#' @usage PlotEnrichedPathways(Accs,OS="hsapiens",p_value=0.05,Path=NULL,theme="aaas",w=w,h=h)
#'
#' @param Accs Vector of UniProt Accession/s or genes 
#' 
#' @param OS  organism name Example: human - 'hsapiens', mouse - 'mmusculus'
#' 
#' @param p_value custom p-value threshold for significance, default = 0.05
#' 
#' @param theme optional parameter to generate specific theme for journals ex: "aaas", "nature", "lancet", "jama"
#' 
#' @param Path Path to save output plot 
#' 
#' @param w width of the generated plot 
#' 
#' @param h height of the generated plot
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#' 
PlotEnrichedPathways <- function(Accs,OS="hsapiens",p_value=0.05,Path=NULL,theme="aaas",w=w,h=h)
{

  Enr.data <- gost(Accs)

  
  Enr.frame <- Enr.data$result
  
  Pathways <- Enr.frame[Enr.frame$source %in% c("KEGG", "REAC"),]
  Pathways$`-log10 (p)` <- -log10(Pathways$p_value)
  
  P <- ggbarplot(Pathways, x = "term_name", y = "-log10 (p)",
                 fill = "source",               
                 color = "white",            
                 palette = "jco",            
                 sort.val = "asc",           
                 sort.by.groups = TRUE,      
                 x.text.angle = 90          
  )
  if (tolower(theme) == "aaas")
    P <- P + scale_fill_aaas() + xlab("") +  coord_flip()
  if (tolower(theme) == "lancet")
    P <- P + scale_fill_lancet() + xlab("") +  coord_flip()
  if (tolower(theme) == "jama")
    P <- P + scale_fill_jama() + xlab("") +  coord_flip()
  if (tolower(theme) == "nature")
    P <- P + scale_fill_nejm() + xlab("") +  coord_flip()
  
  if (!is.null(Path))
  {
    if (dim(Pathways)[1] < 50)
      ggsave(path = Path, filename = "Significant Pathways.jpeg", plot = P,width = w, height = h, dpi = 300)
    if (dim(Pathways)[1] > 50)
      ggsave(path = Path, filename = "Significant Pathways.jpeg", plot = P,width = w, height = h, dpi = 300)
  }
  plot(P)

}

