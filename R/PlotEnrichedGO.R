#' Connect and parse UniProt information
#'
#' This function is used to generate a combined plot for the enriched Gene Ontology terms  
#'
#' @usage PlotEnrichedGO(Accs,OS="hsapiens",p_value=0.05,Path=NULL,theme="aaas",width=7,height=7)
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
#' @param width width of the generated plot 
#' 
#' @param height height of the generated plot
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#' 
PlotEnrichedGO <- function(Accs,OS="hsapiens",p_value=0.05,Path=NULL,theme="aaas",width=7,height= 7)
{

  Enr.data <- gost(Accs)

  
  Enr.frame <- Enr.data$result
  
  GOs <- Enr.frame[Enr.frame$source %in% c("GO:BP", "GO:CC", "GO:MF"),]
  GOs$`-log10 (p)` <- -log10(GOs$p_value)
  
  P <- ggbarplot(GOs, x = "term_name", y = "-log10 (p)",
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
    if (dim(GOs)[1] < 50)
      ggsave(path = Path, filename = "Significant GOs.jpeg", plot = P,width = width, height = height, dpi = 300)
    if (dim(GOs)[1] > 50)
      ggsave(path = Path, filename = "Significant GO.jppeg", plot = P,width = width, height =height, dpi = 300)
  }
  plot(P)

}

