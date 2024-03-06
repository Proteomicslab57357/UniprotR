#' Connect and parse UniProt information
#'
#' This function is used for Enrichment analysis of given list of genes or proteins 
#'
#' @usage Pathway.Enr(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
#'
#' @param Accs Vector of UniProt Accession/s or genes 
#' 
#' @param OS  organism Example: human - 'hsapiens' for more info https://biit.cs.ut.ee/gprofiler/page/organism-list
#' 
#' @param p_value custom p-value threshold for significance, default = 0.05
#' 
#' @param directorypath Path to save output plot 
#' 
#' @param top Top N terms to be visualized
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
Pathway.Enr <- function(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
{
  
  AccList <- as.character(unique(Accs))
  Enrich.object <- gost(query = Accs, sources = c('KEGG', 'REAC'), organism = OS, user_threshold = p_value, evcodes = T)
  
  Enrich.Res <- Enrich.object$result
  Enrich.Res[1:top,]
  Enrich.Res$source <- ifelse(Enrich.Res$source == "KEGG", "KEGG", "REACTOME")
  # A function factory for getting integer y-axis values.
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }
  
  Enr.plot <- ggplot(Enrich.Res, aes(x = Enrich.Res$intersection_size, y = reorder(Enrich.Res$term_name, Enrich.Res$intersection_size),
                                       size = reorder(round(Enrich.Res$p_value, 3), -log10(Enrich.Res$p_value)), 
                                       color = Enrich.Res$source)) +
    geom_point() +
    theme_bw() + xlab("# of detected proteins") + ylab("Pathway") +
    theme(legend.position="right", text = element_text(face="bold"),
          axis.text = element_text(color = "black", face = "bold")) +
    labs(size = "p.adj", color = "Database") +
    scale_color_manual(values = c("#17202A", "#A93226")) +
   scale_x_continuous(breaks = integer_breaks())
  plot(Enr.plot)
  if (!is.null(directorypath))
  {
    if (dim(Enrich.Res)[1] < 50)
    {
      ggsave(filename = "Enrichment analysis.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 8.5, height = 10 ,dpi = 300)
      ggsave(filename = "Enrichment analysis.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 8.5, height = 10 ,dpi = 300)
    }
    else if (dim(Enrich.Res)[1] < 100)
    {
      ggsave(filename = "Enrichment analysis.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 15, height = 14 ,dpi = 300)
      ggsave(filename = "Enrichment analysis.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 15, height = 14 ,dpi = 300)
    }
  }
  return(Enr.plot)
}
#' Connect and parse UniProt information
#'
#' This function is used for Enrichment analysis of given list of genes or proteins from KEGG database
#'
#' @usage Enrichment.KEGG(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
#'
#' @param Accs Vector of UniProt Accession/s or genes 
#' 
#' @param OS  organism name Example: human - 'hsapiens', mouse - 'mmusculus'
#' 
#' @param p_value custom p-value threshold for significance, default = 0.05
#' 
#' @param directorypath Path to save output plot 
#' 
#' @param top Top N terms to be visualized
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
Enrichment.KEGG <- function(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
{
  
  AccList <- as.character(unique(Accs))
  Enrich.object <- gost(query = Accs, sources = c('KEGG'), organism = OS, user_threshold = p_value, evcodes = T)
  
  Enrich.Res <- Enrich.object$result
  Enrich.Res <- Enrich.Res[1:top,]
  # A function factory for getting integer y-axis values.
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }
  
  Enr.plot <- ggplot(Enrich.Res, aes(x = Enrich.Res$intersection_size, y = reorder(Enrich.Res$term_name, Enrich.Res$intersection_size),
                                     size = reorder(round(Enrich.Res$p_value, 3), -log10(Enrich.Res$p_value)), 
                                     color = Enrich.Res$p_value)) +
    geom_point() +
    theme_bw() + xlab("# of detected proteins") + ylab("Pathway") +
    theme(legend.position="right", text = element_text(face="bold"),
          axis.text = element_text(color = "black", face = "bold")) +
    labs(size = "p.adj", color = " ") +
    scale_color_gradient(low = "#17202A", high =  "#A93226") +
    scale_x_continuous(breaks = integer_breaks())
  plot(Enr.plot)
  if (!is.null(directorypath))
  {
    if (dim(Enrich.Res)[1] < 50)
    {
      ggsave(filename = "Enrichment KEGG.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 7, height = 9 ,dpi = 300)
      ggsave(filename = "Enrichment KEGG.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 7, height = 9 ,dpi = 300)
    }
    else if (dim(Enrich.Res)[1] < 100)
    {
      ggsave(filename = "Enrichment KEGG.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 15, height = 13 ,dpi = 300)
      ggsave(filename = "Enrichment KEGG.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 15, height = 13 ,dpi = 300)
    }
  }
  return(Enr.plot)
}
#' Connect and parse UniProt information
#'
#' This function is used for Enrichment analysis of given list of genes or proteins from REACTOME
#'
#' @usage Enrichment.REAC(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
#'
#' @param Accs Vector of UniProt Accession/s or genes 
#' 
#' @param OS  organism name Example: human - 'hsapiens', mouse - 'mmusculus'
#' 
#' @param p_value custom p-value threshold for significance, default = 0.05
#' 
#' @param directorypath Path to save output plot 
#'
#' @param top Top N terms to be visualized
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
Enrichment.REAC <- function(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
{
  
  AccList <- as.character(unique(Accs))
  Enrich.object <- gost(query = Accs, sources = c('REAC'), organism = OS, user_threshold = p_value, evcodes = T)
  
  Enrich.Res <- Enrich.object$result
  Enrich.Res <- Enrich.Res[1:top,]
  
  # A function factory for getting integer y-axis values.
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }
  
  Enr.plot <- ggplot(Enrich.Res, aes(x = Enrich.Res$intersection_size, y = reorder(Enrich.Res$term_name, Enrich.Res$intersection_size),
                                     size = reorder(round(Enrich.Res$p_value, 3), -log10(Enrich.Res$p_value)), 
                                     color = Enrich.Res$p_value)) +
    geom_point() +
    theme_bw() + xlab("# of detected proteins") + ylab("Pathway") +
    theme(legend.position="right", text = element_text(face="bold"),
          axis.text = element_text(color = "black", face = "bold")) +
    labs(size = "p.adj", color = " ") +
    scale_color_gradient(low = "#17202A", high =  "#A93226") +
    scale_x_continuous(breaks = integer_breaks())
  plot(Enr.plot)
  if (!is.null(directorypath))
  {
    if (dim(Enrich.Res)[1] < 50)
    {
      ggsave(filename = "Enrichment REAC.jpeg", plot = Enr.plot, path = directorypath , device = "jpeg", width = 8.5, height = 10 ,dpi = 300)
      ggsave(filename = "Enrichment REAC.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 8.5, height = 10 ,dpi = 300)
    }
    else if (dim(Enrich.Res)[1] < 100)
    {
      ggsave(filename = "Enrichment REAC.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 15, height = 13 ,dpi = 300)
      ggsave(filename = "Enrichment REAC.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 15, height = 13 ,dpi = 300)
    }
  }
  return(Enr.plot)
}

#' Connect and parse UniProt information
#'
#' This function is used for Enrichment analysis of biological process of given list of genes or proteins
#'
#' @usage Enrichment.BP(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
#'
#' @param Accs Vector of UniProt Accession/s or genes 
#' 
#' @param OS  organism name Example: human - 'hsapiens', mouse - 'mmusculus'
#' 
#' @param p_value custom p-value threshold for significance, default = 0.05
#' 
#' @param directorypath Path to save output plot 
#' 
#' @param top Top N terms to be visualized
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
Enrichment.BP <- function(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL, top=10)
{
  
  AccList <- as.character(unique(Accs))
  Enrich.object <- gost(query = Accs, sources = c('GO:BP'), organism = OS, user_threshold = p_value, evcodes = T)
  
  Enrich.Res <- Enrich.object$result
  Enrich.Res <- Enrich.Res[1:top,]
  
  Enr.plot <- ggplot(Enrich.Res, aes(x = -log10(Enrich.Res$p_value), y = reorder(Enrich.Res$term_name, -Enrich.Res$p_value))) +
    geom_bar(stat = "identity", fill = "darkred") +
    theme_bw() + xlab("-log10 (p.adj)") + ylab("Biological process") +
    theme(legend.position="right", text = element_text(face="bold"),
          axis.text = element_text(color = "black", face = "bold")) 
  plot(Enr.plot)
  if (!is.null(directorypath))
  {
    if (dim(Enrich.Res)[1] < 50)
    {
      ggsave(filename = "Enrichment BP.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 7, height = 6 ,dpi = 300)
      ggsave(filename = "Enrichment BP.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 7, height = 6 ,dpi = 300)
    }
    else if (dim(Enrich.Res)[1] < 100)
    {
      ggsave(filename = "Enrichment BP.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 15, height = 13 ,dpi = 300)
      ggsave(filename = "Enrichment BP.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 15, height = 13 ,dpi = 300)
    }
  }
  return(Enr.plot)
}

#' Connect and parse UniProt information
#'
#' This function is used for Enrichment analysis of Molecular function of given list of genes or proteins
#'
#' @usage Enrichment.MF(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
#'
#' @param Accs Vector of UniProt Accession/s or genes 
#' 
#' @param OS  organism name Example: human - 'hsapiens', mouse - 'mmusculus'
#' 
#' @param p_value custom p-value threshold for significance, default = 0.05
#' 
#' @param directorypath Path to save output plot 
#' 
#' @param top Top N terms to be visualized
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
Enrichment.MF <- function(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
{
  
  AccList <- as.character(unique(Accs))
  Enrich.object <- gost(query = Accs, sources = c('GO:MF'), organism = OS, user_threshold = p_value, evcodes = T)
  
  Enrich.Res <- Enrich.object$result
  Enrich.Res <- Enrich.Res[1:top,]
  Enr.plot <- ggplot(Enrich.Res, aes(x = -log10(Enrich.Res$p_value), y = reorder(Enrich.Res$term_name, -Enrich.Res$p_value))) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    theme_bw() + xlab("-log10 (p.adj)") + ylab("Molecular function") +
    theme(legend.position="right", text = element_text(face="bold"),
          axis.text = element_text(color = "black", face = "bold")) 
  plot(Enr.plot)
  if (!is.null(directorypath))
  {
    if (dim(Enrich.Res)[1] < 50)
    {
      ggsave(filename = "Enrichment MF.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 7, height = 6 ,dpi = 300)
      ggsave(filename = "Enrichment MF.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 7, height = 6 ,dpi = 300)
    }
    else if (dim(Enrich.Res)[1] < 100)
    {
      ggsave(filename = "Enrichment MF.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 15, height = 13 ,dpi = 300)
      ggsave(filename = "Enrichment MF.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 15, height = 13 ,dpi = 300)
    }
  }
  return(Enr.plot)
}

#' Connect and parse UniProt information
#'
#' This function is used for Enrichment analysis of cellular component of given list of genes or proteins
#'
#' @usage Enrichment.CC(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
#'
#' @param Accs Vector of UniProt Accession/s or genes 
#' 
#' @param OS  organism name Example: human - 'hsapiens', mouse - 'mmusculus'
#' 
#' @param p_value custom p-value threshold for significance, default = 0.05
#' 
#' @param directorypath Path to save output plot
#'
#' @param top Top N terms to be visualized
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
Enrichment.CC <- function(Accs,OS="hsapiens",p_value=0.05,directorypath=NULL,top=10)
{
  
  AccList <- as.character(unique(Accs))
  Enrich.object <- gost(query = Accs, sources = c('GO:CC'), organism = OS, user_threshold = p_value, evcodes = T)
  
  Enrich.Res <- Enrich.object$result
  Enrich.Res <- Enrich.Res[1:top,]
  
  Enr.plot <- ggplot(Enrich.Res, aes(x = -log10(Enrich.Res$p_value), y = reorder(Enrich.Res$term_name, -Enrich.Res$p_value))) +
    geom_bar(stat = "identity", fill = "darkblue") +
    theme_bw() + xlab("-log10 (p.adj)") + ylab("Cellular component") +
    theme(legend.position="right", text = element_text(face="bold"),
          axis.text = element_text(color = "black", face = "bold")) 
  plot(Enr.plot)
  if (!is.null(directorypath))
  {
    if (dim(Enrich.Res)[1] < 50)
    {
      ggsave(filename = "Enrichment CC.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 7, height = 6 ,dpi = 300)
      ggsave(filename = "Enrichment CC.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 7, height = 6 ,dpi = 300)
    }
    else if (dim(Enrich.Res)[1] < 100)
    {
      ggsave(filename = "Enrichment CC.jpeg", plot = Enr.plot, path = directorypath, device = "jpeg", width = 15, height = 13 ,dpi = 300)
      ggsave(filename = "Enrichment CC.tiff", plot = Enr.plot, path = directorypath, device = "tiff", width = 15, height = 13 ,dpi = 300)
    }
  }
  return(Enr.plot)
}