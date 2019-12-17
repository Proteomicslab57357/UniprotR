#' Handle bad requests
#' This Function is used to handle possible errors when trying to get url response.
#'
#' @usage HandleBadRequests(RequestCode)
#'
#' @param RequestCode Response returned from url
#'
#' @return None
#'
#' @examples HandleBadRequests(400)
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'

HandleBadRequests <- function(RequestCode)
{
  # all Bad requestes
  if (RequestCode == 400) {
  print("Bad request. The resource you requested doesn't exist or There is a problem with your input.")
}else if (RequestCode == 404) {
  print("Not found. The resource you requested doesn't exist.")
}else if (RequestCode == 410) {
  print("Gone. The resource you requested was removed.")
}else if (RequestCode == 500) {
  print("Internal server error. Most likely a temporary problem, but if the problem persists please contact us.")
}else if (RequestCode == 503) {
  print("Service not available. The server is being updated, try again later.")
}
}
