#' Retrieve TABNET data in dataframe form
#'
#' Retrieve a dataframe with indicator data, per region, per year.
#' @param indicator_index Indicator index number, @seealso [tabnet_index()].
#' @param region String describing how should data be regionalized.
#' @param subindicator Among the indicator, which subindicator should be used? Defaults to the last index.
#' @param years Retrieve data from one or more specific years?
#' @param onlyMostRecent Boolean. If true, will retrieve only data from the most recent year informed.
#' @keywords tabnet
#' @export
#' @return Regionalized data frame from selected indicator
#' @examples
#' @seealso [tabnet_csv_retrieval()]

tabnet_df <- function(indicator_index, region = "Município", subindicator = NULL, years = NULL, 
                            onlyMostRecent = FALSE) {
  matriz <- tabnet_index()
  data <- NULL
  if (indicator_index %in% c(1:length(matriz$Links))) {
    obj <- make_tabnet_obj(matriz$Links[indicator_index])
    linha <- min(c(which(obj$NomesLinhas == region)))
    indicator <- ifelse(is.null(subindicator), length(obj$Indicadores), subindicator)
    anos <- obj$Arquivos
    if (!is.null(years)) {
      anos <- obj$Arquivos[obj$Anos %in% years]
    }
    
    if (onlyMostRecent) {
      anos <- anos[1]
    }
    
    if (linha == 0) {
      warning("region not found")
    } else if (indicator > length(obj$Indicadores)) {
      warning("indicator not found")
    } else {
      print(paste0("Returning indicator: '", obj$NomesIndicadores[indicator],
                   "'; To retrieve a different one, use 'subindicator' index."))
      print(paste0("linha:", obj$Linhas[linha]))
      print(obj$Indicadores[indicator])
      print(anos)
      
      body <- getPostRequestBody(obj$Linhas[linha], obj$Indicadores[indicator], anos)
      print(body)
      print(obj$POST_url)
      print(region)
      data <- tabnet_csv_retrieval(obj$POST_url, body, region)
      print(head(data))
    }
  } else {
    warning("indicator index not found among available indicators")
  }
  
  return(data)
}