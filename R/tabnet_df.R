#' TABNET Object Check
#'
#' Check if a certain object has the appropriate TABNET object parameters.
#' @param obj Object to be checked
#' @keywords tabnet
#' @export
#' @return Boolean, indicating whether object passed is a valid TABNET object
#' @examples


is.tabnet_obj <- function(obj) {
  return (all(
    c(
      "url",
      "POST_url",
      "NomesLinhas",
      "Linhas",
      "Info",
      "NomesIndicadores",
      "Indicadores",
      "Anos",
      "Arquivos"
    ) %in% attributes(obj)$names
  ))
}

#' TABNET dataframe retrieval
#'
#' Given a TABNET Object and certain parameters, automatically retrieve corresponding data frame
#' @param obj TABNET Object
#' @param line_index Chosen line index, defaults to 2 (city)
#' @param ind_index Chosen subindicator index, defaults to last one
#' @param years_index Chosen year(s), defaults to most recent available
#' @param onlyMostRecent Boolean. Should only data from most recent year informed be returned?
#' @keywords tabnet
#' @export
#' @return Corresponding data frame
#' @examples

tabnet_df_retrieval <- function(obj, line_index = 2, ind_index = NULL, years_index = NULL, onlyMostRecent = FALSE, timeout = 4) {
  if (is_tabnet_obj(obj)) {
    subindicator <- ind_index
    if (is.null(subindicator)) {
      subindicator <- length(obj$Indicadores)
    }
    
    years <- years_index
    if (is.null(years)) {
      years <- c(1:length(obj$Arquivos))
    }
    if (onlyMostRecent) {
      years <- years[1]
    }
  }
  body <- getPostRequestBody(obj$Linhas[line_index],
                             obj$Indicadores[subindicator],
                             obj$Arquivos[years]
  )
  
  return (tabnet_csv_retrieval(obj$POST_url, body, obj$NomesLinhas[line_index], timeout = timeout))
}

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
      
      body <- getPostRequestBody(obj$Linhas[linha], obj$Indicadores[indicator], anos)
      data <- tabnet_csv_retrieval(obj$POST_url, body, region)
    }
  } else {
    warning("indicator index not found among available indicators")
  }
  
  return(data)
}