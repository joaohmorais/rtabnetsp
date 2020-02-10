library(purrr)

#' TABNET Object Check
#'
#' Check if a certain object has the appropriate TABNET object parameters.
#' @param obj Object to be checked
#' @keywords tabnet
#' @export
#' @return Boolean, indicating whether object passed is a valid TABNET object
#' @examples
#' @seealso [make_tabnet_obj()]


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

#' TABNET dataframe retrieval from TABNET object
#'
#' Given a TABNET Object and certain parameters, automatically retrieve corresponding data frame
#' @param obj TABNET Object
#' @param line_index Chosen line index, defaults to 2 - city
#' @param ind_index Chosen subindicator index, defaults to last one
#' @param years_index Chosen years/years, defaults to most recent available
#' @param onlyMostRecent Boolean. Should only data from most recent year informed be returned?
#' @keywords tabnet
#' @export
#' @return Corresponding data frame
#' @examples
#' \dontrun{tabnet_df_retrieval(obj, line_index = 3, ind_index = 1, years_index = c(2:4))}
#' \dontrun{tabnet_df_retrieval(obj, onlyMostRecent = TRUE)}
#' @seealso [tabnet_csv_retrieval()]
#' @seealso [indicator_df()]

tabnet_df_retrieval <- function(obj, line_index = 2, ind_index = NULL, years_index = NULL, onlyMostRecent = FALSE, timeout = 4) {
  if (is.tabnet_obj(obj)) {
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

#' Retrieve TABNET data from Indicator Index (deprecated)
#'
#' Retrieve a dataframe with indicator data, per region, per year. Deprecated, @seealso [indicator_df()]
#' @param indicator_index Indicator index number, @seealso [tabnet_index()].
#' @param region String describing how should data be regionalized.
#' @param subindicator Among the indicator, which subindicator should be used? Defaults to the last index.
#' @param years Retrieve data from one or more specific years?
#' @param onlyMostRecent Boolean. If true, will retrieve only data from the most recent year informed.
#' @keywords tabnet
#' @export
#' @return Regionalized data frame from selected indicator
#' @examples
#' @seealso [indicator_df()]
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

#' Retrieve Indicator data from its Index
#'
#' Retrieve a dataframe with indicator data from its index, per region, in certain year period.
#' @param indicator_index Indicator index number, @seealso [tabnet_index()].
#' @param region String describing how should data be regionalized, or an integer indicating region index, @seealso [view_indicator()]
#' @param subindicator Among the indicator, which subindicator should be used? Defaults to the last index.
#' @param years Specific years desired or their indexes. Defaults to all years available.
#' @param onlyMostRecent Boolean. If true, will retrieve only data from the most recent year informed.
#' @param url TABNET url, defaults to Sao Paulo TABNET.
#' @param timeout Time for connection to expire, in seconds.
#' @keywords tabnet
#' @export
#' @return Regionalized data frame from selected indicator
#' @examples
#' \dontrun{indicator_df(21, region = "DRS", subindicator = 1, years = c(2007, 2008))}
#' \dontrun{indicator_df(4, region = "RRAS", onlyMostRecent = TRUE)}
#' @seealso [tabnet_df_retrieval()]
#' @seealso [tabnet_csv_retrieval()]
indicator_df <- function(indicator_index, region = NULL, subindicator = NULL, years = NULL, 
                      onlyMostRecent = FALSE, url = "http://portal.saude.sp.gov.br/links/matriz", timeout = 1) {
  obj <- make_tabnet_obj(tabnet_index(url)$Links[indicator_index], timeout = timeout)
  if (is.null(region)) {
    region_index <- ifelse(obj$NomesLinhas[1] == "Ano", 2, 1)
  } else if (is.numeric(region)) {
    region_index <- region
  }  else {
    region_index <- min(c(which(obj$NomesLinhas == region)))
  }
  
  subindicator_index <- ifelse(is.null(subindicator), length(obj$Indicadores), subindicator)
  
  
  if (is.null(years)) {
    anos <- obj$Arquivos
  } else {
    if (all(years > 1000)) {
      anos <- obj$Arquivos[obj$Anos %in% years]
    } else {
      anos <- obj$Arquivos[years]
    }
  }
  
  if (onlyMostRecent) {
    anos <- anos[1]
  }
  
  body <- getPostRequestBody(obj$Linhas[region_index], obj$Indicadores[subindicator_index], anos)
  data <- tabnet_csv_retrieval(obj$POST_url, body)
  return(data)
  
}

#' Regionalization Check
#'
#' Check if a certain indicator supports desired regionalization.
#' @param obj TABNET object to be checked. @seealso [make_tabnet_obj()]
#' @param region String indicating regionalization to be tested.
#' @keywords tabnet
#' @return Boolean, indicating whether the TABNET object has desired regionalization among its avaiable ones.
#' @examples
isRegionAvailable <- function(obj, region = "Município") {
  return (region %in% obj$NomesLinhas)
}

#' Retrieve All Indicators
#'
#' Retrieve a gathered data frame with ll indicators from a TABNET link, given a regionalization.
#' @param region String specifying desired regionalization. Defaults to "Município"
#' @param url TABNET url, defaults to Sao Paulo TABNET.
#' @param timeout Time for connection to expire, in seconds.
#' @keywords tabnet
#' @export
#' @return Data frame with values from all indicators, from all available periods, regionalized.
#' @examples
#' \dontrun{df <- fetch_all(region = "Região de Saúde")}
fetch_all <- function(region = "Município", url = "http://portal.saude.sp.gov.br/links/matriz", timeout = 1) {
  links <- tabnet_index()$Links
  tabnet_objs <- lapply(links, make_tabnet_obj)
  available_indicators <- data.frame(cbind(c(1:length(links)), unlist(lapply(tabnet_objs, isRegionAvailable, region = region))))
  colnames(available_indicators) <- c("indicator_id", "available")
  available_indicators <- available_indicators$indicator_id[available_indicators$available == 1]
  s_tabnet_df_retrieval <- safely(tabnet_df_retrieval)
  
  df <- NULL
  for (i in available_indicators) {
    obj <- make_tabnet_obj(links[i])
    mun_index <- min(which(obj$NomesLinhas == region))
    
    result <- s_tabnet_df_retrieval(obj, line_index = mun_index, years_index = NULL)
    if (is.null(result$error)) {
      new_data <- result$result
      name <- obj$Nome
      name_start_index <- unlist(gregexpr("-", name))
      name <- substr(name, name_start_index + 2, nchar(name))
      column_name <- paste0(name, "_", obj$NomesIndicadores[length(obj$NomesIndicadores)])
      column_name <- gsub("\ ", ".", column_name) #remove spaces
      column_name <- iconv(column_name, from="UTF-8", to="ASCII//TRANSLIT")
      new_data$Indicador <- as.factor(column_name)
      if (is.null(df)) {
        df <- new_data
      } else {
        df <- rbind(df, new_data)
      }
    }
    
  }
  df <- df[,c(1, 2, 5, 3, 4)]
  return(df)
}

