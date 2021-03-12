library(dplyr)
library(ggplot2)

#' Generate Map plot from indicator data, with North indicator and scale
#'
#' Other version of [tabnet_map()], with North indicators and scale bar. Requires ggspatial package.
#' @param indicator_index Indicator index number, @seealso [tabnet_index()].
#' @param region String describing how should data be regionalized.
#' @param subindicator Among the indicator, which subindicator should be used? Defaults to the last index.
#' @param years Retrieve data from one or more specific years?
#' @param label Boolean. Should the regions be labeled? Defaults to false.
#' @param nBins Number of bins to discretize desired indicator value.
#' @param palette RColorBrewer palette.
#' @param title Do you want to provide a specific title? Otherwise, function will generate one itself.
#' @param scalebar Boolean, indicating whether a scale bar should be present on the map. Defaults to true.
#' @param north_arrow Boolean, indicating whether a North arrow should be added on the map. Defaults to true.
#' @param scale_width ggspatial parameter, indicating scale bar width.
#' @keywords tabnet
#' @return ggplot2 object
#' @export
#' @examples

tabnet_map2 <- function(indicator_index, region = "Município", subindicator = NULL, years = NULL,
                       label = FALSE, nBins = NULL, palette = "Purples", title = NULL, scalebar = TRUE,
                       north_arrow = TRUE, scale_width = 0.3) {
  require(ggspatial)
  #ShapeFiles
  sp_cities <- readRDS(system.file("extdata", "/shapefile/simpl_sp.rds", package = "rtabnetsp"))
  sp_rras <- readRDS(system.file("extdata", "/shapefile/simpl_rras.rds", package = "rtabnetsp"))
  sp_drs <- readRDS(system.file("extdata", "/shapefile/simpl_drs.rds", package = "rtabnetsp"))
  sp_reg_saude <- readRDS(system.file("extdata", "/shapefile/simpl_reg_saude.rds", package = "rtabnetsp"))


  data <- tabnet_df(indicator_index, region, subindicator, years, onlyMostRecent = TRUE)
  obj <- make_tabnet_obj(tabnet_index()$Links[indicator_index])
  indicator <- ifelse(is.null(subindicator), length(obj$Indicadores), subindicator)
  geometry <- switch (region,
                      "Município" = sp_cities,
                      "DRS" = sp_drs,
                      "RRAS" = sp_rras,
                      "Região de Saúde" = sp_reg_saude
  )

  key <- switch (region,
                 "Município" = "cod_mun",
                 "DRS" = "cod_drs",
                 "RRAS" = "cod_rras",
                 "Região de Saúde" = "cod_rs")

  colnames(data)[c(1, 2)] <- c(key, "Region")
  plotData <- merge(geometry, data, by=key)
  g <- NULL
  if (!is.null(nBins)) {
    plotData <- plotData %>%
      mutate(valor_discreto = cut_number(Valor, nBins))

    g <- ggplot(data=plotData) +
      geom_sf(aes(fill=valor_discreto, geometry = geometry)) +
      scale_fill_brewer(obj$NomesIndicadores[indicator], palette = palette)
  } else {
    g <- ggplot(data=plotData) +
      geom_sf(aes(fill=Valor, geometry = geometry)) +
      scale_fill_distiller(obj$NomesIndicadores[indicator], palette = palette)
  }

  g <- g + xlab("") + ylab("") +
    ggtitle(ifelse(is.null(title), paste0(obj$NomesIndicadores[indicator], ", por ",
                                          region, ", em ", levels(plotData$Ano)),
                   title)) +
    theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "#FFFFFF"),
          plot.title = element_text(hjust = 0.5, size = 12), axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8), axis.line = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank())

  if (label) {
    g <- g + geom_sf_text(aes(label = Region), size = 2)
  }

  #north indicator
  if (north_arrow) {
    g <- g +
      annotation_north_arrow(location = "br", which_north = "true",
                             height = unit(1, "cm"), width = unit(1, "cm"),
                             style = north_arrow_orienteering())
  }

  if (scalebar) {
    g <- g +
      annotation_scale(location = "bl", width_hint = scale_width)
  }


  g
}
