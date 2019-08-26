library(stringi)

#' Indicator Check
#'
#' Clean an indicator string.
#' @param indicators Indicator strings. 
#' @keywords tabnet
#' @return Return clean indicator strings.
#' @examples
tabnet_indicator_catch <- function(indicators) {
  fixed_indicators <- indicators
  start <- regexpr("\"", fixed_indicators)
  fixed_indicators <- substr(fixed_indicators, start + 1, sapply(as.character(fixed_indicators), nchar))
  end <- regexpr("\"", fixed_indicators)
  fixed_indicators <- substr(fixed_indicators, 1, end-1)
  return(fixed_indicators)
}

#' Tabnet Hexadecimal Escape Function
#'
#' Adapt parts of the escaped string so it is uploadable to TABNET.
#' @param indicators Indicator strings. 
#' @keywords tabnet
#' @return String with adapted escaped characters.
#' @examples
tabnet_hex_escape <- function(indicators) {
  clean_indicators <- NULL
  for (i in c(1:length(indicators))) {
    #uppercase codes
    linha <- indicators[i]
    indexes <- unlist(gregexpr("\\u00", linha))
    if (indexes[1] != -1) {
      for (j in c(1:length(indexes))) {
        linha <- paste0(substr(linha, 1, indexes[j] + 2), 
                        toupper(substr(linha, indexes[j] + 3, indexes[j] + 4)),
                        substr(linha, indexes[j] + 5, nchar(linha)))
      }
    }
    
    clean_indicators <- c(clean_indicators, linha)
  }
  return (clean_indicators)
}

#' TABNET Special Character Check
#'
#' Some characters need special attention when generating an encoded string to TABNET.
#' @param escaped_string String already escaped.
#' @keywords tabnet
#' @return The same string, with modifications if special characters are present.
#' @examples
tabnet_special_character_check <- function(escaped_string) {
  fixed_string <- escaped_string
  
  #percentage check
  fixed_string <- gsub("%", "%25", fixed_string)
  
  #parenthesis check
  fixed_string <- gsub("(", "%28", fixed_string, fixed=TRUE)
  fixed_string <- gsub(")", "%29", fixed_string, fixed=TRUE)
  
  #greater and less than check
  fixed_string <- gsub("&lt;", "%3C", fixed_string)
  fixed_string <- gsub("&gt;", "%3E", fixed_string)
  
  return(fixed_string)
}

#' W1252 Encode
#'
#' Process the string to the W1252 encoding, used by the TABNET System.
#' @param decoded The regular string, before encoding. 
#' @keywords tabnet
#' @export
#' @return An encoded string according to TABNET's W1252 pattern.
#' @examples
W1252.encode <- function(decoded) {
  encoded <- decoded
  encoded <- stri_escape_unicode(encoded)
  encoded <- tabnet_hex_escape(encoded)
  encoded <- tabnet_special_character_check(encoded)
  encoded <- gsub("\\u00", "%", encoded, fixed=TRUE)
  return (encoded)
}

#' TABNET Breakline fix
#'
#' Fix the line breaks of indicators.
#' @param lines Lines to be fixed. 
#' @keywords tabnet
#' @return The same line, without break characters.
#' @examples
tabnet_breakline_fix <- function(lines) {
  fixed_lines <- lines
  fixed_lines <- gsub("\r\n", "", fixed_lines)
  return (fixed_lines)
}

#' Concatenate TABNET Files
#'
#' When asking for more than one file, the names must be concatenated in a certain pattern.
#' @param elements The files to be concatenated. 
#' @param sep The separator between the file names.
#' @keywords tabnet
#' @return A string concatenating all elements provided using the separator.
#' @examples
tabnet_concatFormat <- function(elements, sep="Arquivos") {
  formatted <- NULL
  for (i in c(1:length(elements))) {
    formatted <- paste0(formatted, "&", sep, "=", elements[i])
  }
  return (formatted)
}