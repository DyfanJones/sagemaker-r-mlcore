#' @importFrom urltools url_parse

`%||%` <- function(x, y) if (is.null(x)) return(y) else return(x)

parse_url = function(url){
  url = ifelse(is.null(url) | is.logical(url) , "", url)
  url = ifelse(grepl("/", url), url, sprintf("/%s", url))
  urltools::url_parse(url)
}

