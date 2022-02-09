#' @importFrom urltools url_parse

`%||%` <- function(x, y) if (is.null(x)) return(y) else return(x)

parse_url = function(url){
  url = ifelse(is.null(url) | is.logical(url) , "", url)
  url = ifelse(grepl("/", url), url, sprintf("/%s", url))
  urltools::url_parse(url)
}

# Correctly mimic python append method for list
# Full credit to package rlist: https://github.com/renkun-ken/rlist/blob/2692e064fc7b6cc7fe7079b3762df37bc25b3dbd/R/list.insert.R#L26-L44
list.append = function (.data, ...) {
  if (is.list(.data)) c(.data, list(...)) else c(.data, ..., recursive = FALSE)
}
