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

# Full credit to R6:
# https://github.com/r-lib/R6/blob/main/R/env_utils.R#L4-L11
assign_func_envs <- function(objs, target_env) {
  if (is.null(target_env)) return(objs)

  lapply(objs, function(x) {
    if (is.function(x)) environment(x) <- target_env
    x
  })
}

# Ability to call class methods
# Full credit to R6 new method
# modified: https://github.com/r-lib/R6/blob/main/R/new.R
r6_class_method = function(r6_class_gen, parent_env = parent.frame()){
  enclos_env <- new.env(parent = parent_env, hash = FALSE)
  public_bind_env <- new.env(parent = emptyenv(), hash = FALSE)
  private_bind_env <- new.env(parent = emptyenv(), hash = FALSE)

  enclos_env$self <- public_bind_env
  enclos_env$private <- private_bind_env

  public_objs = append(r6_class_gen$public_fields, r6_class_gen$public_methods)
  public_methods <- assign_func_envs(public_objs, enclos_env)

  private_objs = append(r6_class_gen$private_fields, r6_class_gen$private_methods)
  private_method <- assign_func_envs(private_objs, enclos_env)


  list2env(public_methods, envir = public_bind_env)
  list2env(private_method, envir = private_bind_env)

  return(enclos_env)
}
