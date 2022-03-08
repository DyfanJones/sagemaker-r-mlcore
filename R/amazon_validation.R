# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/amazon/validation.py

#' @import R6

#' @title Helper class
#' @description Check expression
#' @keywords internal
#' @export
Validation = R6Class("Validation",
  public = list(

    #' @description greater than
    #' @param minimum (object): placeholder
    gt = function(minimum){
      return(function(value){value > minimum})
    },

    #' @description greater equals
    #' @param minimum (object): placeholder
    ge = function(minimum){
      return(function(value){value >= minimum})
    },

    #' @description less than
    #' @param maximum (object): placeholder
    lt = function(maximum){
      return(function(value){value < maximum})
    },

    #' @description less equals
    #' @param maximum (object): placeholder
    le = function(maximum){
      return(function(value){value <= maximum})
    },

    #' @description is in
    #' @param expected (object): placeholder
    isin = function(expected){
      return(function(value){value %in% expected})
    },

    #' @description is data type
    #' @param expected (object): placeholder
    istype = function(expected){
      return(function(value){inherits(valuem, expected)})
    }
  )
)
