# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/serverless/predictor.py

#' @include r_utils.R
#' @include deserializers.R
#' @include serializers.R
#' @include predictor.R

#' @import R6
#' @import paws

#' @title LamdbaPredictor
#' @description A deployed model hosted on Lambda.
#' @export
LambdaPredictor = R6Class("LambdaPredictor",
  inherit = PredictorBase,
  public = list(

    #' @description Initialize instance attributes.
    #' @param function_name : The name of the function.
    #' @param client : The Lambda client used to interact with Lambda.
    initialize = function(function_name,
                          client=NULL){
      private$.client = client %||% paws::lambda()
      private$.function_name = function_name
      private$.serializer = JSONSerializer$new()
      private$.deserializer = JSONDeserializer$new()
    },

    #' @description Invoke the Lambda function specified in the constructor.
    #'              This function is synchronous. It will only return after the function
    #'              has produced a prediction.
    #' @param data : The data sent to the Lambda function as input.
    #' @return The data returned by the Lambda function.
    predict = function(data){
      response = private$.client$invoke(
        FunctionName=private$.function_name,
        InvocationType="RequestResponse",
        Payload=private$.serializer$serialize(data)
      )
      return(private$.deserializer$deserialize(
        response[["Payload"]],
        response[["ResponseMetadata"]][["HTTPHeaders"]][["content-type"]])
      )
    },

    #' @description Destroy the Lambda function specified in the constructor.
    delete_predictor = function(){
      return(private$.client$delete_function(FunctionName=private$.function_name))
    }
  ),
  active = list(

    #' @field content_type
    #' The MIME type of the data sent to the Lambda function.
    content_type = function(){
      return(private$.serializer$CONTENT_TYPE)
    },

    #' @field accept
    #' The content type(s) that are expected from the Lambda function.
    accept = function(){
      return(private$.deserializer$ACCEPT)
    },

    #' @field function_name
    #' The name of the Lambda function this predictor invokes.
    function_name = function(){
      return(private$.function_name)
    }
  )
)
