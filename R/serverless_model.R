# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/serverless/model.py

#' @include r_utils.R
#' @include serverless_predictor.R

#' @import R6
#' @import sagemaker.common
#' @import paws

#' @title LambdaModel class
#' @description A model that can be deployed to Lambda
#' @export
LambdaModel = R6Class("LambdaModel",
  inherit = sagemaker.common::ModelBase,
  public = list(

    #' @description Initialize instance attributes.
    #' @param image_uri : URI of a container image in the Amazon ECR registry. The image
    #'              should contain a handler that performs inference.
    #' @param role : The Amazon Resource Name (ARN) of the IAM role that Lambda will assume
    #'              when it performs inference
    #' @param client : The Lambda client used to interact with Lambda.
    initialize = function(image_uri,
                          role,
                          client=NULL){
      private$.client = client %||% paws::lambda()
      private$.image_uri = image_uri
      private$.role = role
    },

    #' @description Create a Lambda function using the image specified in the constructor.
    #' @param function_name : The name of the function.
    #' @param timeout : The number of seconds that the function can run for before being terminated.
    #' @param memory_size : The amount of memory in MB that the function has access to.
    #' @param wait : If true, wait until the deployment completes (default: True).
    #' @return A LambdaPredictor instance that performs inference using the specified image.
    deploy = function(function_name,
                      timeout,
                      memory_size,
                      wait = TRUE){
      response = private$.client$create_function(
        FunctionName=function_name,
        PackageType="Image",
        Role=private$.role,
        Code=list(
          "ImageUri"=private$.image_uri),
        Timeout=timeout,
        MemorySize=memory_size
      )

      if (!wait)
        return(LambdaPredictor$new(function_name, client=private$.client))

      # Poll function state.
      polling_interval = 5
      while (response[["State"]] == "Pending"){
        Sys.sleep(polling_interval)
        response = private$.client$get_function_configuration(FunctionName=function_name)
      }

      if (response[["State"]] != "Active")
        RuntimeError$new(sprintf("Failed to deploy model to Lambda: %s", response[["StateReason"]]))

      return(LambdaPredictor$new(function_name, client=private$.client))
    },

    #' @description Destroy resources associated with this model.
    #'              This method does not delete the image specified in the constructor. As
    #'              a result, this method is a no-op.
    delete_model = function(){
      return(invisible(NULL))
    }
  ),

  private = list(
    .client = NULL,
    .image_uri = NULL,
    .role = NULL
  )
)
