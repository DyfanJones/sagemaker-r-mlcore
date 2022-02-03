# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/dev/src/sagemaker/training_compiler/config.py


#' @import R6
#' @import sagemaker.core

#' @title The configuration class for accelerating SageMaker training jobs through compilation.
#' @description SageMaker Training Compiler speeds up training by optimizing the model execution graph.
#' @export
TrainingCompilerConfig = R6Class("TrainingCompilerConfig",
  public = list(

    #' @field DEBUG_PATH
    #' Placeholder
    DEBUG_PATH = "/opt/ml/output/data/compiler/",

    #' @field SUPPORTED_INSTANCE_CLASS_PREFIXES
    #' Placeholder
    SUPPORTED_INSTANCE_CLASS_PREFIXES = c("p3", "g4dn", "p4"),

    #' @field HP_ENABLE_COMPILER
    #' Placeholder
    HP_ENABLE_COMPILER = "sagemaker_training_compiler_enabled",

    #' @field HP_ENABLE_DEBUG
    #' Placeholder
    HP_ENABLE_DEBUG = "sagemaker_training_compiler_debug_mode",

    #' @description This class initializes a ``TrainingCompilerConfig`` instance.
    #'              Pass the output of it to the ``compiler_config``
    #'              parameter of the :class:`~sagemaker.huggingface.HuggingFace`
    #'              class.
    #' @param enabled (bool): Optional. Switch to enable SageMaker Training Compiler.
    #'              The default is \code{True}.
    #' @param debug (bool): Optional. Whether to dump detailed logs for debugging.
    #'              This comes with a potential performance slowdown.
    #'              The default is \code{FALSE}.
    initialize = function(enabled=TRUE,
                          debug=FALSE){
      self$enabled = enabled
      self$debug = debug

      self$disclaimers_and_warnings()
    },

    #' @description Disclaimers and warnings.
    #'              Logs disclaimers and warnings about the
    #'              requested configuration of SageMaker Training Compiler.
    disclaimers_and_warnings = function(){
      if (isTRUE(self$enabled) && isTRUE(self$debug)){
        LOGGER$warn(paste(
          "Debugging is enabled.",
          "This will dump detailed logs from compilation to %s",
          "This might impair training performance."),
          self$DEBUG_PATH
        )
      }
    },

    #' @description Converts configuration object into hyperparameters.
    #' @return (list): A portion of the hyperparameters passed to the training job as a list
    .to_hyperparameter_list = function(){
      compiler_config_hyperparameters = list(
        self$enabled, self$debug
      )
      names(compiler_config_hyperparameters) = c(self$HP_ENABLE_COMPILER,  self$HP_ENABLE_DEBUG)
      return(compiler_config_hyperparameters)
    },

    #' @description Checks if SageMaker Training Compiler is configured correctly.
    #' @param image_uri (str): A string of a Docker image URI that's specified
    #'              to :class:`~sagemaker.huggingface.HuggingFace`.
    #'              If SageMaker Training Compiler is enabled, the HuggingFace estimator
    #'              automatically chooses the right image URI. You cannot specify and override
    #'              the image URI.
    #' @param instance_type (str): A string of the training instance type that's specified
    #'              to :class:`~sagemaker.huggingface.HuggingFace`.
    #'              The `validate` classmethod raises error
    #'              if an instance type not in the ``SUPPORTED_INSTANCE_CLASS_PREFIXES`` list
    #'              or ``local`` is passed to the `instance_type` parameter.
    #' @param distribution (dict): A dictionary of the distributed training option that's specified
    #'              to :class:`~sagemaker.huggingface.HuggingFace`.
    #'              SageMaker's distributed data parallel and model parallel libraries
    #'              are currently not compatible with SageMaker Training Compiler.
    validate = function(image_uri,
                        instance_type,
                        distribution){
      if (!grepl("local",instance_type)) {
        requested_instance_class = split_str(instance_type, "\\.")[[2]]  # Expecting ml.class.size
        if (!any(sapply(self$SUPPORTED_INSTANCE_CLASS_PREFIXES, startsWith, x = requested_instance_class))){
          error_helper_string = "Unsupported Instance class %s. SageMaker Training Compiler only supports %s"
          error_helper_string = sprintf(error_helper_string,
            requested_instance_class, cls.SUPPORTED_INSTANCE_CLASS_PREFIXES
          )
          ValueError$new(error_helper_string)
        }
      } else if (instance_type == "local") {
        error_helper_string = paste(
          "The local mode is not supported by SageMaker Training Compiler.",
          "It only supports the following GPU instances: p3, g4dn, and p4."
        )
        ValueError$new(error_helper_string)
      }

      if (!missing(image_uri)) {
        error_helper_string = paste(
          "Overriding the image URI is currently not supported ",
          "for SageMaker Training Compiler.",
          "Specify the following parameters to run the Hugging Face training job ",
          "with SageMaker Training Compiler enabled:",
          "transformer_version, tensorflow_version or pytorch_version, and compiler_config."
        )
        ValueError$new(error_helper_string)
      }

      if (!missing(distribution) && "smdistributed" %in% names(distribution)){
        ValueError$new(
          "SageMaker distributed training configuration is currently not compatible with ",
          "SageMaker Training Compiler."
        )
      }
    }
  )
)
