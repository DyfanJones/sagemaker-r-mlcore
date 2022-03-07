# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/amazon/common.py

# Note: this file has been split into components sagemaker.core and this package

#' @include serializers.R
#' @include deserializers.R

#' @import R6
#' @import sagemaker.core
#' @importFrom methods is as

#' @title RecordSerializer Class
#' @description Serialize a matrices and array for an inference request.
#' @export
RecordSerializer = R6Class("RecordSerializer",
  inherit = SimpleBaseSerializer,
  public = list(

    #' @description Intialize RecordSerializer class
    #' @param content_type (str): The MIME type to signal to the inference endpoint when sending
    #'              request data (default: "application/x-recordio-protobuf").
    initialize = function(content_type="application/x-recordio-protobuf"){
      super$initialize(content_type=content_type)
      initProtoBuf()
    },

    #' @description Serialize a matrix/array into a buffer containing RecordIO records.
    #' @param data (matrix): The data to serialize.
    #' @return raw: A buffer containing the data serialized as records.
    serialize = function(data){
      if(is.vector(data))
        data = as.array(data)

      if(length(dim(data)) == 1)
        data = matrix(data, 1, dim(data)[1])

      obj = raw(0)
      buf = rawConnection(obj, open = "wb")
      on.exit(close(buf))

      write_matrix_to_dense_tensor(buf, data)

      return(rawConnectionValue(buf))
    }
  )
)

#' @title RecordDeserializer Class
#' @description Deserialize RecordIO Protobuf data from an inference endpoint.
#' @export
RecordDeserializer = R6Class("RecordDeserializer",
  inherit = SimpleBaseDeserializer,
  public = list(

    #' @description Intialize RecordDeserializer class
    #' @param accept (union[str, tuple[str]]): The MIME type (or tuple of allowable MIME types) that
    #'              is expected from the inference endpoint (default:
    #'              "application/x-recordio-protobuf").
    initialize = function(accept="application/x-recordio-protobuf"){
      super$initialize(accept=accept)
      initProtoBuf()
    },

    #' @description Deserialize RecordIO Protobuf data from an inference endpoint.
    #' @param data (object): The protobuf message to deserialize.
    #' @param content_type (str): The MIME type of the data.
    #' @return list: A list of records.
    deserializer = function(data, content_type){
      tryCatch(read_records_io(data),
               finally = function(f) close(data))
    }
  )
)
