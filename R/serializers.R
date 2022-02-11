# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/serializers.py

#' @include r_utils.R

#' @import R6
#' @import sagemaker.core
#' @import data.table
#' @importFrom jsonlite write_json stream_out

connection_value = function(con){
  get(sprintf("%sValue", class(con)[[1]]))(con)
}

#' @title Default BaseSerializer Class
#' @description  All serializer are children of this class. If a custom
#'               serializer is desired, inherit this class.
#' @export
BaseSerializer = R6Class("BaseSerializer",
  public = list(

   #' @description Take data of various data formats and serialize them into CSV.
   #' @param data (object): Data to be serialized
   #' @return object: Serialized data used for a request.
   serialize = function(data) {
     NotImplementedError$new("I'm an abstract interface method")
   },

   #' @description format class
   format = function(){
      format_class(self)
   }
  ),
  active = list(
     #' @field CONTENT_TYPE
     #' The MIME type of the data sent to the inference endpoint.
     CONTENT_TYPE = function(){
       NotImplementedError$new("I'm an abstract interface method")
     }
  )
)

#' @title Abstract base class for creation of new serializers.
#' @description This class extends the API of :class:~`sagemaker.serializers.BaseSerializer` with more
#'              user-friendly options for setting the Content-Type header, in situations where it can be
#'              provided at init and freely updated.
#' @export
SimpleBaseSerializer = R6Class("SimpleBaseSerializer",
   inherit = BaseSerializer,
   public = list(

      #' @field content_type
      #' The data MIME type
      content_type = NULL,

      #' @description Initialize a ``SimpleBaseSerializer`` instance.
      #' @param content_type (str): The MIME type to signal to the inference endpoint when sending
      #'              request data (default: "application/json").
      initialize = function(content_type = "application/json"){
         if (!is.character(content_type)){
            ValueError$new(
              "content_type must be a string specifying the MIME type of the data sent in ",
              sprintf("requests: e.g. 'application/json', 'text/csv', etc. Got %s", content_type)
            )
         }
         self$content_type = content_type
      },

      #' @description Take data of various data formats and serialize them into CSV.
      #' @param data (object): Data to be serialized
      serialize = function(data) {
        NotImplementedError$new("I'm an abstract interface method")
      }
   ),
   active = list(
   #' @field CONTENT_TYPE
   #' The data MIME type set in the Content-Type header on prediction endpoint requests.
   CONTENT_TYPE = function(){
      return(self$content_type)
      }
   )
)

#' @title CSVSerializer Class
#' @description Make Raw data using text/csv format
#' @export
CSVSerializer = R6Class("CSVSerializer",
  inherit = SimpleBaseSerializer,
  public = list(
    #' @description Initialize a ``CSVSerializer`` instance.
    #' @param content_type (str): The MIME type to signal to the inference endpoint when sending
    #'              request data (default: "text/csv").
    initialize = function(content_type="text/csv"){
       super$initialize(content_type=content_type)
    },
    #' @description Take data of various data formats and serialize them into CSV.
    #' @param data (object): Data to be serialized. Any list of same length vectors; e.g. data.frame and data.table.
    #'               If matrix, it gets internally coerced to data.table preserving col names but not row names
    serialize = function(data) {
      # read file
      if(is.character(data) && length(data) == 1 && file.exists(data)){
        return(readBin(data, "raw", n = file.size(data)))
      }
      # read connection
      if(inherits(data, "connection")){
        return(connection_value(data))
      }
      if (is.matrix(data) | is.data.frame(data)) {
        f = tempfile()
        on.exit(unlink(f))
        fwrite(data, f, col.names = FALSE, showProgress = FALSE)
        obj = readBin(f, "raw", n = file.size(f))
      } else {
        list_data = sapply(if(is.list(data)) data else list(data), private$.serialize_row)
        obj = paste(list_data, collapse = "\n")
      }
      return(obj)
    }
  ),
  private = list(
    .serialize_row = function(data){
      if(length(data) == 0){
        ValueError$new("Cannot serialize empty array")
      }
      if(is.character(data)){
        return(data)
      }
      return(paste(data, collapse=","))
    }
  )
)

#' @title NumpySerializer Class
#' @description Serialize data of various formats to a numpy npy file format.
#'              This serializer class uses python numpy package to serialize,
#'              R objects through the use of the `reticulate` package.
#' @export
NumpySerializer = R6Class("NumpySerializer",
  inherit = SimpleBaseSerializer,
  public = list(

    #' @field dtype
    #' The dtype of the data
    dtype = NULL,

    #' @field np
    #' Initialized python numpy package
    np = NULL,

    #' @description Initialize a ``NumpySerializer`` instance.
    #' @param content_type (str): The MIME type to signal to the inference endpoint when sending
    #'              request data (default: "application/x-npy").
    #' @param dtype (str): The `dtype` of the data. `reticulate` auto maps to python, please set R class
    #'              to be serialized.
    initialize = function(dtype=NULL,
                          content_type="application/x-npy"){
      if(!requireNamespace('reticulate', quietly=TRUE))
        SagemakerError$new('Please install `reticulate` package and try again')
      super$initialize(content_type = content_type)
      if(!is.null(dtype)) {
        ValueError$new("`reticulate` auto maps to python. Please set class in R.")
      }
      self$np = reticulate::import("numpy")
    },

    #' @description Serialize data to a buffer using the .npy format.
    #' @param data (object): Data to be serialized. Can be a NumPy array, list,
    #'              file, or buffer.
    #' @return (raw): A buffer containing data serialized in the .npy format.
    serialize = function(data) {
      # read file
      if(is.character(data) && length(data) == 1 && file.exists(data)){
        return(readBin(data, "raw", n = file.size(data)))
      }
      # read connection
      if(inherits(data, "connection")){
        return(connection_value(data))
      }
      if(inherits(data, c("array", "vector"))){
        if(length(data) == 0)
          ValueError$new("Cannot serialize empty array.")
        data = private$.serialize_array(data)
      }
      if(inherits(data, "data.frame")){
        if(nrow(data) == 0)
          ValueError$new("Cannot serialize empty data.frame.")
        data = as.matrix(data)
      }
      if(is.list(data)) {
        if (length(data) == 0)
          ValueError$new("Cannot serialize empty array.")
        data = private$.serialize_array(data)
      }
      f = tempfile(fileext = ".npy")
      on.exit(unlink(f))
      self$np$save(f, data)
      return(readBin(f, "raw", n = file.size(f)))
    }
  ),
  private = list(
    .serialize_array = function(data){
      if(is.list(data))
        return(matrix(unlist(data), ncol = lengths(data)[[1]], byrow = TRUE))
      return(data)
    }
  )
)

#' @title JSONSerializer Class
#' @description Serialize data to a JSON formatted string.
#' @export
JSONSerializer = R6Class("JSONSerializer",
  inherit = SimpleBaseSerializer,
  public = list(

    #' @description Serialize data of various formats to a JSON formatted string.
    #' @param data (object): Data to be serialized.
    #' @return (raw): The data serialized as a JSON string.
    serialize = function(data){
      # read file
      if(is.character(data) && length(data) == 1 && file.exists(data)){
        return(readBin(data, "raw", n = file.size(data)))
      }
      # read connection
      if(inherits(data, "connection")){
        return(connection_value(data))
      }
      con = rawConnection(raw(0), "r+")
      on.exit(close(con))
      jsonlite::write_json(data, con, dataframe = "columns", auto_unbox = F)
      return(rawConnectionValue(con))
    }
  )
)

#' @title Serialize data by returning data without modification.
#' @description This serializer may be useful if, for example, you're sending raw bytes such as from an image
#'              file's .read() method.
#' @export
IdentitySerializer = R6Class("IdentitySerializer",
  inherit = SimpleBaseSerializer,
  public = list(

    #' @description Initialize an ``IdentitySerializer`` instance.
    #' @param content_type (str): The MIME type to signal to the inference endpoint when sending
    #'              request data (default: "application/octet-stream").
    initialize = function(content_type="application/octet-stream"){
      super$initialize(content_type = content_type)
    },

    #' @description Return data without modification.
    #' @param data (object): Data to be serialized.
    #' @return object: The unmodified data.
    serialize = function(data){
      return(data)
    }
  )
)

#' @title JSONLinesSerializer Class
#' @description Serialize data to a JSON Lines formatted string.
#' @export
JSONLinesSerializer = R6Class("IdentitySerializer",
  inherit = SimpleBaseSerializer,
  public = list(

    #' @description Initialize a ``JSONLinesSerializer`` instance.
    #' @param content_type (str): The MIME type to signal to the inference endpoint when sending
    #'              request data (default: "application/jsonlines").
    initialize = function(content_type="application/jsonlines"){
      super$initialize(content_type = content_type)
    },

    #' @description Serialize data of various formats to a JSON Lines formatted string.
    #' @param data (object): Data to be serialized. The data can be a string,
    #'              iterable of JSON serializable objects, or a file-like object.
    #' @return str: The data serialized as a string containing newline-separated
    #'              JSON values.
    serialize = function(data){
      # read file
      if(is.character(data) && length(data) == 1 && file.exists(data)){
        return(readBin(data, "raw", n = file.size(data)))
      }
      # read connection
      if(inherits(data, "connection")){
        return(connection_value(data))
      }
      con = rawConnection(raw(0), "r+")
      on.exit(close(con))
      jsonlite::stream_out(data, con = con, verbose = FALSE)
      return(rawConnectionValue(con))
    }
  )
)

#' @title SparseMatrixSerializer Class
#' @description Serialize a sparse matrix to a buffer using the .npz format.
#' @export
SparseMatrixSerializer = R6Class("SparseMatrixSerializer",
  inherit = SimpleBaseSerializer,
  public = list(

    #' @field scipy
    #' Python scipy package
    scipy = NULL,

    #' @description Initialize a ``SparseMatrixSerializer`` instance.
    #' @param content_type (str): The MIME type to signal to the inference endpoint when sending
    #'              request data (default: "application/x-npz").
    initialize = function(content_type="application/x-npz"){
      if(!requireNamespace('reticulate', quietly=TRUE))
        SagemakerError$new('Please install `reticulate` package and try again')
      super$initialize(content_type=content_type)
      self$scipy = reticulate::import("scipy")
    },

    #' @description Serialize a sparse matrix to a buffer using the .npz format.
    #'              Sparse matrices can be in the ``csc``, ``csr``, ``bsr``, ``dia`` or
    #'              ``coo`` formats.
    #' @param data (sparseMatrix): The sparse matrix to serialize.
    #' @return raw: A buffer containing the serialized sparse matrix.
    serialize = function(data){
      # read file
      if(is.character(data) && length(data) == 1 && file.exists(data)){
        return(readBin(data, "raw", n = file.size(data)))
      }
      # read connection
      if(inherits(data, "connection")){
        return(connection_value(data))
      }
      f = tempfile(fileext = ".npz")
      on.exit(unlink(f))
      self$scipy$sparse$save_npz(f, data)
      return(readBin(f, what = "raw", n = file.size(f)))
    }
  )
)

#' @title LibSVMSerializer Class
#' @description Serialize data of various formats to a LibSVM-formatted string.
#'              The data must already be in LIBSVM file format:
#'              <label> <index1>:<value1> <index2>:<value2> ...
#'              It is suitable for sparse datasets since it does not store zero-valued
#'              features.
#' @export
LibSVMSerializer = R6Class("LibSVMSerializer",
  inherit = SimpleBaseSerializer,
  public = list(

    #' @description Initialize a ``LibSVMSerializer`` instance.
    #' @param content_type (str): The MIME type to signal to the inference endpoint when sending
    #'              request data (default: "text/libsvm").
    initialize = function(content_type="text/libsvm"){
      super$initialize(content_type = content_type)
      if(!requireNamespace('readsparse', quietly=TRUE))
        SagemakerError$new('Please install readsparse package and try again')
      },

    #' @description Serialize data of various formats to a LibSVM-formatted string.
    #' @param data (object): Data to be serialized. Can be a string, a
    #'              file-like object, sparse matrix, or a list (format: list(<sparse matrix>, <label>)).
    #' @return str: The data serialized as a LibSVM-formatted string.
    serialize = function(data) {
      # read file
      if(is.character(data) && length(data) == 1 && file.exists(data)){
        return(readBin(data, "raw", n = file.size(data)))
      }
      # read connection
      if(inherits(data, "connection")){
        return(connection_value(data))
      }
      if(is.character(data)){
        return(data)
      }

      f = tempfile(fileext = ".svmlight")
      on.exit(unlink(f))

      if(is.list(data)){
        if(!inherits(data[[1]], private$.dtype))
          ValueError$new(sprintf(
            "data is in invalid list format, expecting ['%s']",
            paste(private$.dtype, collapse = "', '")
            )
          )
        if(!is.vector(data[[2]]))
          ValueError$new(
            "data is in invalid list format, please check documentation"
          )
        readsparse::write.sparse(file = f, X = data[[1]], y=data[[2]])
      } else if(inherits(data, private$.dtype)){
        readsparse::write.sparse(file = f, X = data, y = rep(0, nrow(data)))
      } else {
        ValueError$new(sprintf(
          "Unable to handle input format: %s", class(data)[[1]])
        )
      }
      return(readBin(f, what = "raw", n = file.size(f)))
    }
  ),
  private = list(
    .dtype = c(
      'dgRMatrix',
      'dgTMatrix',
      'dgCMatrix',
      'ngRMatrix',
      'ngTMatrix',
      'ngCMatrix',
      'matrix.csr',
      'matrix.coo',
      'matrix.csc',
      'matrix',
      'data.frame',
      'numeric',
      'integer',
      'dsparseVector'
    )
  )
)
