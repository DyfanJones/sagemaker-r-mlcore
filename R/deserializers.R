# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/deserializers.py

#' @include r_utils.R

#' @import R6
#' @import data.table
#' @importFrom jsonlite fromJSON stream_in

#' @title Default BaseDeserializer Class
#' @description All BaseDeserializer are children of this class. If a custom
#'              BaseDeserializer is desired, inherit this class.
#' @family serializer
#' @export
BaseDeserializer = R6Class("BaseDeserializer",
  public = list(

    #' @description  Deserialize data received from an inference endpoint.
    #' @param stream (botocore.response.StreamingBody): Data to be deserialized.
    #' @param content_type (str): The MIME type of the data.
    #' @return object: The data deserialized into an object.
    deserialize = function(stream, content_type){
      NotImplementedError$new("I'm an abstract interface method")
    },

    #' @description format class
    format = function(){
      format_class(self)
    }
  ),
  active = list(

    #' @field ACCEPT
    #' The content types that are expected from the inference endpoint.
    ACCEPT = function(){
      NotImplementedError$new("I'm an abstract interface method")
    }
  )
)

#' @title Abstract base class for creation of new deserializers.
#' @description This class extends the API of \code{BaseDeserializer} with more
#'              user-friendly options for setting the ACCEPT content type header,
#'              in situations where it can be provided at init and freely updated.
#' @family serializer
#' @export
SimpleBaseDeserializer = R6Class("SimpleBaseDeserializer",
  inherit = BaseDeserializer,
  public = list(

    #' @field accept
    #' The MIME type that is expected from the inference endpoint
    accept = NULL,

    #' @description Initialize a ``SimpleBaseDeserializer`` instance.
    #' @param accept (union[str, tuple[str]]): The MIME type (or tuple of allowable MIME types) that
    #'              is expected from the inference endpoint (default: "*/*").
    initialize = function(accept="*/*"){
      self$accept = accept
    }
  ),
  active = list(

    #' @field ACCEPT
    #' The tuple of possible content types that are expected from the inference endpoint.
    ACCEPT = function(){
      return(self$accept)
    }
  )
)

#' @title StringBaseDeserializer Class
#' @description  Deserialize raw data stream into a character string
#' @family serializer
#' @export
StringDeserializer = R6Class("StringBaseDeserializer",
  inherit = SimpleBaseDeserializer,
  public = list(

    #' @field encoding
    #' string encoding to be used
    encoding = NULL,

    #' @description Initialize a ``StringDeserializer`` instance.
    #' @param encoding (str): The string encoding to use (default: UTF-8).
    #' @param accept (str): The MIME type (or tuple of allowable MIME types) that
    #'              is expected from the inference endpoint (default: "application/json").
    initialize = function(encoding="UTF-8", accept = "application/json"){
      super$initialize(accept = accept)
      self$encoding = encoding
    },

    #' @description  Takes raw data stream and deserializes it.
    #' @param stream raw data to be deserialize
    #' @param content_type (str): The MIME type of the data.
    deserialize = function(stream, content_type) {
      con = rawConnection(stream)
      on.exit(close(con))
      obj = readLines(con, encoding = self$encoding, warn = FALSE)
      return(obj)
    }
  )
)

#' @title BytesDerializer Class
#' @description Deserialize a stream of bytes into a bytes object.
#' @family serializer
#' @export
BytesDeserializer = R6Class("BytesDeserializer",
  inherit = SimpleBaseDeserializer,
  public = list(

    #' @description Read a stream of bytes returned from an inference endpoint.
    #' @param stream (raw): A stream of bytes.
    #' @param content_type (str): The MIME type of the data.
    #' @return bytes: The bytes object read from the stream.
    deserialize = function(stream, content_type){
      return(stream)
    }
  )
)

#' @title Deserialize a stream of bytes into a list of lists.
#' @description Consider using \code{NumpyDeserializer} or
#'              \code{DataTableDeserializer} or \code{TibbleDeserializer} instead,
#'              if you'd like to convert text/csv
#'              responses directly into other data types.
#' @family serializer
#' @export
CSVDeserializer = R6Class("CSVDeserializer",
  inherit = SimpleBaseDeserializer,
  public = list(

    #' @field encoding
    #' string encoding to be used
    encoding = NULL,

    #' @description Initialize a ``CSVDeserializer`` instance.
    #' @param encoding (str): The string encoding to use (default: "UTF-8").
    #' @param accept (union[str, tuple[str]]): The MIME type (or tuple of allowable MIME types) that
    #'              is expected from the inference endpoint (default: "text/csv").
    initialize = function(encoding="UTF-8", accept = "text/csv"){
      super$initialize(accept = accept)
      self$encoding = encoding
      },

    #' @description Takes raw data stream and deserializes it.
    #' @param stream raw data to be deserialize
    #' @param content_type (str): The MIME type of the data.
    #' @return list: The data deserialized into a list of lists representing the
    #'              contents of a CSV file.
    deserialize = function(stream, content_type) {
      con = rawConnection(stream)
      on.exit(close(con))
      obj = readLines(con, warn = F, encoding = "UTF-8")
      return(strsplit(obj, ","))
    }
  )
)

#' @title NumpySerializer Class
#' @description Deserialize a stream of data in the .npy or UTF-8 CSV/JSON format.
#'              This serializer class uses python numpy package to deserialize,
#'              R objects through the use of the `reticulate` package.
#' @family serializer
#' @export
NumpyDeserializer = R6Class("NumpyDeserializer",
  inherit = SimpleBaseDeserializer,
  public = list(

    #' @field np
    #' Python Numpy package
    np = NULL,

    #' @field dtype
    #' The dtype of the data
    dtype = NULL,

    #' @field allow_pickle
    #' Allow loading pickled object arrays
    allow_pickle = NULL,

    #' @description Initialize a ``NumpyDeserializer`` instance.
    #' @param dtype (str): The dtype of the data (default: None).
    #' @param accept (union[str, tuple[str]]): The MIME type (or tuple of allowable MIME types) that
    #'              is expected from the inference endpoint (default: "application/x-npy").
    #' @param allow_pickle (bool): Allow loading pickled object arrays (default: True).
    initialize = function(dtype=NULL,
                          accept="application/x-npy",
                          allow_pickle=TRUE){
      if(!requireNamespace('reticulate', quietly=TRUE))
        SagemakerError$new('Please install `reticulate` package and try again')
      super$initialize(accept = accept)
      self$dtype = dtype
      self$allow_pickle = allow_pickle
      self$np = reticulate::import("numpy")
      },

    #' @description Deserialize data from an inference endpoint into a NumPy array.
    #' @param stream (botocore.response.StreamingBody): Data to be deserialized.
    #' @param content_type (str): The MIME type of the data.
    #' @return matrix: The data deserialized into a R matrix/array.
    deserialize = function(stream, content_type){
      if(content_type != "application/json"){
        f = tempfile()
        write_bin(stream, f)
        on.exit(unlink(f))
      }

      tryCatch({
        if(content_type == "text/csv"){
          obj = t(as.matrix(
            fread(f, sep = ",", encoding = "UTF-8", showProgress = FALSE, header = FALSE)
          ))
          rownames(obj) <- NULL
          attr(obj, "dimnames") <- NULL
          return(obj)
        }

        if(content_type == "application/json"){
          con = rawConnection(stream)
          on.exit(close(con))
          return(as.array(fromJSON(
            con,
            simplifyDataFrame = FALSE,
            simplifyMatrix = FALSE)
          ))
        }

        if(content_type == "application/x-npy"){
          return(self$np$load(f, allow_pickle = self$allow_pickle))
        }
      })

      ValueError$new(sprintf("%s cannot read content type %s.", class(self)[1L], content_type))
    }
  )
)

#' @title JSONDeserializer Class
#' @description Deserialize JSON data from an inference endpoint into a R object.
#' @family serializer
#' @export
JSONDeserializer = R6Class("JSONDeserializer",
  inherit = SimpleBaseDeserializer,
  public = list(

    #' @description Initialize a ``JSONDeserializer`` instance.
    #' @param accept (union[str, tuple[str]]): The MIME type (or tuple of allowable MIME types) that
    #'              is expected from the inference endpoint (default: "application/json").
    initialize = function(accept="application/json"){
      super$initialize(accept = accept)
      },

    #' @description  Deserialize JSON data from an inference endpoint into a Python object.
    #' @param stream (botocore.response.StreamingBody): Data to be deserialized.
    #' @param content_type (str): The MIME type of the data.
    #' @return object: The JSON-formatted data deserialized into a R object.
    deserialize = function(stream, content_type) {
      con = rawConnection(stream)
      on.exit(close(con))
      return(fromJSON(
        con,
        simplifyDataFrame = FALSE,
        simplifyMatrix = FALSE)
      )
    }
  )
)

#' @title JSONDeserializer Class
#' @description Deserialize JSON lines data from an inference endpoint.
#' @family serializer
#' @export
JSONLinesDeserializer = R6Class("JSONDeserializer",
  inherit = SimpleBaseDeserializer,
  public = list(

    #' @description Initialize a ``JSONLinesDeserializer`` instance.
    #' @param accept (union[str, tuple[str]]): The MIME type (or tuple of allowable MIME types) that
    #'              is expected from the inference endpoint (default: ("text/csv","application/json")).
    initialize = function(accept="application/json"){
      super$initialize(accept = accept)
    },

    #' @description  Deserialize JSON lines data from an inference endpoint.
    #'               See https://docs.python.org/3/library/json.html#py-to-json-table to
    #'               understand how JSON values are converted to R objects.
    #' @param stream (botocore.response.StreamingBody): Data to be deserialized.
    #' @param content_type (str): The MIME type of the data.
    #' @return list: A list of JSON serializable objects.
    deserialize = function(stream, content_type) {
      con = rawConnection(stream)
      on.exit(close(con))
      data = stream_in(con, verbose = FALSE)
      return(data)
    }
  )
)

#' @title DataTableDeserializer Class
#' @description Deserialize CSV or JSON data from an inference endpoint into a data.table.
#' @family serializer
#' @export
DataTableDeserializer = R6Class("DataTableDeserializer",
  inherit = SimpleBaseDeserializer,
  public = list(

    #' @field encoding
    #' string encoding to be used
    encoding = NULL,

    #' @description Initialize a ``DataTableDeserializer`` instance.
    #' @param encoding (str): The string encoding to use (default: "UTF-8").
    #' @param accept (union[str, tuple[str]]): The MIME type (or tuple of allowable MIME types) that
    #'              is expected from the inference endpoint (default: ("text/csv","application/json")).
    initialize = function(encoding = "UTF-8",
                          accept=c("text/csv", "application/json")){
      super$initialize(accept = accept)
      self$encoding = encoding
    },

    #' @description Deserialize CSV or JSON data from an inference endpoint into a data.table.
    #'              If the data is JSON, the data should be formatted in the 'columns' orient.
    #' @param stream (botocore.response.StreamingBody): Data to be deserialized.
    #' @param content_type (str): The MIME type of the data.
    #' @return data.table: The data deserialized into a data.table.
    deserialize = function(stream, content_type){

      if(content_type == "text/csv"){
        f = tempfile()
        write_bin(stream, f)
        on.exit(unlink(f))
        return(fread(f, encoding = self$encoding, sep = ",", showProgress = FALSE))
      }

      if(content_type == "application/json"){
        con = rawConnection(stream)
        on.exit(close(con))
        data = as.data.table(parse_json(con))
        return(data)
      }

      ValueError$new(sprintf("%s cannot read content type %s.",content_type))
    }
  )
)

#' @title TibbleDeserializer Class
#' @description Deserialize CSV or JSON data from an inference endpoint into a tibble.
#' @family serializer
#' @export
TibbleDeserializer = R6Class("TibbleDeserializer",
  inherit = SimpleBaseDeserializer,
  public = list(

    #' @field encoding
    #' string encoding to be used
    encoding = NULL,

    #' @description Initialize a ``TibbleDeserializer`` instance.
    #' @param encoding (str): The string encoding to use (default: "UTF-8").
    #' @param accept (union[str, tuple[str]]): The MIME type (or tuple of allowable MIME types) that
    #'              is expected from the inference endpoint (default: ("text/csv","application/json")).
    initialize = function(encoding = "UTF-8",
                          accept=c("text/csv", "application/json")){
      super$initialize(accept = accept)
      if(!requireNamespace('readr', quietly=TRUE))
        SagemakerError$new('Please install `readr` package and try again')
      self$encoding = encoding
    },

    #' @description Deserialize CSV or JSON data from an inference endpoint into a data.table.
    #'              If the data is JSON, the data should be formatted in the 'columns' orient.
    #' @param stream (botocore.response.StreamingBody): Data to be deserialized.
    #' @param content_type (str): The MIME type of the data.
    #' @return data.table: The data deserialized into a tibble.
    deserialize = function(stream, content_type){

      if(content_type == "text/csv"){
        return(readr::read_csv(
          stream,
          locale = readr::locale(encoding = self$encoding),
          progress = F)
        )
      }

      if(content_type == "application/json"){
        con = rawConnection(stream)
        on.exit(close(con))

        if(!requireNamespace('tibble', quietly=TRUE))
          SagemakerError$new('Please install `tibble` package and try again')

        data = tibble::as_tibble(fromJSON(con))
        return(data)
      }

      ValueError$new(sprintf("%s cannot read content type %s.",content_type))
    }
  )
)
