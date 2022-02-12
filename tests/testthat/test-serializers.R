# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/tests/unit/sagemaker/test_serializers.py

DATA_DIR = file.path(getwd(), "data")

test_that("test_csv_serializer_str", {
  original = "1,2,3"
  result = CSVSerializer$new()$serialize("1,2,3")
  expect_equal(original, result)
})

test_that("test_csv_serializer_r_vector", {
  original = "1,2,3"
  result = CSVSerializer$new()$serialize(c(1, 2, 3))
  expect_equal(result, original)
})

test_that("test_csv_serializer_array", {
  original = "1,2,3"
  result = CSVSerializer$new()$serialize(array(c(1, 2, 3)))
  expect_equal(result, original)
})

test_that("test_csv_serializer_valid_2dimensional", {
  result = CSVSerializer$new()$serialize(array(list(c(1,2,3), c(4,5,6))))
  expect_equal(result, "1,2,3\n4,5,6")
})

test_that("test_csv_serializer_list_of_str", {
  result = CSVSerializer$new()$serialize(c("1,2,3", "4,5,6"))
  expect_equal(result, "1,2,3\n4,5,6")
})

test_that("test_csv_serializer_list_of_vectors", {
  result = CSVSerializer$new()$serialize(list(c(1, 2, 3), c(4,5,6)))
  expect_equal(result, "1,2,3\n4,5,6")
})

test_that("test_csv_serializer_list_of_list", {
  result = CSVSerializer$new()$serialize(list(list(1, 2, 3), list(4,5,6)))
  expect_equal(result, "1,2,3\n4,5,6")
})

test_that("test_csv_serializer_list_of_empty", {
  expect_error(
    CSVSerializer$new()$serialize(list(list(), list())),
    "empty array",
    class = "ValueError"
  )
})

test_that("test_csv_serializer_empty_vector", {
  expect_error(
    CSVSerializer$new()$serialize(c()),
    "empty array",
    class = "ValueError"
  )
})

test_that("test_csv_serializer_empty_vector", {
  expect_error(
    CSVSerializer$new()$serialize(c()),
    "empty array",
    class = "ValueError"
  )
})

test_that("test_csv_serializer_csv_reader", {
  csv_file_path = file.path(DATA_DIR, "with_integers.csv")
  validation_data = readBin(csv_file_path, "raw", n = file.size(csv_file_path))
  result = CSVSerializer$new()$serialize(csv_file_path)
  expect_equal(result, validation_data)
})

test_that("test_csv_serializer_matrix", {
  csv_file_path = file.path(DATA_DIR, "with_integers.csv")
  validation_data = readBin(csv_file_path, "raw", n = file.size(csv_file_path))
  result = CSVSerializer$new()$serialize(matrix(1:9, ncol=3, byrow =T))
  cat("\nDebug print\n")
  print(rawToChar(result))
  print(rawToChar(validation_data))
  expect_equal(result, validation_data)
})

test_that("test_csv_serializer_dataframe", {
  csv_file_path = file.path(DATA_DIR, "with_integers.csv")
  validation_data = readBin(csv_file_path, "raw", n = file.size(csv_file_path))
  result = CSVSerializer$new()$serialize(as.data.frame(matrix(1:9, ncol=3, byrow =T)))
  cat("\nDebug print\n")
  print(rawToChar(result))
  print(rawToChar(validation_data))
  expect_equal(result, validation_data)
})

numpy_bin = function(data){
  f = tempfile(fileext = ".npy")
  on.exit(unlink(f))
  np = reticulate::import("numpy")
  np$save(f, data)
  return(readBin(f, "raw", n = file.size(f)))
}

test_that("test_numpy_serializer_array" , {
  skip_if_no_python()
  skip_if_no_numpy()
  array = c(1, 2, 3)
  result = NumpySerializer$new()$serialize(array)
  expect_equal(result, numpy_bin(array))
})

test_that("test_numpy_serializer_error_for_dtype" , {
  skip_if_no_python()
  skip_if_no_numpy()
  expect_error(
    NumpySerializer$new(dtype="float16"),
    ".*Please set class in R",
    class = "ValueError"
  )
})

test_that("test_numpy_serializer_numpy_valid_2_dimensional" , {
  skip_if_no_python()
  skip_if_no_numpy()
  array = array(list(c(1, 2, 3), c(4,5,6)))
  result = NumpySerializer$new()$serialize(array)
  expect_equal(result,
    numpy_bin(matrix(unlist(array), ncol = lengths(array)[[1]], byrow = T))
  )
})

test_that("test_numpy_serializer_numpy_valid_2_list" , {
  skip_if_no_python()
  skip_if_no_numpy()
  array = list(c(1, 2, 3), c(4,5,6))
  result = NumpySerializer$new()$serialize(array)
  expect_equal(result,
    numpy_bin(matrix(unlist(array), ncol = lengths(array)[[1]], byrow = T))
  )
})

test_that("test_numpy_serializer_numpy_valid_multidimensional" , {
  skip_if_no_python()
  skip_if_no_numpy()
  data = array(1, c(10,10,10,10))
  result = NumpySerializer$new()$serialize(data)
  expect_equal(
    result,
    numpy_bin(data)
  )
})

test_that("test_numpy_serializer_numpy_valid_list_of_strings", {
  skip_if_no_python()
  skip_if_no_numpy()
  data = array(c("one", "two", "three"))
  result = NumpySerializer$new()$serialize(data)
  expect_equal(
    result,
    numpy_bin(data)
  )
})

test_that("test_numpy_serializer_from_buffer", {
  skip_if_no_python()
  skip_if_no_numpy()
  data = array(c(2,3))
  expected = numpy_bin(data)
  con = rawConnection(expected, open = "r+")
  result = NumpySerializer$new()$serialize(con)
  expect_equal(
    result,
    expected
  )
  close(con)
})

test_that("test_numpy_serializer_from_file", {
  skip_if_no_python()
  skip_if_no_numpy()
  f = tempfile(fileext = ".npy")
  np = reticulate::import("numpy")

  data = array(c(2,3))
  np$save(f, data)

  expected = numpy_bin(data)
  result = NumpySerializer$new()$serialize(f)

  expect_equal(
    result,
    expected
  )
})

test_that("test_numpy_serializer_list_of_empty", {
  skip_if_no_python()
  skip_if_no_numpy()
  expect_error(
    NumpySerializer$new()$serialize(list()),
    "Cannot serialize empty array.",
    class = "ValueError"
  )
})

test_that("test_numpy_serializer_data.frame_invalid_empty", {
  skip_if_no_python()
  skip_if_no_numpy()
  expect_error(
    NumpySerializer$new()$serialize(data.frame()),
    "Cannot serialize empty data.frame.",
    class = "ValueError"
  )
})

json_bin = function(data){
  con = rawConnection(raw(0), "r+")
  on.exit(close(con))
  jsonlite::write_json(data, con, dataframe = "columns", auto_unbox = F)
  return(rawConnectionValue(con))
}

test_that("test_json_serializer_array_valid", {
  data = array(c(1, 2, 3))
  result = JSONSerializer$new()$serialize(data)
  expect_equal(result, json_bin(data))
})

test_that("test_json_serializer_array_valid_2dimensional", {
  data = array(list(c(1,2,3), c(4,5,6)))
  result = JSONSerializer$new()$serialize(data)
  expect_equal(result, json_bin(data))
})

test_that("test_json_serializer_empty", {
  data = list()
  result = JSONSerializer$new()$serialize(data)
  expect_equal(result, json_bin(data))
})

test_that("test_json_serializer_vector", {
  data = c(1,2,3)
  result = JSONSerializer$new()$serialize(data)
  expect_equal(result, json_bin(data))
})

test_that("test_json_serializer_r_list", {
  data =  list("gender"="m", "age"=22, "city"="Paris")
  result = JSONSerializer$new()$serialize(data)
  con <- rawConnection(result, "r+")
  expect_equal(data, jsonlite::fromJSON(con))
  close(con)
})

test_that("test_json_serializer_csv_buffer", {
  csv_file_path = file.path(DATA_DIR, "with_integers.csv")
  validation_data = readBin(csv_file_path, "raw", n = file.size(csv_file_path))
  result = JSONSerializer$new()$serialize(csv_file_path)
  expect_equal(result, validation_data)
})

test_that("test_identity_serializer", {
  result = identity_serializer = IdentitySerializer$new()$serialize(as.raw(list(1,2)))
  expect_equal(result, as.raw(list(1,2)))
})

csv_bin = function(data){
  f = tempfile()
  on.exit(unlink(f))
  data.table::fwrite(data, f, col.names = FALSE, showProgress = FALSE)
  return(readBin(f, "raw", n = file.size(f)))
}

test_that("test_identity_serializer_with_custom_content_type", {
  data = csv_bin(matrix(c("a","b", "1", "2"), ncol = 2, byrow = T))
  identity_serializer = IdentitySerializer$new(content_type="text/csv")
  expect_equal(identity_serializer$serialize(data), data)
  expect_equal(identity_serializer$CONTENT_TYPE, "text/csv")
})

jsonline_bin = function(data){
  con = rawConnection(raw(0), "r+")
  on.exit(close(con))
  jsonlite::stream_out(data, con = con, verbose = FALSE)
  return(rawConnectionValue(con))
}

test_that("test_json_lines_serializer_dataframe", {
  data = data.frame(Name = c("Gilbert", "Alexa"), Score = c(24, 29))
  actual = JSONLinesSerializer$new()$serialize(data)
  expect_equal(actual, jsonline_bin(data))
})

test_that("test_json_lines_serializer_file_like", {
  data = data.frame(Name = c("Gilbert", "Alexa"), Score = c(24, 29))
  obj = jsonline_bin(data)
  con = rawConnection(obj, "r+")
  actual = JSONLinesSerializer$new()$serialize(con)
  expect_equal(actual, rawConnectionValue(con))
  close(con)
})

test_that("test_json_lines_serializer_file_like", {
  data = data.frame(Name = c("Gilbert", "Alexa"), Score = c(24, 29))
  con <- textConnection("foo", "w")
  jsonlite::stream_out(data, con=con, verbose = FALSE)
  actual = JSONLinesSerializer$new()$serialize(con)
  expect_equal(actual, textConnectionValue(con))
  close(con)
})

sparse_npz_bin <- function(data){
  f = tempfile(fileext = ".npz")
  on.exit(unlink(f))
  scipy = reticulate::import("scipy")
  scipy$sparse$save_npz(f, data)
  return(readBin(f, "raw", n = file.size(f)))
}

library(Matrix)

test_that("test_sparse_matrix_serializer", {
  skip_if_no_python()
  skip_if_no_scipy()

  data = Matrix::Matrix(c(0, 0, 3, 4, 0, 0), ncol=3, byrow=T)
  actual = SparseMatrixSerializer$new()$serialize(data)
  expect_equal(actual, sparse_npz_bin(data))
})

test_that("test_libsvm_serializer_str", {
  original = "0 0:1 5:1"
  actual = LibSVMSerializer$new()$serialize(original)
  expect_equal(actual, original)
})

test_that("test_libsvm_serializer_file_like", {
  libsvm_file_path = file.path(DATA_DIR, "xgboost_abalone", "abalone")
  actual = LibSVMSerializer$new()$serialize(libsvm_file_path)
  expect_equal(actual, readBin(libsvm_file_path, "raw", n=file.size(libsvm_file_path)))
})

lib_svm_bin = function(data){
  f = tempfile(fileext = ".svmlight")
  on.exit(unlink(f))
  if(is.list(data)){
    readsparse::write.sparse(file = f, X = data[[1]], y=data[[2]])
  } else {
    readsparse::write.sparse(file = f, X = data, y = rep(0, nrow(data)))
  }
  return(readBin(f, what = "raw", n = file.size(f)))
}

test_that("test_libsvm_serializer_sparse_matrix", {
  data = Matrix::Matrix(c(0, 0, 3, 4, 0, 0), ncol=3, byrow=T)
  actual = LibSVMSerializer$new()$serialize(data)
  expect_equal(actual, lib_svm_bin(data))
})

test_that("test_libsvm_serializer_list", {
  data = list(
    matrix = Matrix::Matrix(c(0, 0, 3, 4, 0, 0), ncol=3, byrow=T),
    labels = c(1,2)
  )
  actual = LibSVMSerializer$new()$serialize(data)
  expect_equal(actual, lib_svm_bin(data))
})
