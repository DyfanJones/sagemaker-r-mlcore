# NOTE: This code has been modified from AWS Sagemaker Python:
# https://github.com/aws/sagemaker-python-sdk/blob/master/tests/unit/sagemaker/test_deserializers.py

test_that("test_string_deserializer", {
  deserializer = StringDeserializer$new()
  result = deserializer$deserialize(charToRaw("[1, 2, 3]"), "application/json")
  expect_equal(result, "[1, 2, 3]")
})

test_that("test_bytes_deserializer", {
  deserializer = BytesDeserializer$new()
  result = deserializer$deserialize(charToRaw("[1, 2, 3]"), "application/json")
  expect_equal(result, charToRaw("[1, 2, 3]"))
})

test_that("test_csv_deserializer_single_element", {
  result = CSVDeserializer$new()$deserialize(charToRaw("1"), "text/csv")
  expect_equal(result, list("1"))
})

test_that("test_csv_deserializer_single_element", {
  result = CSVDeserializer$new()$deserialize(charToRaw("1,2,3"), "text/csv")
  expect_equal(result, list(c("1", "2", "3")))
})

test_that("test_csv_deserializer_2dimensional", {
  result = CSVDeserializer$new()$deserialize(charToRaw("1,2,3\n3,4,5"), "text/csv")
  expect_equal(result, list(c("1", "2", "3"), c("3", "4", "5")))
})

test_that("test_csv_deserializer_posix_compliant", {
  result = CSVDeserializer$new()$deserialize(charToRaw("1,2,3\n3,4,5\n"), "text/csv")
  expect_equal(result, list(c("1", "2", "3"), c("3", "4", "5")))
})

test_that("test_numpy_deserializer_from_csv", {
  skip_if_no_python()
  skip_if_no_numpy()

  deserializer = NumpyDeserializer$new()
  result = deserializer$deserialize(charToRaw("1,2,3\n4,5,6"), "text/csv")
  expect_equal(result, matrix(c(1, 2, 3, 4, 5, 6), ncol=2))
})

test_that("test_numpy_deserializer_from_csv_alpha", {
  skip_if_no_python()
  skip_if_no_numpy()

  deserializer = NumpyDeserializer$new()
  result = deserializer$deserialize(charToRaw("hello,2,3\n4,5,6"), "text/csv")
  expect_equal(result, matrix(c("hello","2","3", "4", "5", "6"), ncol=2))
})

test_that("test_numpy_deserializer_from_json", {
  skip_if_no_python()
  skip_if_no_numpy()

  stream = charToRaw("[[1,2,3],\n[4,5,6]]")
  result = NumpyDeserializer$new()$deserialize(stream, "application/json")
  expect_equal(result, array(list(c(1,2,3), c(4,5,6))))
})

test_that("test_numpy_deserializer_from_json_ragged", {
  skip_if_no_python()
  skip_if_no_numpy()

  stream = charToRaw("[[1,2,3],\n[4,5,6,7]]")
  result = NumpyDeserializer$new()$deserialize(stream, "application/json")
  expect_equal(result, array(list(c(1,2,3), c(4,5,6,7))))
})

test_that("test_numpy_deserializer_from_json_ragged", {
  skip_if_no_python()
  skip_if_no_numpy()

  stream = charToRaw('[["hello",2,3],\n[4,5,6]]')
  result = NumpyDeserializer$new()$deserialize(stream, "application/json")

  # sadly R has to have vector or all the same type
  expect_equal(result, array(list(c("hello","2","3"), c(4,5,6))))
})

numpy_bin = function(data){
  f = tempfile(fileext = ".npy")
  on.exit(unlink(f))
  np = reticulate::import("numpy")
  np$save(f, data)
  return(readBin(f, "raw", n = file.size(f)))
}

test_that("test_numpy_deserializer_from_array", {
  skip_if_no_python()
  skip_if_no_numpy()

  data = array(1,c(2,3))
  result = NumpyDeserializer$new()$deserialize(numpy_bin(data), "application/x-npy")
  expect_equal(result, data)
})

test_that("test_numpy_deserializer_from_list", {
  skip_if_no_python()
  skip_if_no_numpy()

  data = list(list("a"="", "b"=""), list("c"="", "d"=""))
  result = NumpyDeserializer$new()$deserialize(numpy_bin(data), "application/x-npy")
  attr(result, "dim") <- NULL
  expect_equal(result, data)
})

test_that("test_numpy_deserializer_from_npy_object_array_with_allow_pickle_false", {
  skip_if_no_python()
  skip_if_no_numpy()

  numpy_deserializer = NumpyDeserializer$new(allow_pickle=FALSE)

  data = list(list("a"="", "b"=""), list("c"="", "d"=""))
  expect_error(
    numpy_deserializer$deserialize(numpy_bin(data), "application/x-npy")
  )
})

test_that("test_json_deserializer_array", {
  result = JSONDeserializer$new()$deserialize(charToRaw("[1, 2, 3]"), "application/json")
  expect_equal(result, c(1,2,3))
})

test_that("test_json_deserializer_2dimensional", {
  result = JSONDeserializer$new()$deserialize(charToRaw("[[1, 2, 3], [3, 4, 5]]"), "application/json")
  expect_equal(result, list(c(1,2,3), c(3,4,5)))
})

test_that("test_json_deserializer_invalid_data", {
  expect_error(
    JSONDeserializer$new()$deserialize(charToRaw("[[1]"), "application/json")
  )
})

test_that("test_data.table_deserializer_json", {
  data = '{"col 1": {"row 1": "a", "row 2": "c"}, "col 2": {"row 1": "b", "row 2": "d"}}'
  result = DataTableDeserializer$new()$deserialize(charToRaw(data), "application/json")
  expected = data.table("col 1" = list("a", "c"), "col 2" = list("b", "d"))
  expect_equal(result, expected)
})

test_that("test_data.table_deserializer_csv", {
  result = DataTableDeserializer$new()$deserialize(charToRaw("col 1,col 2\na,b\nc,d"), "text/csv")
  expected = data.table("col 1" = c("a", "c"), "col 2" = c("b", "d"))
  expect_equal(result, expected)
})

test_that("test_tibble_deserializer_json", {
  data = '{"col 1": {"row 1": "a", "row 2": "c"}, "col 2": {"row 1": "b", "row 2": "d"}}'
  result = TibbleDeserializer$new()$deserialize(charToRaw(data), "application/json")
  expected = tibble::tibble("col 1" = list("row 1"="a", "row 2"="c"), "col 2" = list("row 1"="b", "row 2"="d"))
  expect_equal(result, expected)
})

test_that("test_tibble_deserializer_csv", {
  result = TibbleDeserializer$new()$deserialize(charToRaw("col 1,col 2\na,b\nc,d"), "text/csv")
  expected = readr::read_csv("col 1,col 2\na,b\nc,d")
  expect_equal(result, expected)
})


data_ll = list(
  list(
    source = '["Name", "Score"]\n["Gilbert", 24]',
    expected = data.frame(V1 = c("Name", "Gilbert"), V2 = c("Score", "24"))
  ),
  list(
    source = '["Name", "Score"]\n["Gilbert", 24]\n',
    expected = data.frame(V1 = c("Name", "Gilbert"), V2 = c("Score", "24"))
  ),
  list(
    source = '{"Name": "Gilbert", "Score": 24}\n{"Name": "Alexa", "Score": 29}',
    expected = data.frame(Name = c("Gilbert", "Alexa"), Score = c(24, 29))
  )
)

test_that("test_json_lines_deserializer", {

  for (ll in data_ll){

    # jsonlite::stream_in raises warnings if not correctly ended
    if(!endsWith(ll$source, "\n")) {
      expect_warning({result = JSONLinesDeserializer$new()$deserialize(charToRaw(ll$source))})
    } else {
      result = JSONLinesDeserializer$new()$deserialize(charToRaw(ll$source))
    }
    expect_equal(result, ll$expected)
  }
})
