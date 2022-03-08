# sagemaker.mlcore 0.3.0:
## Architecture Change:
* moved all Amazon machine learning framework classes to `sagemaker.mlframework`
* moved clarify classes and method to `sagemaker.common`
** NOTE:** this changes reduce package overall size from `5Mb` to `3.7Mb` align to `CRAN` package size limitations.

# sagemaker.mlcore 0.2.4:
## Bug Fix:
* Class `HyperparameterTuner` correctly validates hyper parameters
* Correctly pass static hyperparameters


## Feature:
* Class `HyperparameterTuner` `create` method is now callable at `R6ClassGenerator` level. To help mimic python equivalent.

# sagemaker.mlcore 0.2.3:
## Bug Fix:
* correctly parse parameters class

## Minor:
* Add `.module` field to estimator classes to minic python `cls.__module__`

# sagemaker.mlcore 0.2.2:
## Bug Fix:
* allow `container_log_level` to pass any string instead of restricting to logging levels.

## Minor:
* explicitly reference `sagemaker.core` functions to help with mock functionality

# sagemaker.mlcore 0.2.1:
## Bug Fix:
* Ensure deserializer classes correctly parse raw vectors as expected.

# sagemaker.mlcore 0.2.0:
## Feature:
* Add SparseMatrixSerializer serialize class
* Allow all serializer classes read from data in from file.
* LibSVMSerializer class now uses readsparse package in the backend

## Bug Fix:
* CSVSerializer correctly serialize data

# sagemaker.mlcore 0.1.0:

Initial release to r-universe
