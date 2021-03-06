% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_monitor_model_monitoring.R
\name{EndpointInput}
\alias{EndpointInput}
\title{Accepts parameters that specify an endpoint input for monitoring execution.}
\description{
It also provides a method to turn those parameters into a dictionary.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{EndpointInput$new()}}
\item \href{#method-to_request_list}{\code{EndpointInput$to_request_list()}}
\item \href{#method-format}{\code{EndpointInput$format()}}
\item \href{#method-clone}{\code{EndpointInput$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initialize an ``EndpointInput`` instance. EndpointInput accepts parameters
             that specify an endpoint input for a monitoring job and provides a method
             to turn those parameters into a dictionary.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{EndpointInput$new(
  endpoint_name,
  destination,
  s3_input_mode = c("File", "Pipe"),
  s3_data_distribution_type = c("FullyReplicated", "ShardedByS3Key"),
  start_time_offset = NULL,
  end_time_offset = NULL,
  features_attribute = NULL,
  inference_attribute = NULL,
  probability_attribute = NULL,
  probability_threshold_attribute = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{endpoint_name}}{(str): The name of the endpoint.}

\item{\code{destination}}{(str): The destination of the input.}

\item{\code{s3_input_mode}}{(str): The S3 input mode. Can be one of: "File", "Pipe. Default: "File".}

\item{\code{s3_data_distribution_type}}{(str): The S3 Data Distribution Type. Can be one of:
"FullyReplicated", "ShardedByS3Key"}

\item{\code{start_time_offset}}{(str): Monitoring start time offset, e.g. "-PT1H"}

\item{\code{end_time_offset}}{(str): Monitoring end time offset, e.g. "-PT0H".}

\item{\code{features_attribute}}{(str): JSONpath to locate features in JSONlines dataset.
Only used for ModelBiasMonitor and ModelExplainabilityMonitor}

\item{\code{inference_attribute}}{(str): Index or JSONpath to locate predicted label(s).
Only used for ModelQualityMonitor, ModelBiasMonitor, and ModelExplainabilityMonitor}

\item{\code{probability_attribute}}{(str or int): Index or JSONpath to locate probabilities.
Only used for ModelQualityMonitor, ModelBiasMonitor and ModelExplainabilityMonitor}

\item{\code{probability_threshold_attribute}}{(float): threshold to convert probabilities to binaries
Only used for ModelQualityMonitor, ModelBiasMonitor and ModelExplainabilityMonitor}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-to_request_list"></a>}}
\if{latex}{\out{\hypertarget{method-to_request_list}{}}}
\subsection{Method \code{to_request_list()}}{
Generates a request dictionary using the parameters provided to the class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{EndpointInput$to_request_list()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-format"></a>}}
\if{latex}{\out{\hypertarget{method-format}{}}}
\subsection{Method \code{format()}}{
format class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{EndpointInput$format()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{EndpointInput$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
