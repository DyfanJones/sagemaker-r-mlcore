% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/serializers.R
\name{JSONSerializer}
\alias{JSONSerializer}
\title{JSONSerializer Class}
\description{
Serialize data to a JSON formatted string.
}
\seealso{
Other serializer: 
\code{\link{BaseDeserializer}},
\code{\link{BaseSerializer}},
\code{\link{BytesDeserializer}},
\code{\link{CSVDeserializer}},
\code{\link{CSVSerializer}},
\code{\link{DataTableDeserializer}},
\code{\link{IdentitySerializer}},
\code{\link{JSONDeserializer}},
\code{\link{JSONLinesDeserializer}},
\code{\link{JSONLinesSerializer}},
\code{\link{LibSVMSerializer}},
\code{\link{NumpyDeserializer}},
\code{\link{NumpySerializer}},
\code{\link{SimpleBaseDeserializer}},
\code{\link{SimpleBaseSerializer}},
\code{\link{SparseMatrixSerializer}},
\code{\link{StringDeserializer}},
\code{\link{TibbleDeserializer}}
}
\concept{serializer}
\section{Super classes}{
\code{\link[sagemaker.mlcore:BaseSerializer]{sagemaker.mlcore::BaseSerializer}} -> \code{\link[sagemaker.mlcore:SimpleBaseSerializer]{sagemaker.mlcore::SimpleBaseSerializer}} -> \code{JSONSerializer}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-serialize}{\code{JSONSerializer$serialize()}}
\item \href{#method-clone}{\code{JSONSerializer$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="sagemaker.mlcore" data-topic="BaseSerializer" data-id="format">}\href{../../sagemaker.mlcore/html/BaseSerializer.html#method-format}{\code{sagemaker.mlcore::BaseSerializer$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="sagemaker.mlcore" data-topic="SimpleBaseSerializer" data-id="initialize">}\href{../../sagemaker.mlcore/html/SimpleBaseSerializer.html#method-initialize}{\code{sagemaker.mlcore::SimpleBaseSerializer$initialize()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-serialize"></a>}}
\if{latex}{\out{\hypertarget{method-serialize}{}}}
\subsection{Method \code{serialize()}}{
Serialize data of various formats to a JSON formatted string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JSONSerializer$serialize(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{(object): Data to be serialized.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
(raw): The data serialized as a JSON string.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JSONSerializer$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
