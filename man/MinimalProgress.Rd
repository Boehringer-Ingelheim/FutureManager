% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progress.R
\name{MinimalProgress}
\alias{MinimalProgress}
\title{Minimal progress}
\description{
Minimal progress

Minimal progress
}
\details{
Minimal progress class that enhances the default shiny progress behavior.
The most important is ability to initialize the progress without opening
in the app frontend. Also, the interface is simplified.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{MinimalProgress$new()}}
\item \href{#method-open}{\code{MinimalProgress$open()}}
\item \href{#method-close}{\code{MinimalProgress$close()}}
\item \href{#method-set}{\code{MinimalProgress$set()}}
\item \href{#method-clone}{\code{MinimalProgress$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new progress object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinimalProgress$new(id, session)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{character string, the progress ID}

\item{\code{session}}{shiny session object}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
MinimalProgress object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-open"></a>}}
\if{latex}{\out{\hypertarget{method-open}{}}}
\subsection{Method \code{open()}}{
Opens the progress bar in the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinimalProgress$open()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
self
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-close"></a>}}
\if{latex}{\out{\hypertarget{method-close}{}}}
\subsection{Method \code{close()}}{
Close the progress bar in the frontend
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinimalProgress$close()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
self
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set"></a>}}
\if{latex}{\out{\hypertarget{method-set}{}}}
\subsection{Method \code{set()}}{
Set a progress bar state
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinimalProgress$set(label, value, msg)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{label}}{character string, the progress label}

\item{\code{value}}{numeric in range 0..1}

\item{\code{msg}}{character string, the progress message}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
self
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinimalProgress$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
