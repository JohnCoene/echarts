% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add.R, R/add_.R
\name{ecandle}
\alias{ecandle}
\alias{candlestick}
\alias{ecandle_}
\title{Add candlestick}
\usage{
ecandle(p, opening, closing, low, high, name = NULL, clickable = TRUE,
  z = 2, zlevel = 0, ...)

ecandle_(p, opening, closing, low, high, name = NULL, clickable = TRUE,
  z = 2, zlevel = 0, ...)
}
\arguments{
\item{p}{an echart object.}

\item{opening, closing, low, high}{stock prices.}

\item{name}{name of serie.}

\item{clickable}{whether serie is clickable.}

\item{z, zlevel}{first and second grade cascading control, the higher z the closer to the top.}

\item{...}{any other options to pass to candlessticks.}
}
\description{
Add candlestick bars.
}
\examples{
# generate data
date <- c("2017-01-01", "2017-01-02", "2017-01-03", "2017-01-04", "2017-03-05",
          "2017-01-06", "2017-01-07")
stock <- data.frame(date = date,
                    opening = c(200.60, 200.22, 198.43, 199.05, 203.54, 203.40, 208.34),
                    closing = c(200.72, 198.85, 199.05, 203.73, 204.08, 208.11, 211.88),
                    low = c(197.82, 198.07, 197.90, 198.10, 202.00, 201.50, 207.60),
                    high = c(203.32, 200.67, 200.00, 203.95, 204.90, 208.44, 213.17))

js <- htmlwidgets::JS("function(params){
  var res = 'opening: ' + params.value[0] + '<br>' + 'closing: ' + params.value[3];
  return res}")

stock \%>\%
  echart(date) \%>\%
  ecandle(opening, closing, low, high, barMaxWidth = 20) \%>\%
  etooltip(trigger = "item", formatter = js) \%>\%
  etheme("macarons")

}
\seealso{
\href{http://echarts.baidu.com/echarts2/doc/option-en.html#title~series-i(k)}{candlestick official docs}
}
