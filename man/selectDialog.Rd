\name{selectDialog}
\alias{selectDialog}

\title{Dialog window for selection between items}

\description{
Dialog window for selection between items.
}

\usage{
selectDialog(title = "Selection entry", question = "Select item",
             itemNames, top = NULL, returnValOnCancel = "ID_CANCEL", 
             do.grab = FALSE)
}

\arguments{
  \item{title}{ Text string for the title bar of the appering window. }
  \item{question}{ Text string for the \code{question}. }
  \item{itemNames}{ Default value of answer. }
  \item{top}{ Text string for the \code{TclTk} \code{top}. }
% \item{entryWidth}{ Integer for the \code{entryWidth}. }
  \item{returnValOnCancel}{ Text
        string for the returned value on \code{Cancel}. }
% \item{graphWindow}{ \code{graphWindow} can be given to give the \code{top}. }
  \item{do.grab}{ Logical. 
        \code{tkgrab.set} resulted in fail for some systems. }
}

\value{
    An integer, or \code{returnValOnCancel}.
}

\references{\url{http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/modalDialog.html}}

\author{From the examples compiled by James Wettenhall.}

\seealso{ \code{\link{modalDialog}}. }

% \examples{ }

\keyword{ dynamic }

