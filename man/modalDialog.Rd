\name{modalDialog}

\alias{modalDialog}

\title{Modal dialog window for returning a text string}

\description{
  Ask for a text string in a pop up window.
}

\usage{
modalDialog(title, question, entryInit, top = NULL, entryWidth = 20, 
            returnValOnCancel = "ID_CANCEL", do.grab = FALSE)
}

% Modal Dialog Boxes

% A modal dialog box requires the user to respond to it before changing the focus to other windows within the application. The Tk command tk_dialog is designed for this purpose, and can be called in R TclTk, using the tkdialog function. However, the method illustrated below will use the tktoplevel function and call tkgrab.set and tkgrab.release explicitly, rather than relying on tkdialog to call them automatically.

% Modal Dialog Box Example

% The following example defines a subroutine which creates a simple modal dialog box using tktoplevel to create the dialog window, and using tkgrab.set and tkgrab.release to force the user to respond to this dialog before changing focus to other window(s) in the applciation.


\arguments{
  \item{title}{ Text string for the title bar of the appering window. }
  \item{question}{ Text string for the \code{question}. }
  \item{entryInit}{ Default value of answer. }
  \item{top}{ Text string for the \code{TclTk} \code{top}. }
  \item{entryWidth}{ Integer for the \code{entryWidth}. }
  \item{returnValOnCancel}{ Text
        string for the returned value on \code{Cancel}. }
% \item{graphWindow}{ \code{graphWindow} can be given to give the \code{top}. }
  \item{do.grab}{ Logical. 
        \code{tkgrab.set} resulted in fail for some systems. }
}

\value{
    The text string entered, or \code{returnValOnCancel}.
}

\references{\url{http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/modalDialog.html}}

\author{From the examples compiled by James Wettenhall.}

\examples{
Menus <- 
 list(MainUser = 
      list(label = "Test of user drag down menu - Position of \"vertices\"",
           command = function(object, ...) 
             print(Positions(list(...)$Arguments$vertexList))),
      MainUser = 
      list(label = "Test of user drag down menu - modalDialog",
           command = function(object, ...) {
             Args <- list(...)$Arguments
             ReturnVal <- modalDialog("Test modalDialog Entry", "Enter name",
                                      Args$title, graphWindow = Args$graphWindow)
             print(ReturnVal)
             if (ReturnVal == "ID_CANCEL")
               return() } )
     )
}

\keyword{ dynamic }

