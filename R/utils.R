#' Add a double hash comment
#'
#' The function inserts or removes two hashes and a space (\code{"## "}) at the
#' beginning of a line. It also works for multiple line. The function is meant
#' to behave similarly to the Rstudio built-in \code{Comment / Uncomment
#' Selection} tool (see Details for an comprehensive description).
#'
#' @import rstudioapi
#'
#' @usage
#' insertDoubleHash()
#'
#' @details
#' The function has two behaviors depending on the current selection in the
#' document:
#' \itemize{
#'   \item The current selection spans a single line. Different scenarios
#'   \itemize{
#'     \item The line doesn't start with \code{"#"} (ignoring leading spaces):
#'     the \code{"## "} is inserted after the leading space
#'     \item The line starts with \code{"## "} (ignoring leading spaces):
#'     the leading \code{"## "} is removed
#'     \item The line start with a regular comment \code{"#"}  (ignoring leading
#'     spaces): the comment (\code{"# "} or \code{"#"}) is replaced by
#'     \code{"## "}. For roxygen2 comments \code{"#'"} nothing happen.
#'   }
#'   \item The current selection spans multiple lines:
#'   \itemize{
#'     \item Any of the lines don't start with \code{"#"} (ignoring leading
#'     spaces): the \code{"## "} is inserted after the smallest leading space
#'     for all lines. For roxygen2 comment \code{"#'"} nothing happen.
#'     \item All lines start with \code{"## "} (ignoring leading spaces):
#'     all leading \code{"## "} are removed.
#'   }
#' }
#'
#' @export
insertDoubleHash <- function() {
  ## Get document context
  .context <- rstudioapi::getActiveDocumentContext()
  ## Get the lines of the current selection
  .range <- .context$selection[[1]]$range
  .idx <- .range$start[1]:.range$end[1]
  .line <- .context$contents[.idx]
  if (length(.line) == 1) { ## If selection is a single line
    ## Comment/uncomment line with '## '
    out <- insertDoubleHashOneLine(line = .line, range = .range)
    .line <- out$line
    .range <- out$range
  } else { ## If the selection spans multiple line
    ## Comment/uncomment lines with '## '
    out <- insertDoubleHashMultipleLines(lines = .line, range = .range)
    .line <- out$lines
    .range <- out$range
  }
  ## Apply changes to the document buffer
  for (i in seq_along(.idx)) {
    .start <- rstudioapi::document_position(.idx[i], 1)
    .stop <- rstudioapi::document_position(.idx[i], Inf)
    rstudioapi::modifyRange(location = rstudioapi::document_range(.start, .stop),
                            text = .line[i],
                            id = .context$id)
  }
  ## Set cursor at initial location
  rstudioapi::setCursorPosition(position = .range$end,
                                id = .context$id)
  ## Set selection at initial range
  rstudioapi::setSelectionRanges(ranges = .range,
                                 id = .context$id)
}

## Function to comment or uncomment a single line
## line: a string
## range: a document_range object
insertDoubleHashOneLine <- function(line, range){
  if(length(line) > 1) stop("'line' should be of length one")
  if (!grepl("^[ ]*#", line)) {
    ## Case 1: no comment present, add '## '
    line <- gsub("(^[ ]*)(.*$)", "\\1## \\2", line)
    range$end[2] <- range$end[2] + 3
    range$start[2] <- range$start[2] + 3
  } else if (grepl("^[ ]*## ", line)) {
    ## Case 2: (trimmed) lines starts with '## ', remove it
    line <- gsub("(^[ ]*)## (.*$)", "\\1\\2", line)
  } else if (grepl("^[ ]*#", line)) {
    ## Case 3: lines starts with '#', replace with ''
    ## Note: for roxygen2 comments "#'" nothing happens
    line <- gsub("(^[ ]*#)[ ]*(.*$)", "\\1\\2", line)
    line <- gsub("(^[ ]*)#([^'#].*$)", "\\1## \\2", line)
  }
  return(list(line = line, range = range))
}

## Function to comment or uncomment multiple lines
## line: a vector of string of length greater than 1
## range: a document_range object
insertDoubleHashMultipleLines <- function(lines, range){
  if(length(lines) <= 1) stop("'lines' should be of length greater than one")
  ## Get the lowest number of heading spaces
  .nspace <- min(nchar(gsub("(^[ ]*).*", "\\1", lines)))
  if (any(!grepl("^[ ]*#[#']", lines))) {
    ## Add double hash if any line is not commented
    lines <- gsub(paste0("(^[ ]{", .nspace, "})(.*$)"), "\\1## \\2", lines)
    range$start[2] <- 1
    range$end[2] <- Inf
  } else {
    ## Else remove the double hash
    lines <- gsub(paste0("(^[ ]{", .nspace, "})## (.*$)"), "\\1\\2", lines)
  }
  return(list(lines = lines, range = range))
}
