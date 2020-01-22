#' Add a double hash comment
#'
#' The function inserts
#'
#' @import rstudioapi
#' @return
#' @export
#' rstudioapi::getActiveDocumentContext()
#' @examples
insertDoubleHash <- function() {
  ## Get document context
  .context <- rstudioapi::getActiveDocumentContext()
  ## Get the line of the current cursor position
  .idx <- .context$selection[[1]]$range$start[1]
  .line <- .context$contents[.idx]
  ## Modify the line content to insert/delete the comment double hash
  if (!grepl("^[ ]*#", .line)) {
    ## Case 1: no comment present, add '## '
    .line <- gsub("(^[ ]*)(.*$)", "\\1## \\2", .line)
  } else if (grepl("^[ ]*## ", .line)) {
    ## Case 2: (trimmed) lines starts with '## ', remove it
    .line <- gsub("(^[ ]*)## (.*$)", "\\1\\2", .line)
  } else if (grepl("^[ ]*#", .line)) {
    ## Case 3: lines starts with '#', replace with ''
    ## Note: for roxygen2 comments "#'" nothing happens
    .line <- gsub("(^[ ]*#)[ ]*(.*$)", "\\1\\2", .line)
    .line <- gsub("(^[ ]*)#([^'#].*$)", "\\1## \\2", .line)
  }
  ## Apply changes to the document buffer
  .start <- rstudioapi::document_position(.idx, 1)
  .stop <- rstudioapi::document_position(.idx, Inf)
  rstudioapi::modifyRange(location = rstudioapi::document_range(.start, .stop),
                          text = .line,
                          id = .context$id)
  ## Set cursor at initial location
  rstudioapi::setCursorPosition(position = .context$selection[[1]]$range$end,
                                id = .context$id)
  ## Set selection at initial range
  rstudioapi::setSelectionRanges(ranges = .context$selection[[1]]$range,
                                 id = .context$id)
}














































