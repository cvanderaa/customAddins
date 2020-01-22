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
  ## Get the lines of the current selection
  .range <- .context$selection[[1]]$range
  .idx <- .range$start[1]:.range$end[1]
  .line <- .context$contents[.idx]
  if (length(.line) == 1) { ## If selection is a single line
    ## Modify the line content to insert/delete the comment double hash
    if (!grepl("^[ ]*#", .line)) {
      ## Case 1: no comment present, add '## '
      .line <- gsub("(^[ ]*)(.*$)", "\\1## \\2", .line)
      .range$end[2] <- .range$end[2] + 3
      .range$start[2] <- .range$start[2] + 3
    } else if (grepl("^[ ]*## ", .line)) {
      ## Case 2: (trimmed) lines starts with '## ', remove it
      .line <- gsub("(^[ ]*)## (.*$)", "\\1\\2", .line)
    } else if (grepl("^[ ]*#", .line)) {
      ## Case 3: lines starts with '#', replace with ''
      ## Note: for roxygen2 comments "#'" nothing happens
      .line <- gsub("(^[ ]*#)[ ]*(.*$)", "\\1\\2", .line)
      .line <- gsub("(^[ ]*)#([^'#].*$)", "\\1## \\2", .line)
    }
  } else { ## If the selection spans multiple line
    ## Get the lowest number of heading spaces
    .nspace <- min(nchar(gsub("(^[ ]*).*", "\\1", .line)))
    if (any(!grepl("^[ ]*##", .line))) {
      ## Add double hash if any line is not commented
      .line <- gsub(paste0("(^[ ]{", .nspace, "})(.*$)"), "\\1## \\2", .line)
      .range$start[2] <- 1
      .range$end[2] <- Inf
    } else {
      ## Else remove the double hash
      .line <- gsub(paste0("(^[ ]{", .nspace, "})## (.*$)"), "\\1\\2", .line)
    }
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


