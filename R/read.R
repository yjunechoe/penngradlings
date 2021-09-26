#' Read the results from a PCIbex experiment
#'
#' Converts a comma-separated PCIbex results file into rectangular format.
#'
#' @param file Path to the results file
#'
#' @return A dataframe
#' @export
#' @examples
#' \dontrun{
#' # Takes a few seconds because it's reading from remote
#' # but the parsing itself is very fast
#' read_pcibex("https://raw.githubusercontent.com/yjunechoe
#'              /Semantic-Persistence/master/data/result.txt")
#' }
read_pcibex <- function(file) {

  results_raw <- readLines(file, warn = FALSE)
  results_raw <- gsub(",+$", "", results_raw)

  blocks <- asplit(matrix(cumsum(rle(grepl("^#", results_raw))$lengths), ncol = 2, byrow = TRUE), 1)
  ref_lines <- c(1, sapply(blocks, `[`, 2)[-length(blocks)] + 1)

  block_lines <- lapply(seq_len(length(blocks)), function(i) {
    block <- lapply(blocks[[i]], function(x) {
      if (x == blocks[[i]][1]) {
        ref_lines[i]:x
      } else {
        (blocks[[i]][1] + 1):x
      }
    })
    names(block) <- c("colnames", "data")
    block
  })

  parse_colnames <- function(lines) {
    colnames_lines <- results_raw[lines]
    colnames_lines <- colnames_lines[grepl("^# \\d+", colnames_lines)]
    colnames_names <- as.list(gsub("(^# \\d+\\. |\\.$)", "", colnames_lines))
    names(colnames_names) <- as.integer(gsub("^# (\\d+).*$", "\\1", colnames_lines))
    colnames_names
  }
  ref_colnames <- parse_colnames(1:blocks[[1]][1])
  block_colnames <- lapply(block_lines, function(block) {
    colnames_list <- utils::modifyList(ref_colnames, parse_colnames(block$colnames))
    colnames_vec <- make.names(as.character(colnames_list), unique = TRUE)
    setdiff(colnames_vec, "Controller name")
  })
  all_colnames <- unique(unlist(block_colnames, use.names = FALSE))

  block_data <- lapply(seq_len(length(blocks)), function(i) {
    utils::read.csv(text = results_raw[block_lines[[i]]$data], header = FALSE, col.names = block_colnames[[i]])
  })

  for (i in seq_len(length(block_data))) {
    diff_cols <- setdiff(all_colnames, colnames(block_data[[i]]))
    if (length(diff_cols) > 0) {
      block_data[[i]][, diff_cols] <- NA
    }
  }

  result <- do.call(rbind, block_data)

  if ("dplyr" %in% loadedNamespaces()) {
    dplyr::as_tibble(result)
  } else {
    result
  }

}
