#' Read the results from a PCIbex experiment
#'
#' Converts the comma-separated PCIbex results file into rectangular format.
#'
#' @param file Path to the results file
#'
#' @return A dataframe
#' @export
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
    gsub("(^# \\d+\\. |\\.$)", "", colnames_lines)
  }
  ref_colnames <- setdiff(parse_colnames(1:blocks[[1]][1]), "Comments")
  block_colnames <- lapply(block_lines, function(block) {
    cols <- unique(c(ref_colnames, parse_colnames(block$colnames)))
    cols <- setdiff(cols, "Controller name")
    gsub("\\W", "", gsub("\\s", "_", cols))
  })
  all_colnames <- unique(unlist(block_colnames, use.names = FALSE))

  block_data <- lapply(seq_len(length(blocks)), function(i) {
    utils::read.csv(text = results_raw[block_lines[[i]]$data], col.names = block_colnames[[i]])
  })

  for (i in seq_len(length(block_data))) {
    block_data[[i]][, setdiff(all_colnames, colnames(block_data[[i]]))] <- NA
  }

  result <- do.call(rbind, block_data)

  if ("dplyr" %in% loadedNamespaces()) {
    dplyr::as_tibble(result)
  } else {
    result
  }

}
