#' Read the results from a PCIbex experiment
#'
#' Converts a comma-separated PCIbex results file into rectangular format.
#'
#' @param file Path to the results file
#' @param encoding The name of the encoding to be assumed.
#' @param exclude_controller_name For backwards compatibility, whether to skip the column for `"Controller name"`.
#'   In a previous version of PCIbex (last checked: September 2021), `"Controller name"` appeared in the column
#'   specification as the third column but does not have associated values in the results. This has
#'   since been fixed, so the value is set to `FALSE` by default.
#' @param type_opts A list of arguments passed to `type.convert()`, or `readr::type_convert()` if tidyverse is installed.
#'
#' @export
#'
#' @return A dataframe
read_pcibex <- function(file, encoding = "UTF-8", exclude_controller_name = FALSE, type_opts = list()) {
  con <- file(file, encoding = encoding)
  results_raw <- readLines(con = con, warn = FALSE)
  results_raw <- gsub(",+$", "", results_raw)

  blocks <- asplit(matrix(cumsum(rle(grepl("^#", results_raw))$lengths), ncol = 2, byrow = TRUE), 1)
  ref_lines <- c(1, sapply(blocks, `[`, 2)[-length(blocks)] + 1)

  block_lines <- lapply(seq_along(blocks), function(i) {
    block <- lapply(blocks[[i]], function(x) {
      if (x == blocks[[i]][1]) {
        seq(ref_lines[i], x)
      } else {
        seq(blocks[[i]][1] + 1, x)
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
    colnames_vec <- unique(make.names(gsub("\\s+", "_", gsub("[^\\w\\s]", "", colnames_list, perl = TRUE))))
    if (exclude_controller_name) {
      setdiff(colnames_vec, "Controller_name")
    } else {
      colnames_vec
    }
  })
  all_colnames <- unique(unlist(block_colnames, use.names = FALSE))

  block_data <- lapply(seq_along(blocks), function(i) {
    out <- as.data.frame(do.call(rbind, strsplit(results_raw[block_lines[[i]]$data], ",")))
    colnames(out) <- block_colnames[[i]]
    out
  })

  for (i in seq_along(block_data)) {
    diff_cols <- setdiff(all_colnames, colnames(block_data[[i]]))
    if (length(diff_cols) > 0) {
      block_data[[i]][, diff_cols] <- NA
    }
  }

  result <- do.call(rbind, block_data)

  close(con)

  if (all(c("readr", "tibble") %in% rownames(utils::installed.packages()))) {
    asNamespace("tibble")$as_tibble(do.call(asNamespace("readr")$type_convert, utils::modifyList(type_opts, list(df = result))))
  } else {
    do.call(utils::type.convert, utils::modifyList(type_opts, list(x = result)))
  }
}

#' Opens .Last.value as path
#' @export
open_last <- function() {
  message(.Last.value)
  system2("open", .Last.value)
  invisible(.Last.value)
}
