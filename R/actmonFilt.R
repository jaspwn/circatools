#' @import data.table
#' @export

actmonFiltR <- function(x, filtkey = c("CT", "PnF")) {

  #create output drive if it doesnt exist
  if (!dir.exists(paste(dirname(x), "/filtered", sep = ""))) {
    dir.create(paste(dirname(x), "/filtered", sep = ""))
  }

  dt <- data.table(fread(x, sep = "\t"))
  dt_new <- dt[V8 %in% filtkey]
  write.table(dt_new, file = paste(dirname(x), "/filtered/filtered_", basename(x), sep = ""), sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, eol = "\r\n")

  message(paste("Filtered output saved in ", dirname(x), "/filtered/filtered_", basename(x), sep = ""))
}
