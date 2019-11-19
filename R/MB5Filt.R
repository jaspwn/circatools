#' @import data.table
#' @export

MB5FiltR <- function(x) {

  #create output drive if it doesnt exist
  if (!dir.exists(paste(dirname(x), "/filtered", sep = ""))) {
    dir.create(paste(dirname(x), "/filtered", sep = ""))
  }

  dt <- data.table(fread(x, sep = "\t"))
  dt_new <- dt[V7 == 0]
  write.table(dt_new, file = paste(dirname(x), "/filtered/filtered_", basename(x), sep = ""), sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, eol = "\r\n")

  message(paste("Filtered output saved in ", dirname(x), "/filtered/filtered_", basename(x), sep = ""))
}
