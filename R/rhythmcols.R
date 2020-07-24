#' @export

## adds logical rhythmicity columns (default = TRUE) to metadata for each free-running (FR) phase of experiment
## and links back to curated behavr table.

# rhythmcols <- function(dt_curated. = dt_curated, phasePattern = "FR") {
#
#   FR_phases <- unique(dt_curated.[, phase])[grep(phasePattern, unique(dt_curated.[, phase]))]
#
#   for (i in 1:length(FR_phases)) {
#
#     FRname <- paste0(FR_phases[i], "_rhythmic")
#
#     dt_curated.[, test := where("FRname"), meta = TRUE]
#
#   }
#
#   return()
#
# }

rhythmcols <- function(dt_curated. = dt_curated, phasePattern = "FR") {

  metadata <- dt_curated[, meta = TRUE]

  FR_phases <- unique(dt_curated.[, phase])[grep(phasePattern, unique(dt_curated.[, phase]))]

  for (i in 1:length(FR_phases)) {

    metadata[, paste0(FR_phases[i], "_rhythmic") := TRUE]

  }

  setmeta(dt_curated., metadata)

  return(metadata)

}
