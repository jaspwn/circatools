#' @import data.table
#' @export

## adds a logical rhythmic column (defaults to TRUE) for each phase of FR found in experiment ie phase name contains "FR"

rhythmcols <- function(dt_curated. = dt_curated, metadata. = metadata) {

  FR_phases <- unique(dt_curated.[, phase])[grep("FR" ,unique(dt_curated.[, phase]))]

  metadata. <- lapply(FR_phases, function(x) {metadata.[, paste0(x, "_rhythmic") := TRUE]})

  return(metadata.)

}
