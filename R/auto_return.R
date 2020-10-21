#' @import behavr
#' @import data.table
#' @importFrom signal butter filtfilt
#' @importFrom  pracma findpeaks
#' @export

auto_returnR <- function(dt_curated, filterHours = 16, minpeakdist = 20) {

  #calculate DAM sample rate to pass to bp filter design
  sampRate <- diff(dt_curated[1:2, t])

  #low pass filter design to remove high frequency activity components
  bpfilt <- butter(n = 2, W = c((1/hours(filterHours))/((1/sampRate)/2)), type = "low", plane = "z")

  #filter activity across entire experiment by individual
  dt_curated[, bpfiltered := as.vector(filtfilt(bpfilt, x = activity)),
             by = c("id", "phase")]

  dt_curated[, autoco := acf(bpfiltered, lag.max = length(bpfiltered), plot = FALSE)$acf,
             by = c("id", "phase")]


  #find daily peak in activity for each fly and remap it to same metadata
  dt_auto <- dt_curated[, data.table(findpeaks(autoco,
                                               zero = "0",
                                               minpeakdistance = 18*60,
                                               peakpat = "[+]{1,}[0]*[-]{1,}",
                                               npeaks = floor(length(t)/1440))),
                        by = c("id", "phase")]

  setnames(dt_auto, c("V1", "V2", "V3", "V4"), c("height", "peak", "start", "end"))
  setorderv(dt_auto, cols = c("id", "phase",  "peak"))
  setkeyv(dt_auto, "id")
  metadata <- dt_curated[, meta = TRUE]
  setmeta(dt_auto, metadata)
  dt_auto[, uid := xmv(uid)]

  return(dt_auto)
}
