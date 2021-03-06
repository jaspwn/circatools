#' @import behavr
#' @import data.table
#' @importFrom signal butter filtfilt
#' @importFrom  pracma findpeaks
#' @export

peak_returnR <- function(dt_curated, filterHours = 16, minpeakdist = 20) {

  #calculate DAM sample rate to pass to bp filter design
  sampRate <- diff(dt_curated[1:2, t])

  #low pass filter design to remove high frequency activity components
  bpfilt <- butter(n = 2, W = c((1/hours(filterHours))/((1/sampRate)/2)), type = "low", plane = "z")

  #filter activity across entire experiment by individual
  dt_curated[, bpfiltered := as.vector(filtfilt(bpfilt, x = activity)),
             by = c("id", "phase")]


  #find daily peak in activity for each fly and remap it to same metadata
  dt_peaks <- dt_curated[, data.table(findpeaks(bpfiltered,
                                                zero = "0",
                                                minpeakdistance = minpeakdist*60,
                                                peakpat = "[+]{10,}[0]*[-]{10,}",
                                                npeaks = floor(length(t)/1440))),
                         by = c("id", "phase")]

  setnames(dt_peaks, c("V1", "V2", "V3", "V4"), c("height", "peak", "start", "end"))
  setorderv(dt_peaks, cols = c("id", "phase",  "peak"))
  setkeyv(dt_peaks, "id")
  metadata <- dt_curated[, meta = TRUE]
  setmeta(dt_peaks, metadata)
  dt_peaks[, .(id, uid) , meta = TRUE]
  dt_peaks[, uid := xmv(uid)]

  ## add rank column and phase times
  dt_peaks <- dt_peaks[, peak_no := rank(peak),
                       by = c("id", "phase")]
  dt_peaks[, peak_phase := peak%%1440]
  dt_peaks[, peak_time := peak_phase/1440*24]

  return(dt_peaks)
}
