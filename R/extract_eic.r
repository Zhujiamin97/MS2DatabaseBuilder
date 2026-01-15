# R/extract_eic.R
#' Extract EIC (Extracted Ion Chromatogram) for a Compound
#'
#' This function extracts the EIC from MS data for a given compound
#' based on its m/z and retention time.
#'
#' @param msdata An mzR object containing the MS data.
#' @param compound_mz The m/z value of the compound.
#' @param compound_rt The retention time of the compound (in seconds).
#' @param mz_tolerance The m/z tolerance for EIC extraction (default: 0.01).
#' @param rt_window The retention time window around the compound (default: 30 seconds).
#'
#' @return A data frame containing the EIC with columns: retentionTime, EIC, msLevel, scanNum.
#'
#' @examples
#' \dontrun{
#' # Assuming msdata is an opened mzR object
#' eic_data <- extract_eic_for_compound(
#'   msdata = msdata,
#'   compound_mz = 456.1234,
#'   compound_rt = 300,
#'   mz_tolerance = 0.01,
#'   rt_window = 30
#' )
#' }
#'
#' @keywords internal
extract_eic_for_compound <- function(msdata, 
                                     compound_mz, 
                                     compound_rt, 
                                     mz_tolerance = 0.01, 
                                     rt_window = 30) {
  
  hdr_all <- mzR::header(msdata)
  
  rt_min_sec <- max(0, compound_rt - rt_window)
  rt_max_sec <- compound_rt + rt_window
  
  mz_min <- compound_mz - mz_tolerance
  mz_max <- compound_mz + mz_tolerance
  
  rt_filtered <- which(hdr_all$retentionTime >= rt_min_sec & 
                         hdr_all$retentionTime <= rt_max_sec)
  
  if (length(rt_filtered) == 0) {
    return(data.frame(
      retentionTime = numeric(),
      EIC = numeric(),
      msLevel = integer(),
      scanNum = integer()
    ))
  }
  
  eic_intensity <- numeric(length(rt_filtered))
  retention_time <- numeric(length(rt_filtered))
  ms_level <- integer(length(rt_filtered))
  scan_num <- integer(length(rt_filtered))
  
  for (i in seq_along(rt_filtered)) {
    scan_idx <- rt_filtered[i]
    scan_num[i] <- hdr_all$seqNum[scan_idx]
    ms_level[i] <- hdr_all$msLevel[scan_idx]
    retention_time[i] <- hdr_all$retentionTime[scan_idx]
    
    scan_data <- mzR::peaks(msdata, scan_idx)
    
    if (is.null(scan_data) || nrow(scan_data) == 0) {
      eic_intensity[i] <- 0
    } else {
      mz_values <- scan_data[, 1]
      intensity_values <- scan_data[, 2]
      
      target_indices <- which(mz_values >= mz_min & mz_values <= mz_max)
      
      if (length(target_indices) > 0) {
        eic_intensity[i] <- sum(intensity_values[target_indices])
      } else {
        eic_intensity[i] <- 0
      }
    }
  }
  
  data.frame(
    retentionTime = retention_time,
    EIC = eic_intensity,
    msLevel = ms_level,
    scanNum = scan_num
  )
}