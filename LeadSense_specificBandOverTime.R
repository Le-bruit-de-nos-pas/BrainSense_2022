library(LeadSense)

data(dataset)

impedance_summary(dataset)
lfp_data(dataset)

dataset2 <- jsonlite::fromJSON("Report_Json_Session_Report_20240323T174308.json")

summary_long(dataset2)

impedance_summary(dataset2)

lfp_data(dataset2)


library(signal)   # for butterworth filter
library(seewave)  # for hilbert transform (optional, or use stats::filter)


# Define beta band
beta_band <- c(13, 35)
sampling_rate <- 250  # Adjust based on your dataset (Hz)

# Butterworth bandpass filter
butter_filter <- signal::butter(n = 4, W = beta_band / (sampling_rate / 2), type = "pass")

# Loop through each time-domain signal
for (i in seq_along(dataset2$LfpMontageTimeDomain$TimeDomainData)) {
  raw_signal <- dataset2$LfpMontageTimeDomain$TimeDomainData[[i]]

  # Apply bandpass filter
  beta_filtered <- signal::filtfilt(butter_filter, raw_signal)

  # Get Hilbert envelope (amplitude)
  beta_envelope <- Mod(seewave::hilbert(beta_filtered, f = sampling_rate))


  # Time vector (optional, assuming uniform sampling)
  time_vec <- seq(0, length(beta_envelope) - 1) / sampling_rate

  # Plot envelope
  plot(time_vec, beta_envelope, type = "l",
       main = paste("Beta Band Envelope - Pass", i),
       xlab = "Time (seconds)", ylab = "Amplitude (uV)",
       col = "darkorange", lwd = 2)
}


length(dataset2$LfpMontageTimeDomain$TimeDomainData)



library(seewave)

# Assume sampling rate is 250 Hz (adjust if needed)
sampling_rate <- 250  

# Combine all time-domain chunks into one long signal
full_signal <- unlist(dataset2$LfpMontageTimeDomain$TimeDomainData)

sampling_rate <- 250  # adjust if needed

seewave::spectro(full_signal,
                 f = sampling_rate,
                 wl = 512,
                 ovlp = 75,
                 dB = "max0",
                 main = "Full Spectrogram - All Passes Combined",
                 collevels = seq(-100, 0, by = 1))