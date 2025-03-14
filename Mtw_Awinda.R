library(dplyr)
library(ggplot2)
library(readr)
library(signal)

# Load the file (adjust path if needed)
file_path <- "Kinetikos20200224_112904_00B42BF8.txt"

# Read the data, skipping metadata lines (assumes first non-comment row is headers)
raw_data <- read_delim(file_path, delim = "\t", skip = 6)


# Sampling rate
sampling_rate <- 40  # Hz
dt <- 1 / sampling_rate  # Time step in seconds

# High-pass filter to remove drift from AccX
hp_cutoff <- 0.1 / (sampling_rate / 2)  # Normalized cutoff frequency

butter_highpass <- butter(2, hp_cutoff, type = "high")

raw_data <- raw_data %>%
  mutate(AccY = filtfilt(butter_highpass, AccY))

# Integrate **only AccY** to get speed along y-axis
raw_data <- raw_data %>%
  mutate(speed_y = cumsum(AccY * dt))

# Apply **Zero-Velocity Update (ZUPT)**: Reset speed when acceleration is near zero
raw_data <- raw_data %>%
  mutate(speed_y = ifelse(abs(AccY) < 0.2, 0, speed_y))

# Apply low-pass filter to smooth speed
lp_cutoff <- 2 / (sampling_rate / 2)  # Normalized 2 Hz cutoff
butter_lowpass <- butter(2, lp_cutoff, type = "low")
raw_data <- raw_data %>%
  mutate(speed_y = filtfilt(butter_lowpass, speed_y))

# Plot speed over time
ggplot(raw_data, aes(x = (PacketCounter - min(PacketCounter)) / sampling_rate, y = speed_y)) +
  geom_line(color = "blue") +
  labs(title = "Speed Over Time Along Y-Axis (Xsens MTw Awinda)", 
       x = "Time (seconds)", y = "Speed (m/s)") +
  theme_minimal()

# Show first few rows
print(head(raw_data, 10))




library(dplyr)
library(ggplot2)
library(readr)

# Load the file (adjust path if needed)
file_path <- "Kinetikos20200224_112904_00B42BF8.txt"

# Read the data, skipping metadata lines (assumes first non-comment row is headers)
raw_data <- read_delim(file_path, delim = "\t", skip = 6)

# Sampling rate
sampling_rate <- 40  # Hz

# Convert PacketCounter to time (in seconds)
raw_data <- raw_data %>%
  mutate(time = PacketCounter / sampling_rate)

# Plot Yaw over time
ggplot(raw_data, aes(x = time, y = Yaw )) +
  geom_line(color = "blue") +
  labs(title = "Yaw Over Time (Xsens MTw Awinda)", 
       x = "Time (seconds)", y = "Yaw (degrees)") +
  theme_minimal()

# Show first few rows
print(head(raw_data, 10))


raw_data$Yaw



library(dplyr)
library(ggplot2)
library(readr)
library(signal)

# Load the file (adjust path if needed)
file_path <- "Kinetikos20200224_112904_00B42BF8.txt"

# Read the data, skipping metadata lines (assumes first non-comment row is headers)
raw_data <- read_delim(file_path, delim = "\t", skip = 6)

# Sampling rate
sampling_rate <- 40  # Hz
dt <- 1 / sampling_rate  # Time step in seconds

# Convert Yaw from degrees to radians (since cos() and sin() expect radians)
raw_data <- raw_data %>%
  mutate(Yaw_rad = Yaw * pi / 180)

# Compute **global forward acceleration** based on yaw angle
raw_data <- raw_data %>%
  mutate(Acc_forward = AccX * cos(Yaw_rad) + AccY * sin(Yaw_rad))

# High-pass filter to remove drift from Acc_forward
hp_cutoff <- 0.1 / (sampling_rate / 2)  # Normalized cutoff frequency
butter_highpass <- butter(2, hp_cutoff, type = "high")
raw_data <- raw_data %>%
  mutate(Acc_forward = filtfilt(butter_highpass, Acc_forward))

# Integrate to get forward speed
raw_data <- raw_data %>%
  mutate(speed_forward = cumsum(Acc_forward * dt))

# Apply Zero-Velocity Update (ZUPT): Reset speed when acceleration is near zero
raw_data <- raw_data %>%
  mutate(speed_forward = ifelse(abs(Acc_forward) < 0.2, 0, speed_forward))

# Apply low-pass filter to smooth speed
lp_cutoff <- 2 / (sampling_rate / 2)  # Normalized 2 Hz cutoff
butter_lowpass <- butter(2, lp_cutoff, type = "low")
raw_data <- raw_data %>%
  mutate(speed_forward = filtfilt(butter_lowpass, speed_forward))

# Convert PacketCounter to time (in seconds)
raw_data <- raw_data %>%
  mutate(time = PacketCounter / sampling_rate)

# Plot yaw-corrected speed over time
ggplot(raw_data, aes(x = time, y = speed_forward)) +
  geom_line(color = "blue") +
  labs(title = "Yaw-Corrected Forward Speed Over Time (Xsens MTw Awinda)", 
       x = "Time (seconds)", y = "Speed (m/s)") +
  theme_minimal()

# Show first few rows
print(head(raw_data, 10))











library(dplyr)
library(ggplot2)
library(readr)
library(signal)
library(tidyverse)

# Load the file (adjust path if needed)
file_path <- "Kinetikos20200224_105827_00B4184F.txt"


# Read the data, skipping metadata lines (assumes first non-comment row is headers)
raw_data <- read_delim(file_path, delim = "\t", skip = 6)

# Sampling rate
sampling_rate <- 40  # Hz
dt <- 1 / sampling_rate  # Time step in seconds

# High-pass filter to remove drift from AccY
hp_cutoff <- 0.1 / (sampling_rate / 2)  # Normalized cutoff frequency
butter_highpass <- butter(2, hp_cutoff, type = "high")
raw_data <- raw_data %>%
  mutate(AccY = filtfilt(butter_highpass, AccY))

# Integrate only AccY to get speed along Y-axis
raw_data <- raw_data %>%
  mutate(speed_y = cumsum(AccY * dt))

# Apply Zero-Velocity Update (ZUPT): Reset speed when acceleration is near zero
raw_data <- raw_data %>%
  mutate(speed_y = ifelse(abs(AccY) < 0.2, 0, speed_y))

# Apply low-pass filter to smooth speed
lp_cutoff <- 2 / (sampling_rate / 2)  # Normalized 2 Hz cutoff
butter_lowpass <- butter(2, lp_cutoff, type = "low")
raw_data <- raw_data %>%
  mutate(speed_y = filtfilt(butter_lowpass, speed_y))

# Convert PacketCounter to time (in seconds)
raw_data <- raw_data %>%
  mutate(time = PacketCounter / sampling_rate)

# Plot speed along the Y-axis over time
ggplot(raw_data, aes(x = time, y = speed_y)) +
  geom_line(color = "blue") +
  labs(title = "Speed Over Time Along Y-Axis (Xsens MTw Awinda)", 
       x = "Time (seconds)", y = "Speed (m/s)") +
  theme_minimal()

# Show first few rows
print(head(raw_data, 10))



id00b70b13 <- data.table::fread("id00b70b13.csv")

unique(id00b70b13$activity)

id00b70b13 <- id00b70b13 %>% filter(activity==1)

id00b70b13 <- id00b70b13 %>% select(activity, time_s, lh_x, lh_y, lh_z)



library(signal)
library(data.table)
library(tidyverse)

id00b70b13 <- data.table::fread("id00b70b13.csv")

unique(id00b70b13$activity)

id00b70b13 <- id00b70b13 %>% filter(activity==1)

id00b70b13 <- id00b70b13 %>% select(activity, time_s, lh_x, lh_y, lh_z)


# Assuming your data is stored in a variable named id00b70b13
data <- id00b70b13  # Use your dataset name

# Sampling rate (calculate from time_s)
sampling_rate <- 1 / mean(diff(data$time_s))  # Estimate Hz
dt <- 1 / sampling_rate  # Time step in seconds

# REMOVE the previous 9.81 multiplication if already in m/s²
data <- data %>%
  mutate(lh_y = lh_y * 9.8)  # Keep acceleration as is

# Flip AccY if speed was negative
data <- data %>%
  mutate(lh_y = -lh_y)  

# High-pass filter acceleration (0.1 Hz)
hp_cutoff <- 0.1 / (sampling_rate / 2)  
butter_highpass <- butter(2, hp_cutoff, type = "high")
data <- data %>%
  mutate(lh_y = filtfilt(butter_highpass, lh_y))

# Integrate to get speed along Y-axis
data <- data %>%
  mutate(speed_y = cumsum(lh_y * dt))

# Apply Zero-Velocity Update (ZUPT) with a fine-tuned threshold
# data <- data %>%
#   mutate(speed_y = ifelse(abs(lh_y) < 0.15, 0, speed_y))  # Adjusted threshold
# 
# # High-pass filter speed to remove long-term drift
# butter_highpass_speed <- butter(2, 0.01 / (sampling_rate / 2), type = "high")
# data <- data %>%
#   mutate(speed_y = filtfilt(butter_highpass_speed, speed_y))

# Apply low-pass filter to smooth speed
# lp_cutoff <- 2 / (sampling_rate / 2)  
# butter_lowpass <- butter(2, lp_cutoff, type = "low")
# data <- data %>%
#   mutate(speed_y = filtfilt(butter_lowpass, speed_y))

# Plot corrected speed along Y-axis
ggplot(data, aes(x = time_s, y = speed_y)) +
  geom_line(color = "blue") +
  labs(title = "Corrected Speed Over Time Along Y-Axis", 
       x = "Time (seconds)", y = "Speed (m/s)") +
  theme_minimal()

# Show first few rows
print(head(data, 10))









# Assuming your data is stored in a variable named id00b70b13
data <- id00b70b13  # Use your dataset name

# Sampling rate (calculate from time_s)
sampling_rate <- 1 / mean(diff(data$time_s))  # Estimate Hz
dt <- 1 / sampling_rate  # Time step in seconds

# Convert AccX from g to m/s² (if necessary)
data <- data %>%
  mutate(lh_x = lh_x * 9.8)  # Apply gravity scaling if needed

# Flip AccX if needed (if speed is negative when it shouldn't be)
data <- data %>%
  mutate(lh_x = lh_x)  

# High-pass filter acceleration (0.1 Hz) to remove drift
hp_cutoff <- 0.1 / (sampling_rate / 2)  
butter_highpass <- butter(2, hp_cutoff, type = "high")
data <- data %>%
  mutate(lh_x = filtfilt(butter_highpass, lh_x))

# Integrate to get speed along X-axis
data <- data %>%
  mutate(speed_x = cumsum(lh_x * dt))

# (Optional) Apply Zero-Velocity Update (ZUPT) if needed
# data <- data %>%
#   mutate(speed_x = ifelse(abs(lh_x) < 0.15, 0, speed_x))

# (Optional) Apply high-pass filter to speed to remove drift
# butter_highpass_speed <- butter(2, 0.01 / (sampling_rate / 2), type = "high")
# data <- data %>%
#   mutate(speed_x = filtfilt(butter_highpass_speed, speed_x))

# (Optional) Apply low-pass filter to smooth speed
# lp_cutoff <- 2 / (sampling_rate / 2)  
# butter_lowpass <- butter(2, lp_cutoff, type = "low")
# data <- data %>%
#   mutate(speed_x = filtfilt(butter_lowpass, speed_x))

# Plot corrected speed along X-axis
ggplot(data, aes(x = time_s, y = speed_x)) +
  geom_line(color = "red") +
  labs(title = "Speed Over Time Along X-Axis", 
       x = "Time (seconds)", y = "Speed (m/s)") +
  theme_minimal()

# Show first few rows
print(head(data, 10))

















# Assuming your data is stored in a variable named id00b70b13
data <- id00b70b13  # Use your dataset name

# Sampling rate (calculate from time_s)
sampling_rate <- 1 / mean(diff(data$time_s))  # Estimate Hz
dt <- 1 / sampling_rate  # Time step in seconds

# Convert AccX and AccY from g to m/s² (if necessary)
data <- data %>%
  mutate(lh_x = lh_x * 9.8,
         lh_y = lh_y * 9.8)  # Scale both axes if needed

# High-pass filter acceleration (0.1 Hz) to remove drift
hp_cutoff <- 0.1 / (sampling_rate / 2)  
butter_highpass <- butter(2, hp_cutoff, type = "high")
data <- data %>%
  mutate(lh_x = filtfilt(butter_highpass, lh_x),
         lh_y = filtfilt(butter_highpass, lh_y))

# Integrate to get speed along X and Y
data <- data %>%
  mutate(speed_x = cumsum(lh_x * dt),
         speed_y = cumsum(lh_y * dt))

# Plot both speeds together
ggplot(data, aes(x = time_s)) +
  geom_line(aes(y = speed_x, color = "X-Axis Speed")) +
  geom_line(aes(y = speed_y, color = "Y-Axis Speed")) +
  labs(title = "Speed Over Time (X and Y Axes)", 
       x = "Time (seconds)", y = "Speed (m/s)") +
  scale_color_manual(values = c("X-Axis Speed" = "red", "Y-Axis Speed" = "blue")) +
  theme_minimal()

# Show first few rows
print(head(data, 10))














# Assuming your data is stored in a variable named id00b70b13
data <- id00b70b13  # Use your dataset name

# Sampling rate (calculate from time_s)
sampling_rate <- 1 / mean(diff(data$time_s))  # Estimate Hz
dt <- 1 / sampling_rate  # Time step in seconds

# Convert from g to m/s² (if necessary)
data <- data %>%
  mutate(lh_x = lh_x * 9.8,
         lh_y = lh_y * 9.8)

# 🚀 **Swap X and Y, and flip one of them**
data <- data %>%
  mutate(lh_x_corrected = -lh_y,    # Swap: Forward motion is actually Y
         lh_y_corrected = lh_x)   # Sideways motion is actually -X

# High-pass filter acceleration (0.1 Hz)
hp_cutoff <- 0.1 / (sampling_rate / 2)  
butter_highpass <- butter(2, hp_cutoff, type = "high")
data <- data %>%
  mutate(lh_x_corrected = filtfilt(butter_highpass, lh_x_corrected),
         lh_y_corrected = filtfilt(butter_highpass, lh_y_corrected))

# Integrate to get corrected speeds
data <- data %>%
  mutate(speed_x = cumsum(lh_x_corrected * dt),  # Forward speed
         speed_y = cumsum(lh_y_corrected * dt))  # Lateral speed

# Plot corrected speeds
ggplot(data, aes(x = time_s)) +
  geom_line(aes(y = speed_x, color = "Forward Speed (X)")) +
  geom_line(aes(y = speed_y, color = "Lateral Speed (Y)")) +
  labs(title = "Final Corrected Forward vs. Sideways Speed", 
       x = "Time (seconds)", y = "Speed (m/s)") +
  scale_color_manual(values = c("Forward Speed (X)" = "blue", "Lateral Speed (Y)" = "red")) +
  theme_minimal()

# Show first few rows
print(head(data, 10))