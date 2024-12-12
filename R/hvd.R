#' Ideal Lowpass Filter
#'
#' This function applies an ideal lowpass filter to the input signal.
#'
#' @importFrom signal ifft
#' @param x The input signal.
#' @param fp Filter cutoff frequency (0 < fp < 0.5).
#' @return The filtered signal.
ilpf <- function(x, fp) {
  # Obtain the mean of the data:
  M <- mean(x)
  x1 <- x - M

  # Invoke an ideal pass filter using FFT:
  n <- length(x1)
  fft_x1 <- fft(x1)

  # Create a filter mask for the lowpass filter
  f <- (0:(n - 1)) / n # Create a frequency vector
  filter_mask <- rep(0, n)
  filter_mask[which(f <= fp)] <- 1
  filter_mask[which(f >= (1 - fp))] <- 1

  # Apply the filter
  fft_x1_filtered <- fft_x1 * filter_mask

  # Compute the inverse FFT
  x3 <- Re(signal::ifft(fft_x1_filtered))

  # Add the mean back to the data
  xf <- x3 + M

  return(xf)
}

#' Instantaneous Properties
#'
#' This function calculates the instantaneous amplitude, frequency,
#' and phase of a signal.
#'
#' @importFrom hht HilbertTransform
#' @importFrom signal unwrap
#' @param x The input signal.
#' @param Fs Sampling frequency.
#' @return A list containing the amplitude, frequency, and phase.
inst <- function(x, Fs) {
  x <- as.vector(x)
  Xc <- hht::HilbertTransform(x) # Analytic signal
  A <- Mod(Xc) # Envelope

  Xd <- Xc[2:length(x)] * Conj(Xc[1:(length(x) - 1)])
  omega1 <- Arg(Xd)
  om1 <- 0.5 * (c(0, omega1) + c(omega1, 0))
  Fq <- Fs * om1 / (2 * pi)  # Instantaneous frequency

  xH <- Im(Xc)
  ph1 <- atan2(xH, x)
  phi <- signal::unwrap(ph1)  # Instantaneous phase

  return(list(A = A, F = Fq, phi = phi))
}

#' Synchronous Demodulation
#'
#' This function performs synchronous demodulation of a signal.
#'
#' @importFrom hht HilbertTransform
#' @importFrom pracma cumtrapz
#' @param x The input signal.
#' @param omega_i The frequency vector.
#' @param fp Lowpass filter cutoff frequency.
#' @return A list containing the demodulated signal, amplitude, and phase.
synchdem <- function(x, omega_i, fp) {
  x <- as.vector(x)
  om <- as.vector(omega_i)
  Xc <- hht::HilbertTransform(x)
  xH <- Im(Xc)
  A <- Mod(Xc)
  Am <- mean(A[200:(length(x) - 200)])  # A, amplitude
  cs <- pracma::cumtrapz(om)
  xc <- Am * cos(cs)
  xs <- Am * sin(cs)
  x1 <- x * xc
  x2 <- x * xs
  x3 <- xH * xc
  x4 <- xH * xs
  Acos <- (x1 + x4) / Am
  Asin <- (x3 - x2) / Am

  AcosM <- ilpf(Acos, fp)
  AsinM <- ilpf(Asin, fp)
  Ai <- sqrt(abs((AcosM)^2 + (AsinM)^2))
  phi <- atan2(AsinM, AcosM)  # phase shift correction
  xi <- Ai * cos(pracma::cumtrapz(om) + phi)

  return(list(xi = xi, Ai = Ai, phi = phi))
}

#' Hilbert Vibration Decomposition
#'
#' This function implements a variant of the Hilbert Vibration Decomposition.
#'
#' @param x The input signal.
#' @param n The number of components to decompose.
#' @param fp Lowpass filter cutoff frequency.
#' @return A list containing the decomposed components, amplitudes,
#' angular frequencies, and relative standard deviations.
#' @export
hvd <- function(x, n, fp) {
  x <- as.vector(x)
  s <- numeric(n + 1)
  s[1] <- sd(x)

  if (s[1] == 0) {
    print("Zero signal")
    return(list(Y = NULL, A = NULL, om_r = NULL, dev = NULL))
  }

  Y <- matrix(0, nrow = length(x), ncol = n)
  A <- matrix(0, nrow = length(x), ncol = n)
  om_r <- matrix(0, nrow = length(x), ncol = n)

  for (k in 1:n) {

    Ft <- inst(x, 1)$F # Instantaneous frequency
    omf <- 2 * pi * ilpf(Ft, fp)  # Angular Frequency lowpass filtering

    synchdem_result <- synchdem(x, omf, fp)
    yi <- synchdem_result[[1]]
    Ai <- synchdem_result[[2]]

    Y[, k] <- yi
    A[, k] <- Ai
    om_r[, k] <- omf

    x <- x - yi
    s[k] <- sd(x) / s[1]

    if (k == 7) {
      dev <- c(1, diff(s))
      return(list(Y = Y, A = A, om_r = om_r, dev = dev))
    }
  }

  dev <- s  # Relative standard deviation of the components
  return(list(Y = Y, A = A, om_r = om_r, dev = dev))
}
