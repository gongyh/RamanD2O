#' Denosing using EEMD method
#'
#' This function smooths the input signal using the EEMD method.
#'
#' @importFrom hht EEMD EEMDCompile
#' @param xt The input signal.
#' @return The smoothed signal.
#' @export
eemddenoise <- function(
    xt, tt = NULL,
    cv.index, cv.level, cv.tol = 0.1 ^ 3, cv.maxiter = 20, by.imf = FALSE,
    trials.dir = "EEMD", noise.amp = sd(xt) * 0.1 , trials = 5, nimf = 5,
    tol = sd(xt) * 0.1 ^ 2, max.sift = 20, stop.rule = "type1",
    boundary = "periodic", max.imf = 10) {

  if (is.ts(xt)) {
    xt <- as.numeric(xt)
  }

  if (is.null(tt)) tt <- seq_along(length(xt))

  ndata <- length(xt)
  cv.kfold <- nrow(cv.index)

  EEMD(xt, tt, noise.amp , trials, nimf, tol = tol, max.sift = max.sift,
       stop.rule = stop.rule, boundary = boundary,
       trials.dir = trials.dir, max.imf = max.imf)
  tmpemd <- EEMDCompile(trials.dir, trials, nimf)
  tmpemd$imf <- tmpemd$averaged.imfs
  tmpemd$residue <- tmpemd$averaged.residue[, 1]
  tmpnimf <- tmpemd$nimf

  cv.ndim <- min(cv.level, tmpnimf)

  R <- (sqrt(5) - 1) / 2 # 0.61803399000000003
  C <- 1 - R
  if (by.imf) {
    lambda <- matrix(0, 4, cv.ndim)
    lambda.range <- 10 * apply(
                               as.matrix(tmpemd$imf[, 1:cv.ndim]),
                               2,
                               function(t) {
                                 sqrt(sum(t ^ 2)) / sqrt(ndata)
                               })

    lambda[1, ] <- rep(0, cv.ndim)
    lambda[4, ] <- lambda.range
    lambda[2, ] <- lambda[4, ] / 2
    lambda[3, ] <- lambda[2, ] + C * (lambda[4, ] - lambda[2, ])

    perr <- lambdaconv <- NULL
    optlambda <- lambda[3, ]
    j <- 0
    repeat {
      for (i in 1:cv.ndim) {
        optlambda[i] <- lambda[2, i]
        predxt <- tmpxt <- xt
        for (k in 1:cv.kfold) {
          EEMD(xt[-cv.index[k, ]], tt[-cv.index[k, ]],
               noise.amp , trials, nimf, tol = tol, max.sift = max.sift,
               stop.rule = stop.rule, boundary = boundary,
               trials.dir = trials.dir, max.imf = max.imf)
          cvemd <- EEMDCompile(trials.dir, trials, nimf)
          cvemd$imf <- cvemd$averaged.imfs
          cvemd$residue <- cvemd$averaged.residue[, 1]
          for (m in 1:cv.ndim) {
            cvemd$imf[, m] <- cvemd$imf[, m] *
              (abs(cvemd$imf[, m]) > optlambda[m])
          }
          tmpxt[-cv.index[k, ]] <- apply(cvemd$imf, 1, sum) + cvemd$residue

          tmpindex1 <- cv.index[k, ] - 1
          if (tmpindex1[1] == 0) tmpindex1 <- c(tmpindex1[2], tmpindex1[-1])

          tmpindex2 <- cv.index[k, ] + 1
          if (tmpindex2[length(tmpindex2)] == ndata + 1) {
            tmpindex2 <- c(tmpindex2[-length(tmpindex2)],
                           tmpindex2[length(tmpindex2) - 1])
          }

          predxt[cv.index[k, ]] <- (tmpxt[tmpindex1] + tmpxt[tmpindex2]) / 2
        }
        f2 <- mean((predxt - xt) ^ 2)

        optlambda[i] <- lambda[3, i]
        predxt <- tmpxt <- xt
        for (k in 1:cv.kfold) {
          EEMD(xt[-cv.index[k, ]], tt[-cv.index[k, ]],
               noise.amp , trials, nimf, tol = tol, max.sift = max.sift,
               stop.rule = stop.rule, boundary = boundary,
               trials.dir = trials.dir, max.imf = max.imf)
          cvemd <- EEMDCompile(trials.dir, trials, nimf)
          cvemd$imf <- cvemd$averaged.imfs
          cvemd$residue <- cvemd$averaged.residue[, 1]
          for (m in 1:cv.ndim) {
            cvemd$imf[, m] <- cvemd$imf[, m] *
              (abs(cvemd$imf[, m]) > optlambda[m])
          }
          tmpxt[-cv.index[k, ]] <- apply(cvemd$imf, 1, sum) + cvemd$residue

          tmpindex1 <- cv.index[k, ] - 1
          if (tmpindex1[1] == 0) tmpindex1 <- c(tmpindex1[2], tmpindex1[-1])

          tmpindex2 <- cv.index[k, ] + 1
          if (tmpindex2[length(tmpindex2)] == ndata + 1) {
            tmpindex2 <- c(tmpindex2[-length(tmpindex2)],
                           tmpindex2[length(tmpindex2) - 1])
          }

          predxt[cv.index[k, ]] <- (tmpxt[tmpindex1] + tmpxt[tmpindex2]) / 2
        }
        f3 <- mean((predxt - xt) ^ 2)

        if (f3 < f2) {
          optlambda[i] <- lambda[3, i]
          optf <- f3
          lambda[1, i] <- lambda[2, i]
          lambda[2, i] <- lambda[3, i]
          lambda[3, i] <- R * lambda[2, i] + C * lambda[4, i]
        } else {
          optlambda[i] <- lambda[2, i]
          optf <- f2
          lambda[4, i] <- lambda[3, i]
          lambda[3, i] <- lambda[2, i]
          lambda[2, i] <- R * lambda[3, i] + C * lambda[1, i]
        }
        perr <- c(perr, optf)
        lambdaconv <- rbind(lambdaconv, optlambda)
      }

      stopping <- NULL
      for (i in 1:cv.ndim) {
        stopping <- c(stopping,
                      abs(lambda[4, i] - lambda[1, i]) / (abs(lambda[2, i])
                                                          + abs(lambda[3, i])))
      }
      # if (all(stopping < cv.tol) || abs(perr[cv.ndim*(j+1)+1]-
      # perr[cv.ndim*j+1])/perr[cv.ndim*j+1] < cv.tol || j > cv.maxiter) break

      if (all(stopping < cv.tol) || j > cv.maxiter) break

      j <- j + 1
    }
  } else {
    lambda <- matrix(0, 4, 1)
    lambda.range <- 10 * apply(as.matrix(tmpemd$imf[, 1]), 2, function(t) {
      sqrt(sum(t ^ 2)) / sqrt(ndata)
    })

    lambda[1, ] <- rep(0, 1)
    lambda[4, ] <- lambda.range
    lambda[2, ] <- lambda[4, ] / 2
    lambda[3, ] <- lambda[2, ] + C * (lambda[4, ] - lambda[2, ])

    perr <- lambdaconv <- NULL
    optlambda <- lambda[3, ]
    j <- 0
    repeat {
      # for (i in 1:cv.ndim) {
      optlambda[1] <- lambda[2, 1]
      predxt <- tmpxt <- xt
      for (k in 1:cv.kfold) {
        EEMD(xt[-cv.index[k, ]], tt[-cv.index[k, ]],
             noise.amp , trials, nimf, tol = tol, max.sift = max.sift,
             stop.rule = stop.rule, boundary = boundary,
             trials.dir = trials.dir, max.imf = max.imf)
        cvemd <- EEMDCompile(trials.dir, trials, nimf)
        cvemd$imf <- cvemd$averaged.imfs
        cvemd$residue <- cvemd$averaged.residue[, 1]
        for (m in 1:cv.ndim) {
          cvemd$imf[, m] <- cvemd$imf[, m] *
            (abs(cvemd$imf[, m]) > optlambda[1])
        }
        tmpxt[-cv.index[k, ]] <- apply(cvemd$imf, 1, sum) + cvemd$residue

        tmpindex1 <- cv.index[k, ] - 1
        if (tmpindex1[1] == 0) tmpindex1 <- c(tmpindex1[2], tmpindex1[-1])

        tmpindex2 <- cv.index[k, ] + 1
        if (tmpindex2[length(tmpindex2)] == ndata + 1) {
          tmpindex2 <- c(tmpindex2[-length(tmpindex2)],
                         tmpindex2[length(tmpindex2) - 1])
        }

        predxt[cv.index[k, ]] <- (tmpxt[tmpindex1] + tmpxt[tmpindex2]) / 2
      }
      f2 <- mean((predxt - xt) ^ 2)

      optlambda[1] <- lambda[3, 1]
      predxt <- tmpxt <- xt
      for (k in 1:cv.kfold) {
        EEMD(xt[-cv.index[k, ]], tt[-cv.index[k, ]],
             noise.amp , trials, nimf, tol = tol, max.sift = max.sift,
             stop.rule = stop.rule, boundary = boundary,
             trials.dir = trials.dir, max.imf = max.imf)
        cvemd <- EEMDCompile(trials.dir, trials, nimf)
        cvemd$imf <- cvemd$averaged.imfs
        cvemd$residue <- cvemd$averaged.residue[, 1]
        for (m in 1:cv.ndim) {
          cvemd$imf[, m] <- cvemd$imf[, m] *
            (abs(cvemd$imf[, m]) > optlambda[1])
        }
        tmpxt[-cv.index[k, ]] <- apply(cvemd$imf, 1, sum) + cvemd$residue

        tmpindex1 <- cv.index[k, ] - 1
        if (tmpindex1[1] == 0) tmpindex1 <- c(tmpindex1[2], tmpindex1[-1])

        tmpindex2 <- cv.index[k, ] + 1
        if (tmpindex2[length(tmpindex2)] == ndata + 1) {
          tmpindex2 <- c(tmpindex2[-length(tmpindex2)],
                         tmpindex2[length(tmpindex2) - 1])
        }

        predxt[cv.index[k, ]] <- (tmpxt[tmpindex1] + tmpxt[tmpindex2]) / 2
      }
      f3 <- mean((predxt - xt) ^ 2)

      if (f3 < f2) {
        optlambda[1] <- lambda[3, 1]
        optf <- f3
        lambda[1, 1] <- lambda[2, 1]
        lambda[2, 1] <- lambda[3, 1]
        lambda[3, 1] <- R * lambda[2, 1] + C * lambda[4, 1]
      } else {
        optlambda[1] <- lambda[2, 1]
        optf <- f2
        lambda[4, 1] <- lambda[3, 1]
        lambda[3, 1] <- lambda[2, 1]
        lambda[2, 1] <- R * lambda[3, 1] + C * lambda[1, 1]
      }
      perr <- c(perr, optf)
      lambdaconv <- rbind(lambdaconv, optlambda)
      #        }

      stopping <- NULL
      # for (i in 1:cv.ndim)
      stopping <- c(stopping, abs(lambda[4, 1] - lambda[1, 1]) /
                      (abs(lambda[2, 1]) + abs(lambda[3, 1])))
      # if (all(stopping < cv.tol) ||
      # abs(perr[cv.ndim*(j+1)+1]-perr[cv.ndim*j+1])/perr[cv.ndim*j+1] <
      # cv.tol || j > cv.maxiter) break

      if (all(stopping < cv.tol) || j > cv.maxiter) break

      j <- j + 1
    }
  }

  for (m in 1:cv.ndim) {
    if (by.imf) {
      tmpemd$imf[, m] <- tmpemd$imf[, m] * (abs(tmpemd$imf[, m]) > optlambda[m])
    } else {
      tmpemd$imf[, m] <- tmpemd$imf[, m] * (abs(tmpemd$imf[, m]) > optlambda)
    }
  }

  dxt <- apply(tmpemd$imf, 1, sum) + tmpemd$residue
  tmpemd$imf <- ts(tmpemd$imf, min(tt))
  tmpemd$residue <- ts(tmpemd$residue, min(tt))
  list(dxt = dxt, optlambda = optlambda, lambdaconv = lambdaconv,
       perr = perr, demd = tmpemd, niter = j)
}
