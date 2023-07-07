sigmoid <- function(x_from, factor, y_from, y_to, smooth = 5.5, n = 300) {
  x <- seq(-smooth, smooth, length = n)
  y <- exp(x) / (exp(x) + 1)
  out <- data.frame(x = (x + smooth) / (smooth * 2) * factor + x_from,
                    y = y * (y_to - y_from) + y_from)
}

flipped_sig <- function(x_from, factor, y_from, y_to, smooth = 5.5, n = 300) {
  x <- seq(-smooth, smooth, length = n)
  y <- -exp(-x) / (exp(-x) + 1)
  out <- data.frame(x = (x + smooth) / (smooth * 2) * factor + x_from,
                    y = y * (y_to - y_from) + y_to)
}