

#' Reverse jackknife
#'
#' modified from https://github.com/cran/biogeo/blob/master/R/rjack.R
#' also see https://code.google.com/archive/p/ala-dataquality/wikis/DETECTED_OUTLIER_JACKKNIFE.wiki (including diva implementation)
#'
#' @param d Vector of values to search for outliers
#' @param use_diva Logical.
#'
#' @returns Indices of (reverse jackknife) outliers
#' @export
#' @keywords internal
#'
#' @examples
#' x <- c(rnorm(10), 2)
#' rjack(x)
rjack <- function(d
                  , use_diva = TRUE
                  ) {

    d <- unique(d)

    rng <- diff(range(d))

    mx <- mean(d)

    n <- length(d)

    n1 <- n - 1

    t1 <- (0.95 * sqrt(n)) + 0.2

    if(use_diva) t1 <- t1 * rng / 50

    x <- sort(d)

    y <- rep(0, n1)

    for (i in 1:n1) {

        x1 <- x[i + 1]

        if (x[i] < mx) {

            y[i] <- (x1 - x[i]) * (mx - x[i])

        } else {

          y[i] <- (x1 - x[i]) * (x1 - mx)

        }
    }

    my <- mean(y)

    z <- y/(sqrt(sum((y - my)^2)/n1))

    out <- rep(0, length(d))

    if (any(z > t1)) {

        f <- which(z > t1)

        v <- x[f]

        if (any(v < median(x))) {     # modified: wrapped in 'any'

            xa <- (d <= max(v)) * 1 # modified: added 'max'

            out <- out + xa

        }

        if (any(v > median(x))) {     # modified: wrapped in 'any'

            xb <- (d >= min(v)) * 1 # modified: added 'min'

            out <- out + xb
        }

    } else {

        out <- out
    }

    which(out == 1)

}
