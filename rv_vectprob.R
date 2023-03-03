# sum
funsum <- function(x1, x2) {
    return(x1 + x2)
}

funlinear <- function(x1, x2) {
    return(
        runif(n = 1) * x1 +
            runif(n = 1) * x2
    )
}

funstep <- function(x1, x2) {
    return(
        6 * 2 + floor(x1) + floor(x2)
    )
}

funsphere <- function(x1, x2) {
    return(
        (x1 ** 2) + (x2 ** 2)
    )
}

rosenbrok <- function(x1, x2) {
    return(
        ((1 - x1) ** 2) +
            100 * ((x2 - (x1 ** 2)) ** 2)
    )
}

rastrigin <- function(x1, x2) {
    return(
        10 * 2 +
            (x1 ** 2) +
            (x2 ** 2) -
            10 * (cos(2 * pi * x1) +
                      cos(2 * pi * x2))
    )
}

schewefel <- function(x1, x2) {
    return(
        -1 * (
            x1 * sin(sqrt(abs(x1))) +
                x2 * sin(sqrt(abs(x2)))
        )
    )
}

griewank <- function(x1, x2) {
    return(
        1 + (1 / 4000) * ((x1 ** 2) + (x2 ** 2)) -
            cos(x1) * cos(x2 / sqrt(2))
    )
}
