library(ggplot2)

stdom <- seq(-5.12, 5.12, 0.1)

f_field <- function(fdom, f) {
    # data gathering
    f_dom <- c()
    f_rang <- c()
    f_im <- c()
    for (x1 in fdom) {
        for (x2 in fdom) {
            # data gathering
            f_dom <- append(f_dom, x1)
            f_rang <- append(f_rang, x2)
            f_im <- append(f_im, f(x1, x2))
        }
    }
    f_map <- data.frame(
        "x1" = f_dom,
        "x2" = f_rang,
        "f_im" = f_im
    )
    return(f_map)
}

sphere_df <- f_field(fdom = seq(-15.12, 15.12, 0.1), funsphere)

head(sphere_df)

ggplot(sphere_df, aes(x1, x2)) +
    geom_raster(aes(fill = f_im)) +
    ggtitle(label = "Sphere") +
    theme_bw()

ggplot(sphere_df, aes(x1, x2)) +
    geom_contour(aes(z = f_im, colour = ..level..)) +
    ggtitle(label = "Sphere's Contour") +
    theme_bw()

# Rosenbrok's function

rosenbrok_df <- f_field(fdom = seq(-15.12, 15.12, 0.1), rosenbrok)

head(rosenbrok_df)

ggplot(rosenbrok_df, aes(x1, x2)) +
    geom_raster(aes(fill = f_im)) +
    ggtitle(label = "Rosenbrok") +
    theme_bw()

ggplot(rosenbrok_df, aes(x1, x2)) +
    geom_contour(aes(z = f_im, colour = ..level..)) +
    ggtitle(label = "Rosenbrok's Contour") +
    theme_bw()

# Rastrigin's function
rastrigin_df <- f_field(fdom = seq(-15.12, 15.12, 0.1), rastrigin)

head(rastrigin_df)

ggplot(rastrigin_df, aes(x1, x2)) +
    geom_raster(aes(fill = f_im)) +
    theme_void() + theme(legend.position = "none")

ggplot(rastrigin_df, aes(x1, x2)) +
    geom_contour(aes(z = f_im, colour = ..level..)) +
    theme_void() + theme(legend.position = "none")

# Schewefel's function
schewefel_df <- f_field(fdom = seq(-15.12, 15.12, 0.1), schewefel)

head(schewefel_df)

ggplot(schewefel_df, aes(x1, x2)) +
    geom_raster(aes(fill = f_im)) +
    ggtitle(label = "Schewefel") +
    theme_bw()

ggplot(schewefel_df, aes(x1, x2)) +
    geom_contour(aes(z = f_im, colour = ..level..)) +
    ggtitle(label = "Schewefel's Contour") +
    theme_minimal()

# Griewank's function
griewank_df <- f_field(fdom = seq(-15.12, 15.12, 0.1), griewank)

head(griewank_df)

ggplot(griewank_df, aes(x1, x2)) +
    geom_raster(aes(fill = f_im)) +
    ggtitle(label = "Griewank") +
    theme_bw()

ggplot(griewank_df, aes(x1, x2)) +
    geom_contour(aes(z = f_im, colour = ..level..)) +
    ggtitle(label = "Griewank's Contour") +
    theme_minimal()
