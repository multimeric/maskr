
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maskr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/maskr)](https://CRAN.R-project.org/package=maskr)
<!-- badges: end -->

The `maskr` package provides a masked array type, which is either a
vector, array, matrix, or data frame which has a *mask*. The mask is a
list of indices, one for each axis of the array, which defines the parts
of the array to show. All other parts then become invisible.

## Installation

You can install the development version of maskr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("multimeric/maskr")
```

## Basics

You can create a `MaskedArray` using the constructor function, which
takes firstly the array to mask, and secondly a list of mask vectors,
one for each dimension of the array. For a masked vector, the mask is a
list of one vector.

Here we define an inner array with 10 elements, but our mask, which is a
vector of indices to show, hides `"a"` and `"j"`:

``` r
x = maskr::MaskedArray(letters[1:10], masks=list(2:9))
x
#> An object of class "MaskedArray"
#> Slot "value":
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
#> 
#> Slot "masks":
#> [[1]]
#> [1] 2 3 4 5 6 7 8 9
```

For most purposes, the array acts the same as
`c("b", "c", "d", "e", "f", "g", "h", "i")`:

``` r
length(x)
#> [1] 8
```

``` r
x[[1]]
#> [1] "b"
```

However, we can unmask the array the return to the original form:

``` r
maskr::unmask(x)
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
```

## Use Cases

One use case for `maskr` is hiding some kind of bad data, like outliers.

For example, let’s start with some data that includes two outliers

``` r
set.seed(1)
x = c(
  rnorm(10),
  rnorm(2, mean=2)
)
x
#>  [1] -0.6264538  0.1836433 -0.8356286  1.5952808  0.3295078 -0.8204684
#>  [7]  0.4874291  0.7383247  0.5757814 -0.3053884  3.5117812  2.3898432
```

We could identify these outliers using some kind of test. In this case
we test that all the values come from from a $N(0, 1)$ distribution:

``` r
outliers = pnorm(x, lower.tail = F) < 0.05
outliers
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE
```

Then we can use that outlier list as a mask. Note that logical vectors
are also legal masks!

``` r
masked = maskr::MaskedArray(x, mask = list(!outliers))
masked
#> An object of class "MaskedArray"
#> Slot "value":
#>  [1] -0.6264538  0.1836433 -0.8356286  1.5952808  0.3295078 -0.8204684
#>  [7]  0.4874291  0.7383247  0.5757814 -0.3053884  3.5117812  2.3898432
#> 
#> Slot "masks":
#> [[1]]
#>  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
```

We might then want to find the mean of the non-outliers:

``` r
mean(masked)
#> [1] 0.1322028
```

## Subsetting

Another useful feature of `maskr` is that, when you subset an array, the
mask shrinks to hide the excluded elements:

``` r
x[5:6]
#> [1]  0.3295078 -0.8204684
```

What this means, is that you can take a sequence of slices from the
array:

``` r
y = x[-1][-1][-1]
```

We can compute the final subset:

``` r
maskr::apply_mask(y)
#> NULL
```

But also revert the mask:

``` r
maskr::unmask(y)
#> NULL
```

## Multidimensional Arrays

We can mask a matrix. If we do, the mask needs to be a list of two
vectors:

``` r
x = maskr::MaskedArray(matrix(rnorm(n=16), nrow=4), list(1:2, 2:3))
x
#> An object of class "MaskedArray"
#> Slot "value":
#>             [,1]        [,2]        [,3]        [,4]
#> [1,] -0.62124058 -0.01619026  0.91897737  0.61982575
#> [2,] -2.21469989  0.94383621  0.78213630 -0.05612874
#> [3,]  1.12493092  0.82122120  0.07456498 -0.15579551
#> [4,] -0.04493361  0.59390132 -1.98935170 -1.47075238
#> 
#> Slot "masks":
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 2 3
```

``` r
maskr::apply_mask(x)
#>             [,1]      [,2]
#> [1,] -0.01619026 0.9189774
#> [2,]  0.94383621 0.7821363
```

We can also mask a `data.frame` in the same way. Note that we can use a
character vector to select certain columns, as you might expect:

``` r
x = maskr::MaskedArray(iris, list(10:15, c("Petal.Length", "Sepal.Length")))
maskr::apply_mask(x)
#>    Petal.Length Sepal.Length
#> 10          1.5          4.9
#> 11          1.5          5.4
#> 12          1.6          4.8
#> 13          1.4          4.8
#> 14          1.1          4.3
#> 15          1.2          5.8
```
