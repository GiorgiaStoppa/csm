
matteo <- c("c", "d", "f", "g", "h")
paolo  <- c("a", "b", "c", "d", "f", "g", "h")
stefano <- c("a", "c", "d", "f", "g", "h")
davide <- purrr::set_names(x = c("f", "g"), nm = c("a", "b"))
davide_un <- c("f", "g")

test_that("Remove elements with absent names", {
    expect_equal(
        exclude_absent(matteo, davide),
        c("c", "d", "h")
    )
})

test_that("Keep the elements that are included", {
    expect_equal(
        exclude_absent(paolo, davide),
        paolo
    )
})

test_that("Check if it works even when only a subset of elements must included", {
    expect_equal(
        exclude_absent(stefano, davide),
        c("a", "c", "d", "f", "h")
    )
})

test_that("Error if the reference is not a names vector", {
    expect_error(
        exclude_absent(matteo, davide_un),
        class = "usethis_error",
        regexp = "'reference' must be a named vector"
    )
})

