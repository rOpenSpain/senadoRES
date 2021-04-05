test_that("boletin_sumario works", {
    skip_if_offline()
    skip_on_cran()
    # This summary had both with disposiciones with and without authors
    expect_error(boletin_sumario(14, 110), NA)
})

test_that("boletin_sumario works well", {
    skip_if_offline()
    skip_on_cran()
    # This summary had both with disposiciones with and without authors
    expect_error(boletin_sumario(14, "27"), NA)
})
