test_that("boletin_sumario works", {
    skip_if_offline()
    # This summary had both with disposiciones with and without authors
    expect_error(boletin_sumario(14, 110), NA)
})