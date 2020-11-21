test_that("check_code works", {
    expect_true(check_code("BOCG_S_1_1"))
    expect_true(check_code("BOCG_B_1_1"))
    expect_true(check_code("BOCG_D_1_1_1"))

    expect_error(check_code("BOCG_D_1_1"))


})
