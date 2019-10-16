# Test to make sure model outputs are consistent

context("Model results")


test_that("Model outputs are ", {

  # the test data has been truncated and falsified and generated only to produce a fast result
  well_params <- "./data/inputs/well_params_test_data.yml"
  elec_rates <- "./data/inputs/elec_rates_test_data.yml"
  config <- "./data/inputs/inputs_test_data.csv"
  output_file <- "./data/outputs/test_output.csv"

  # load test comparison output
  test_expected <- read.csv(file = "./data/comparison/comp_output.csv", sep = ',', header = TRUE)

  # run the model and generate results
  superwell::main(well_param_file = well_params,
                  elec_cost_file = elec_rates,
                  config_file = config,
                  output_csv = output_file)

  # read new output file
  test_observed <- read.csv(output_file, sep = ',', header = TRUE)

  # test dimension match
  expect_identical(dim(test_observed), dim(test_expected),
                   info = paste("Dimensions are not the same as expected."))

  # test output equivalency to expected
  expect_equivalent(test_observed, test_expected,
                    info = paste("Output dataset does not match what is expected."))

})
