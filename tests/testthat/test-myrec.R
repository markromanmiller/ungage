test_that("simple", {
  current_log_threshold <- logger::log_threshold()
  logger::log_threshold(logger::DEBUG)
  session_temp_root <- tempdir()
  bids_root <- paste0(session_temp_root, "/ungage_bids")
  dir.create(bids_root)
  bd <- rbids::bids(bids_root, readonly = F)

  #myrec_to_bids("extra/week2_section7.myrec", bd, "session_id", tibble())
  # TODO: where should this myrec be?

  # get player names
  player_names <- myrec_player_names("../../extra/week2_section7.myrec")

  mapping <- seq_along(player_names)
  names(mapping) <- player_names

  myrec_to_bids("../../extra/week2_section7.myrec", bd, "testses", "testtask", mapping)

  expect_equal(list.files(bids_root), character())

  logger::log_threshold(current_log_threshold)
})

test_that("a bad file path gives a useful error", {
  expect_error(myrec_to_bids("fake-path", "also-fake", list(), "fake", "what"), regexp = "Could not find file.+")
})

# TODO: test list in string.

test_that("a bad session ID gives a useful error", {
  expect_error(myrec_to_bids("../../extra/week2_section7.myrec", "also-fake", list(), "fake", "what"), regexp = "`session_id` is not a character vector, it is `list`")
})

test_that("a bad task ID gives a useful error", {
  expect_error(myrec_to_bids("../../extra/week2_section7.myrec", "also-fake", "aef", 1, "what"), regexp = "`task_id` is not a character vector, it is `numeric`")
})
