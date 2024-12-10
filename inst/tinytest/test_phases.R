Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8') # Force locale
options(ArchaeoPhases.calendar = calendar("CE"))

# Build ========================================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
pha1 <- phases(eve, groups = list(phase_1 = c(1, 3), phase_2 = c(2, 4)))
pha2 <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1)

expect_true(all(pha1 == pha2))
names(pha2) <- c("A", "B")
expect_identical(names(pha2), colnames(pha2))

# Plot =========================================================================
if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 1000) # pixels
  options(tinysnapshot_os = "Linux")

  ## Phases --------------------------------------------------------------------
  plot_phase_decr <- function() plot(pha1, decreasing = TRUE)
  expect_snapshot_plot(plot_phase_decr, "plot_phase_decr")

  plot_phase_inc <- function() plot(pha1, decreasing = FALSE)
  expect_snapshot_plot(plot_phase_inc, "plot_phase_inc")

  ## Succession ----------------------------------------------------------------
  plot_phase_hiatus <- function() plot(pha1, succession = "hiatus")
  expect_snapshot_plot(plot_phase_hiatus, "plot_phase_hiatus")

  plot_phase_transition <- function() plot(pha1, succession = "transition")
  expect_snapshot_plot(plot_phase_transition, "plot_phase_transition")

  plot_phase_succession <- function() plot(pha1, succession = c("transition", "hiatus"))
  expect_snapshot_plot(plot_phase_succession, "plot_phase_succession")

  expect_error(plot(pha1[, 1, ], succession = "hiatus"), "two phases")
}
