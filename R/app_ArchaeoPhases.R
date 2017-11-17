#####################################################
#        Run ArchaeoPhases shiny apps               #
#####################################################

#' Run ArchaeoPhases shiny apps
#' 
#' 
#' @export app_ArchaeoPhases
app_ArchaeoPhases <- function() {
  
  app <- shiny::runApp(system.file(paste0("shiny/"), package = "ArchaeoPhases"), launch.browser = TRUE)
  
}