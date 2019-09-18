#' Events
#'
#' A data set containing information on the ages of four dated events.
#'
#' @format A data frame with 30,000 rows and 5 variables:
#' \describe{
#'  \item{iter}{iteration of the MCMC algorithm}
#'  \item{Event.2}{information on event 2}
#'  \item{Event.1}{information on event 1}
#'  \item{Event.22}{information on event 22}
#'  \item{Event.12}{information on event 12}
#' }
"Events"
#' Phases
#'
#' A data set containing information on the start and end dates of two phases.
#'
#' @format A data frame with 30,000 rows and 5 variables:
#' \describe{
#'  \item{iter}{iteration of the MCMC algorithm}
#'  \item{Phase.2.alpha}{start date of Phase 2}
#'  \item{Phase.2.beta}{end date of Phase 2}
#'  \item{Phase.1.alpha}{start date of Phase 1}
#'  \item{Phase.1.beta}{end date of Phase 1}
#' }
"Phases"
#' Calibration of a fishpond chronology
#'
#' A data set containing information on the ages of two fishpond deposits.
#'
#' @format A data frame with 55,965 rows and 12 variables
#' \describe{
#' \item{Iteration}{iteration of the MCMC algorithm}
#' \item{beta.2..Layer.II.}{end date of Layer II}
#' \item{theta.5..Layer.II.}{age of dated event 5 in Layer II}
#' \item{theta.4..Layer.II.}{age of dated event 4 in Layer II}
#' \item{theta.3..Layer.II.}{age of dated event 3 in Layer II}
#' \item{theta.2..Layer.II.}{age of dated event 2 in Layer II}
#' \item{alpha.2..Layer.II.}{start date of Layer II}
#' \item{beta.1..Layer.III.}{end date of Layer III}
#' \item{theta.1..Layer.III.}{age of dated event 1 in Layer III}
#' \item{alpha.1..Layer.III.}{start date of Layer III}
#' \item{phi.1}{floating parameter}
#' \item{X}{superfluous column}
#' }
"Fishpond"
#' Ksar Akil dates calibrated by ChronoModel
#'
#' A data set.
#'
#' @format A data frame with 30,000 rows and 17 variables:
#' \describe{
#'  \item{iter}{iteration of the MCMC algorithm}
#'  \item{Layer.V}{Layer V}
#'  \item{Layer.VI}{Layer VI}
#'  \item{Layer.XI}{Layer XI}
#'  \item{Layer.XII}{Layer XII}
#'  \item{Layer.XVI.4}{Layer XVI 4}
#'  \item{Layer.XVI.3}{Layer XVI 3}
#'  \item{Layer.XVI.1}{Layer XVI 1}
#'  \item{Layer.XVI.2}{Layer XVI 2}
#'  \item{Layer.XVII.2}{Layer XVII 2}
#'  \item{Layer.XVII.1}{Layer XVII 1}
#'  \item{Layer.XVII.3}{Layer XVII 3}
#'  \item{Layer.XVII.4}{Layer XVII 4}
#'  \item{Layer.XVIII}{Layer XVIII}
#'  \item{Layer.XIX}{Layer XIX}
#'  \item{Layer.XX}{Layer XX}
#'  \item{Layer.XXII}{Layer XXII}
#' }
"KADatesChronoModel"
#' Ksar Akil dates calibrated by OxCal
#'
#' A data set
#'
#' @format A data frame with 10,000 rows and 27 variables:
#' \describe{
#'  \item{Pass}{iteration of the MCMC algorithm}
#'  \item{Ethelruda}{Ethelruda}
#'  \item{start.dated.IUP}{start dated IUP}
#'  \item{GrA.53000}{GrA 5300}
#'  \item{end.dated.IUP}{end dated IUP}
#'  \item{start.Ahmarian}{start Ahmarian}
#'  \item{GrA.57597}{GrA 57597}
#'  \item{GrA.53004}{GrA 53004}
#'  \item{GrA.57542}{GrA 57542}
#'  \item{GrA.54846}{GrA 54846}
#'  \item{GrA.57603}{GrA 57603}
#'  \item{GrA.57602}{GrA 57602}
#'  \item{GrA.53001}{GrA 53001}
#'  \item{Egbert}{Egbert}
#'  \item{GrA.54847}{GrA 54847}
#'  \item{GrA.57599}{GrA 57599}
#'  \item{GrA.57598}{GrA 57598}
#'  \item{GrA.57544}{GrA 57544}
#'  \item{end.Ahmarian}{end Ahmarian}
#'  \item{start.UP}{start UP}
#'  \item{GrA.57545}{GrA 57545}
#'  \item{GrA.53006}{GrA 53006}
#'  \item{GrA.54848}{GrA 54848}
#'  \item{end.UP}{end UP}
#'  \item{start.EPI}{start EPI}
#'  \item{GrA.53005}{GrA 53005}
#'  \item{end.EPI}{end EPI}
#' }
"KADatesOxcal"
#' Ksar Akil phases calibrated by ChronoModel
#'
#' A data set
#'
#' @format A data frame with 30,000 rows and 9 variables:
#' \describe{
#'  \item{iter}{iteration of the MCMC algorithm}
#'  \item{EPI.alpha}{start date of EPI}
#'  \item{EPI.beta}{end date of EPI}
#'  \item{UP.alpha}{start date of UP}
#'  \item{UP.beta}{end date of UP}
#'  \item{Ahmarian.alpha}{start date of Ahmarian}
#'  \item{Ahmarian.beta}{end date of Ahmarian}
#'  \item{IUP.alpha}{start date of IUP}
#'  \item{IUP.beta}{end date of IUP}
#' }
"KAPhasesChronoModel"
