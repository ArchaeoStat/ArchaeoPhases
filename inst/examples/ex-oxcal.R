\dontrun{
## Download OxCal
oxcal_setup()

## Calibrate 14C dates
cal <- oxcal_calibrate(
  names = c("X","Y"),
  dates = c(5000, 2500),
  errors = c(25, 50)
)
cal

## Custom script
scr <- 'Plot()
 {
  R_Date("X",5000,25);
  R_Date("Y",2500,50);
 };'
out <- oxcal_execute(scr)
res <- oxcal_parse(out)
res
}
