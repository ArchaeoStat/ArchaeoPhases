\dontrun{
## Download OxCal
oxcal_setup()

## Calibrate 14C dates
cal <- oxcal_calibrate(
  names = c("X","Y"),
  dates = c(5000, 4500),
  errors = c(45, 35)
)
plot(cal)

## Custom script
scr <- 'Plot()
 {
  Sequence("Sequence1")
  {
   Boundary("Begin");
   Phase("Phase1")
   {
    R_Date("Lab-1",5000,25);
    R_Date("Lab-2",4900,37);
   };
   Boundary("Between");
   Phase("Phase2")
   {
    R_Date("Lab-3",4800,43);
   };
   Boundary("End");
  };
 };'
out <- oxcal_execute(scr)
res <- oxcal_parse(out)
plot(res)
as.data.frame(res)
}
