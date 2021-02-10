##############################################################################
# FIPEX_run_DCI_Sectional.r
write("ERROR",file='out.txt')
source("FIPEX_output_to_R_input.r")
FIPEX_output_to_R_input()
source("dci_fxs.r")
x = try(dci_fxs(all_sections=T),silent=TRUE)
if(class(x)=='data.frame' | class(x)=='list'){
  write.table(x,file='out.txt')
} else{
  write("ERROR",file='out.txt')
}
