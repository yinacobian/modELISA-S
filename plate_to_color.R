library(tidyverse)
library(readr)


sample_to_info <- read_csv("sample-to-info.csv")
tableFile <- "file-to-info.csv"
if(! file.exists(tableFile)) stop('Error -- tableFile file not found.')
file_table <- read.csv(tableFile,stringsAsFactors=FALSE)
j_table <- left_join(file_table,sample_to_info,by =c("row_B_sample" = "SAMPLE_ID"))
x <- j_table[1,]
kk <- apply(j_table,1, function(x){
  plate <- as.numeric(x["Plate_in_file"])
  filename <- x["File_name"] 
  tablepath <- paste0("table_parts/",sprintf("%02d",plate),"_",tools::file_path_sans_ext(filename),".tab")
  main_t <- paste0(x["row_B_sample"],' group: ',x["GROUP"])
  if(! file.exists(tablepath)) stop(paste0('Error -- tableFile ' , tablepath , ' file not found.'))
  #print(tablepath)
  raw_table <- read.table(tablepath,header = TRUE)
  raw_table
  antigens
  rownames(raw_table) <- c("A:standard","B:HIV-p24","C:Brisavirus","D:Vientovirus",
                 "E:Coronavirus 229E","F:Coronavirus HKU1","G:Norovirus","H:GFP")
  colnames(raw_table) <- c("R1-D1","R1-D2","R1-D3","R1-D4","R2-D1","R2-D2","R2-D3","R2-D4","R3-D1","R3-D2","R3-D3","R3-D4")
  raw_table %>% 
    rownames_to_column(var="antigen") %>%
    pivot_longer(`R1-D1`:`R3-D4`,names_to = 'dilutions') %>%
    mutate(patient=main_t)
})



T <- read.table("table_parts/01_2022-02-11-raw-plate.tab", header=TRUE)



heatmap(as.matrix(T), Colv = NA, Rowv = NA, revC=TRUE, 
        labRow=c("A:standatd","B:HIV-p24","C:Brisavirus","D:Vientovirus","E:Coronavirus 229E","F:Coronavirus HKU1","G:Norovirus","H:GFP"),
        labCol=c("R1-D1","R1-D2","R1-D3","R1-D4","R2-D1","R2-D2","R2-D3","R2-D4","R3-D1","R3-D2","R3-D3","R3-D4"), 
        main = "Healthy 832, saliva, IgA")

