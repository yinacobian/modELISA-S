library(tidyverse)
library(readr)


sample_to_info <- read_csv("sample-to-info.csv")
tableFile <- "file-to-info.csv"
if(! file.exists(tableFile)) stop('Error -- tableFile file not found.')
file_table <- read.csv(tableFile,stringsAsFactors=FALSE)
j_table <- left_join(file_table,sample_to_info,by =c("row_E_sample" = "SAMPLE_ID"))

kk <- apply(j_table,1, function(x){
  plate <- as.numeric(x["Plate_in_file"])
  filename <- x["File_name"] 
  tablepath <- paste0("table_parts/",sprintf("%02d",plate),"_",tools::file_path_sans_ext(filename),".tab")
  main_t <- paste0(x["row_E_sample"],' group: ',x["GROUP"])
  if(! file.exists(tablepath)) stop(paste0('Error -- tableFile ' , tablepath , ' file not found.'))
  raw_table <- read.table(tablepath,header = TRUE)
  rownames(raw_table) <- c("A:standard",
                           "B:HIV-HIV",
                           "C:HIV-BRISA",
                           "D:BRISA-BRISA",
                           "E:BRISA-HIV",
                           "F:VIENTO-VIENTO",
                           "G:VIENTO-HIV",
                           "H:No antigen")
  colnames(raw_table) <- c("serum 1/300",
                           "serum 1/600",
                           "serum 1/1200",
                           "serum 1/2400",
                           "serum 1/4800",
                           "serum 1/9600",
                           "serum 1/300 competitor 8 ug",
                           "serum 1/900 competitor 4 ug",
                           "serum 1/2700 competitor 2 ug",
                           "serum 1/8100 competitor 1 ug",
                           "serum 1/300 competitor 1/2 ug",
                           "serum 1/300 competitor 1/4 ug"
                           )
  raw_table %>% 
    rownames_to_column(var="antigen") %>%
    pivot_longer(`serum 1/300`:`serum 1/300 competitor 1/4 ug`,names_to = 'dilutions') %>%
    mutate(patient=main_t)
})

all_data <- Reduce(rbind,kk) %>%
  mutate(antigen=factor(antigen,rev(c("A:standard",
                                      "B:HIV-HIV",
                                      "C:HIV-BRISA",
                                      "D:BRISA-BRISA",
                                      "E:BRISA-HIV",
                                      "F:VIENTO-VIENTO",
                                      "G:VIENTO-HIV",
                                      "H:No antigen")))) %>%
  mutate(dilutions=factor(dilutions,c("serum 1/300",
                                      "serum 1/600",
                                      "serum 1/1200",
                                      "serum 1/2400",
                                      "serum 1/4800",
                                      "serum 1/9600",
                                      "serum 1/300 competitor 8 ug",
                                      "serum 1/900 competitor 4 ug",
                                      "serum 1/2700 competitor 2 ug",
                                      "serum 1/8100 competitor 1 ug",
                                      "serum 1/300 competitor 1/2 ug",
                                      "serum 1/300 competitor 1/4 ug"
                                      )))
                  
##Samples from all the experiment
IgG_plot <- all_data %>%
  ggplot( aes(dilutions, antigen,fill= value)) + 
  geom_tile(size=1) +
  scale_fill_gradient(low = "bisque", high = "darkorange3") +
  geom_text(aes(label=round(value,2))) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_equal() +
  facet_wrap(~ patient)
ggsave('intensities_plot.pdf',plot = IgG_plot, width = 15.2, height = 13.9, units = 'in', scale = 1.2)

