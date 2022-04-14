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
                           "B:HIV",
                           "C:GFP",
                           "D:No antigen",
                           "E:BRISA",
                           "F:VIENTO",
                           "G:BRISA",
                           "H:VIENTO")
  colnames(raw_table) <- c("serum 1/300 IgA",
                           "serum 1/900 IgA",
                           "serum 1/2700 IgA",
                           "serum 1/8100 IgA",
                           "serum 1/300 IgA competitor 4ug with biotin",
                           "serum 1/300 IgA competitor 4ug no biotin",
                           "serum 1/300 IgG",
                           "serum 1/900 IgG",
                           "serum 1/2700 IgG",
                           "serum 1/8100 IgG",
                           "serum 1/300 IgG competitor 4ug with biotin",
                           "serum 1/300 IgG competitor 4ug no biotin"
                           )
  raw_table %>% 
    rownames_to_column(var="antigen") %>%
    pivot_longer(`serum 1/300 IgA`:`serum 1/300 IgG competitor 4ug no biotin`,names_to = 'dilutions') %>%
    mutate(patient=main_t)
})

all_data <- Reduce(rbind,kk) %>%
  mutate(antigen=factor(antigen,rev(c("A:standard",
                                      "B:HIV",
                                      "C:GFP",
                                      "D:No antigen",
                                      "E:BRISA",
                                      "F:VIENTO",
                                      "G:BRISA",
                                      "H:VIENTO")))) %>%
  mutate(dilutions=factor(dilutions,c("serum 1/300 IgA",
                                      "serum 1/900 IgA",
                                      "serum 1/2700 IgA",
                                      "serum 1/8100 IgA",
                                      "serum 1/300 IgA competitor 4ug with biotin",
                                      "serum 1/300 IgA competitor 4ug no biotin",
                                      "serum 1/300 IgG",
                                      "serum 1/900 IgG",
                                      "serum 1/2700 IgG",
                                      "serum 1/8100 IgG",
                                      "serum 1/300 IgG competitor 4ug with biotin",
                                      "serum 1/300 IgG competitor 4ug no biotin"
                                      )))
                  

IgG_plot <- all_data %>%
  ggplot( aes(dilutions, antigen,fill= value)) + 
  geom_tile(size=1) +
  scale_fill_gradient(low = "bisque", high = "darkorange3") +
  geom_text(aes(label=round(value,2))) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_equal() +
  facet_wrap(~ patient)
ggsave('intensities_plot.pdf',plot = IgG_plot, width = 15.2, height = 13.9, units = 'in', scale = 1.2)
