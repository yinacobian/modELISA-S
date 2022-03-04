library(tidyverse)
library(readr)


sample_to_info <- read_csv("sample-to-info.csv")
tableFile <- "file-to-info.csv"
if(! file.exists(tableFile)) stop('Error -- tableFile file not found.')
file_table <- read.csv(tableFile,stringsAsFactors=FALSE)
j_table <- left_join(file_table,sample_to_info,by =c("row_B_sample" = "SAMPLE_ID"))

kk <- apply(j_table,1, function(x){
  plate <- as.numeric(x["Plate_in_file"])
  filename <- x["File_name"] 
  tablepath <- paste0("table_parts/",sprintf("%02d",plate),"_",tools::file_path_sans_ext(filename),".tab")
  main_t <- paste0(x["row_B_sample"],' group: ',x["GROUP"])
  if(! file.exists(tablepath)) stop(paste0('Error -- tableFile ' , tablepath , ' file not found.'))
  raw_table <- read.table(tablepath,header = TRUE)
  rownames(raw_table) <- c("A:standard","B:HIV-p24","C:Brisavirus","D:Vientovirus",
                 "E:Coronavirus 229E","F:Coronavirus HKU1","G:Norovirus","H:GFP")
  colnames(raw_table) <- c("R1-D1","R1-D2","R1-D3","R1-D4","R2-D1","R2-D2","R2-D3","R2-D4","R3-D1","R3-D2","R3-D3","R3-D4")
  raw_table %>% 
    rownames_to_column(var="antigen") %>%
    pivot_longer(`R1-D1`:`R3-D4`,names_to = 'dilutions') %>%
    mutate(patient=main_t)
})

all_data <- Reduce(rbind,kk) %>%
  mutate(antigen=factor(antigen,rev(c("A:standard","B:HIV-p24","C:Brisavirus","D:Vientovirus",
                                      "E:Coronavirus 229E","F:Coronavirus HKU1","G:Norovirus","H:GFP"))))


IgG_plot_RVpositive <- all_data %>%
  filter(grepl('IgG',patient)) %>%
  filter(grepl('RV-POSITIVE',patient)) %>%
  ggplot( aes(dilutions, antigen,fill= value)) + 
  geom_tile(size=1) +
  scale_fill_gradient(low = "bisque", high = "darkorange3") +
  geom_text(aes(label=round(value,2))) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_equal() +
  facet_wrap(~ patient)
ggsave('IgG_plot_RVpositive.pdf',plot = IgG_plot_RVpositive, width = 15.2, height = 13.9, units = 'in', scale = 1.5)

IgG_plot_RVnegative <- all_data %>%
  filter(grepl('IgG',patient)) %>%
  filter(grepl('RV-NEGATIVE',patient)) %>%
  ggplot( aes(dilutions, antigen,fill= value)) + 
  geom_tile(size=1) +
  scale_fill_gradient(low = "bisque", high = "darkorange3") +
  geom_text(aes(label=round(value,2))) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_equal() +
  facet_wrap(~ patient)
ggsave('IgG_plot_RVnegative.pdf',plot = IgG_plot_RVnegative, width = 15.2, height = 13.9, units = 'in', scale = 1.5)

IgA_plot_RVpositive <- all_data %>%
  filter(grepl('IgA',patient)) %>%
  filter(grepl('RV-POSITIVE',patient)) %>%
  ggplot( aes(dilutions, antigen,fill= value)) + 
  geom_tile(size=1) +
  scale_fill_gradient(low = "bisque", high = "darkorange3") +
  geom_text(aes(label=round(value,2))) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_equal() +
  facet_wrap(~ patient)
ggsave('IgA_plot_RVpositive.pdf',plot = IgA_plot_RVpositive, width = 15.2, height = 13.9, units = 'in', scale = 1.5)

IgA_plot_RVnegative <- all_data %>%
  filter(grepl('IgA',patient)) %>%
  filter(grepl('RV-NEGATIVE',patient)) %>%
  ggplot( aes(dilutions, antigen,fill= value)) + 
  geom_tile(size=1) +
  scale_fill_gradient(low = "bisque", high = "darkorange3") +
  geom_text(aes(label=round(value,2))) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_equal() +
  facet_wrap(~ patient)
ggsave('IgA_plot_RVnegative.pdf',plot = IgA_plot_RVnegative, width = 15.2, height = 13.9, units = 'in', scale = 1.5)



