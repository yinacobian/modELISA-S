# modELISA-S

![monalisadots!](monalisa-in-140dots.jpg)

## Use 

Screening for viral antibodies in saliva samples, using a modified ELISA with viral antigens


1. Run spli_tables2.sh 
2. Run plate_to_color2.R
3. Run Combine_Tables.R

Detailed instructions:

1) Make a folder for your experiment. Place the input files in the expetiment folder. Go to the experiment folder

2) Run split_tables2.sh

`bash ../split_tables2.sh 2022-02-18-SALIVA.txt `

3) Run plate_to_color2.R

`Rscript plate_to_color2.R`

Input:

1. Text file from OD reader in plate format

2. file-to-info : Information about the plate setup and the samples order

3. sample-to-info : Adittional information about the samples

4. neg-pos-concentrations : Standard curve concentrations


Output: 

1. Visualization of OD readings

2. Regression plos (pending)

3. Table with dilution at threshold values

4. Heatmap of dilution at threshold values

## Experimental setup

Modified ELISA to detect antibodies that bind to viral antigens. 

A) Experimental setup

B) Available viral antigens

![experiment!](experiment-image.png)

## Plates setup 

This modular design allows to test several antigens in a single plate.

A1:A4 Blank

A5:A8 Standard curve, replicate 1

A9:A12 Standard curve, replicate 2

B to H, 1:4 Replicate 1

B to H, 5:8 Replicate 2

B to H, 9:12 Replicate 3

![plates!](plate-setup.png)
