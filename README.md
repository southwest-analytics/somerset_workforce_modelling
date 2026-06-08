# NHS Somerset Integrated Care Board Workforce Modelling Project

The **mm_modelling.R** file contains the R source code for the core functions 
for the Gaussian Copula approach to multimorbidity modelling of a population.

The folder **somerset_icb** contains the modelling for NHS Somerset ICB and
below shows the files structure within the project folder

- **somerset_icb** The root folder for the NHS Somerset ICB Workforce Project
  - **input** This folder contains the inputs used for the modelling
    - **qof_input.xlsx** The inputs for the QOF based modelling
    - **gbd_input.xlsx** The inputs for the Global Burden of Disease modelling
    - **gp-reg-pat-prac-quin-age.csv** Used for the comparison of population
  - **output** This folder contains the outputs of the modelling and the workforce planning tool
    - **gbd** This folder contains the Global Burden of Disease modelling outputs
      - **demand_modelling_summary.html** These are the slides showing the output of the Global Burden of Disease modelling 
      - **simulation_grouped_gbd.zip** This zipfile contains the middle level of detail of the modelling used for the workforce planning tool
      - **simulation_summary_gbd.csv** This csv contains the lowest level of detail of the modelling
      - **workforce_modelling_v3_gbd.xlsx** This is the workforce planning tool with inputs based on the Global Burden of Disease modelling
    - **qof** This folder contains the QOF modelling outputs
      - **demand_modelling_summary.html** These are the slides showing the output of the QOF modelling 
      - **simulation_grouped_qof.zip** This zipfile contains the middle level of detail of the modelling used for the workforce planning tool
      - **simulation_summary_qof.csv** This csv contains the lowest level of detail of the modelling
      - **workforce_modelling_v3_qof.xlsx** This is the workforce planning tool with inputs based on the QOF modelling
  - **mm_modelling_somerset.R** This is the source code of the NHS Somerset ICB Demand Modelling
  - **demand_modelling.qmd** This is the source code used to create the output slide decks
  
  



Somerset ICB Workforce Modelling Project
