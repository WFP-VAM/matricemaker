# matricemaker

This resource is a step a long the path to improve speed and accuracy in data processing in the Cadre Harmonise.  This script
[script](https://github.com/WFP-VAM/matricemaker/blob/main/makematrice.R) can produce a formatted excel version of the famous matrice intermediare [matrice intermediare](https://github.com/WFP-VAM/matricemaker/blob/main/Matrice_intermediaire.xlsx) given:
- a household survey data file (usually SPSS format) using standardized variable names (https://wfp-vam.github.io/RBD_FS_CH_guide_FR/)  
- variable names and categorization of contributing factors i.e - variable name = `q101a_chocs_subis_derniers6_mois` and it falls under `dangers/hazards section`

## Notes

- much more testing and work needed to make this  solid but  flexible enough - the hope is to turn a lot of things into functions and then later a package
- currently only works for french version of the matrice intermediare

## To try it out

- experienced R users can try to use this resource as is , probably by first looking at the [sample dataset](), [script](https://github.com/WFP-VAM/matricemaker/blob/main/makematrice.R) and [matrice intermediare](https://github.com/WFP-VAM/matricemaker/blob/main/Matrice_intermediaire.xlsx)
- currently only works for french version of the matrice intermediare
