# Pharmacoepidemiology simulation study practices: A review of the literature  

This is the official source code repository of the review of simulation practices in the Journal Pharmacoepidemiology and Drug Safety.

## Manuscript ##
The manuscript can be found [here.](https://onlinelibrary.wiley.com/doi/full/10.1002/pds.70329)

## Code ##
The code is archived on Zenodo 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15309637.svg)](https://doi.org/10.5281/zenodo.15309637)

## Protocol ##
The protocol of the study can be found here: [Protocol](reports/emulat_sim_protocol.pdf)

## Functionality ##
The following two scripts were used:

* search_and_download.R
* match_included.R

To replicate our results, use the following;
1. search_and_download.R should be called first which downloads the metadata for the simulation studies using easyPubMed and stores the data in an XML file for importing to Covidence. It also obtains the extracted data from [Tazare et al.](https://doi.org/10.1002/pds.5856) and creates another XML file for importing to Covidence.
2. Both XML files are then imported to Covidence for screening.
3. After screening, the metadata from the included articles is exported from Covidence as a CSV file
4. match_included.R should then be called which matches the included PMIDs from screening to the raw metadata files from our search and Tazare et al.
5. A file named included_final.csv then contains the included articles from which we extracted the data.

## Citation ##
If you use this code in your research, please cite it like this:

R.Muddiman, F. I. A.Battan, J.Tazare, et al., “A Methodological Review of Simulation Studies Published in Pharmacoepidemiology and Drug Safety,” Pharmacoepidemiology and Drug Safety35, no. 2 (2026): e70329, https://doi.org/10.1002/pds.70329.

```
@article{https://doi.org/10.1002/pds.70329,
author = {Muddiman, Ryan and Battan, Florencia Inés Aiello and Tazare, John and Schultze, Anna and Boland, Fiona and Perez, Teresa and Wei, Li and Walsh, Mary E. and Moriarty, Frank},
title = {A Methodological Review of Simulation Studies Published in Pharmacoepidemiology and Drug Safety},
journal = {Pharmacoepidemiology and Drug Safety},
volume = {35},
number = {2},
pages = {e70329},
keywords = {review of simulations, simulation, simulation studies},
doi = {https://doi.org/10.1002/pds.70329},
url = {https://onlinelibrary.wiley.com/doi/abs/10.1002/pds.70329},
eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1002/pds.70329},
note = {e70329 PDS-25-0337.R1},
abstract = {ABSTRACT Purpose Simulation studies are used in pharmacoepidemiology for evaluating statistical methods in a controlled setting, whereby a known data-generating mechanism allows evaluation of the performance of different approaches and assumptions. This study aimed to review simulation studies performed in pharmacoepidemiology. Methods We conducted a review of all papers published in the journal of Pharmacoepidemiology and Drug Safety (PDS) over the period 2017–2024. We extracted data on study characteristics and key simulation choices such as the type of data-generating mechanism used, inferential methods tested and simulation size. Results Among 42 simulation studies included, 34 (81\%) were informing comparative effectiveness/safety studies. Twenty-two studies (52\%) used simulation in the context of a clinical condition, and 36 (86\%) used Monte-Carlo simulation. Inputs not derived from empirical data alone (n = 22, 52\%) or in combination with real-world data sources (n = 19, 45\%) were most often used for data generation. The complexity of simulations was often relatively low: although 31 studies (74\%) generated data based on other covariates, time-dependent covariates (n = 3) and effects (n = 4) were rarely implemented. Bias was the most often used performance measure (n = 26, 62\%), although notably 18 studies (43\%) did not report uncertainty in the method. Conclusion Simulations contributed a relatively small number of articles (3.2\% of 1320) to PDS over 2017–2024. Greater focus on evaluating methods and inferential approaches, using simulation studies that are appropriately complex given clinical realities, may be beneficial to the pharmacoepidemiology field.},
year = {2026}
}


```



## License

MIT License

Copyright (c) [year] [fullname]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
