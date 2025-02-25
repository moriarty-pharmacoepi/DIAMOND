# Pharmacoepidemiology simulation study practices: A review of the literature  

This is the official source code repository of the review of simulation practices in the Journal Pharmacoepidemiology and Drug Safety.


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
```
@MISC{muddiman2025,
author =   {authors},
title =    {title},
}
```



## License

TBD
