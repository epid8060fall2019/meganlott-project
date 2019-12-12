This is the repository for Megan Lott's Project.


# Pre-requisites

This is a template for a data analysis project using R (Version 3.6.1), Rmarkdown (and variants, e.g. bookdown), Github and a reference manager that can handle bibtex (such as [Jabref](http://www.jabref.org/) or [Zotero](https://www.zotero.org/)). It is also assumed that you have a word processor installed (e.g. MS Word or [LibreOffice](https://www.libreoffice.org/)). You need that software stack to make use of this template.

# Repository Structure

* All data goes into the subfolders inside the `data` folder.
* All code goes into the `code` folder or subfolders.
* All results (figures, tables, computed values) go into `results` folder or subfolders.
* All products (manuscripts, supplement, presentation slides, web apps, etc.) go into `products` subfolders.
* See the various `readme.md` files in those folders for some more information.


# Getting Started

1. Clone or fork the repository. 
2. Run the processing scipt in the `processing_code` subfolder under `code`. This code will produce the processed data.
3. Run the analysis scripts in the `analysis_code` subfolder under `code`. Run these scripts in the following order: `descriptive_statistics.R`, `univariate_analysis.R`, `bivariate_analysis.R`, `full_analysis.R`. 
4. Run the the `Project_Template.Rmd` file in the 'manuscript' subfolder. This file will generate the draft manuscript with text and figures. 
