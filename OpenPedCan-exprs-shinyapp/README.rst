.. |date| date::

*************************
OpenPedCan-exprs-shinyapp
*************************

:authors: Komal S Rathi
:contact: rathik@email.chop.edu
:organization: D3B, CHOP
:status: This is "work in progress"
:date: |date|

Introduction
============

Mock app built using R shiny to create ``tumor vs normal`` and ``pan-cancer`` plots to visualize TPM expression.

Structure
=========

.. code-block:: bash

    .
	├── R
	│   ├── pubTheme.R
	│   ├── pan_cancer_plot.R
	│   ├── tumor_vs_normal_plot.R
	│   └── viewDataTable.R
	├── README.rst
	├── data
	│   ├── efo-mondo-map.tsv 
	│   ├── ensg-hugo-rmtl-mapping.tsv 
	│   ├── gene-expression-rsem-tpm-collapsed.rds 
	│   └── histologies.tsv
	├── server.R
	├── ui.R
	└── www


Functions:
==========

1. **R/viewDataTable.R**: ``DT::datatable`` with various add-on, buttons etc
2. **R/pubTheme.R**: ggplot2 publication quality themes
3. **R/pan_cancer_plot.R**: function for pan-cancer plots
4. **R/tumor_vs_normal_plot.R**: function for tumor vs normal plots

Input:
======

Input files used are:

1. Expression matrix: ``data/gene-expression-rsem-tpm-collapsed.rds``
2. Histologies file: ``data/histologies.tsv``
3. EFO MONDO mapping file: ``data/efo-mondo-map.tsv``
4. Ensembl ID and RMTL mapping file: ``data/ensg-hugo-rmtl-mapping.tsv``
   
The input files can be downloaded by cloning this repo and running the download script: https://github.com/PediatricOpenTargets/OpenPedCan-analysis

Output:
=======

Two types of output created using this app: one is dynamic (which can be seen on the app as ``plotly`` plots and ``DT`` tables) and the corresponding static version is written to the ``www/`` folder as ``.png`` and ``.tsv`` files.

Note: ``data/`` (input directory) and ``www/`` (output directory) have been added to .gitignore.
