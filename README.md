# Interspecies cooperation in the farm-associated nasal microbiome (2026)

Code and data used to generate figures & data tables associated with the DOME2 (Interspecies cooperation in the farm-associated nasal microbiome) manuscript


Scripts used by C. Miller and S. Paruthiyil for analysis and visualization 

Version numbers are included for bash scripts. All were run in the High Throughput Computing Facility computing environment at the Center for Genome Sciences and WashU Medicine; installation time is not applicable. R code runs successfully on R versions 4.4.2 and later from Mac OS 14.5 and later and relevant packages were installed through R itself with installation times less than 2 minutes.


### Scripts: software versions used

run_dbcan (v5.1.2; CAZyme_annotation --mode prok)
fastp (v0.23.4)
SeqKit2 (v2.10.0; sample)
Unicycler (v0.5.1)
CheckM (v1.2.3; --reduced_tree)
Quast (v5.2.0)
FastANI (v1.34)
GTDB-Tk (v2.4.1, GTDB R220; classify_wf, de_novo_wf --skip_gtdb_refs)
ncbi-datasets (v18.3.0; summary genome accession --as-json-lines)
ncbi-dataformat (v18.3.0; tsv genome --fields organism-name,accession,assmstats-total-sequence-len)
SRA Toolkit (v3.0.0)
R package DADA2 (v1.34.0)
SILVA database (v138.1)
JupyterLab (v4.4.3)
pandas (v2.3.0)
NumPy (v2.2.6)
Python (v3.10.18)
seaborn (v0.13.2)
scikit-bio (v0.7.0)
scipy (v1.15.2)
R package ape (v5.8-1)
R package broom (v1.0.9)
R package cowplot (v1.2.0)
R package dplyr (v1.1.4)
R package ggdist (v3.3.3)
R package ggh4x (v0.3.1)
R package ggplot2 (v3.5.2)
R package ggpubr (0.6.1)
R package igraph (v2.1.4)
R package labdsv (v2.1-2)
R package lme4 (v1.1-37)
R package lsmeans (v2.30-2)
R package lubridate (v1.9.4)
R package purrr (v1.1.0)
R package readr (v2.1.5)
R package reshape2 (v1.4.4)
R package scales (v1.4.0)
R package shape (v1.4.6.1)
R package stringr (v1.5.2)
R package tidyr (v1.3.1)
R package tidyselect (v1.2.1)
R package vegan (v2.7-1)
R package decontam (v1.26.0, threshold = 0.5)
R package phyloseq (v1.50.0)
R package comparegroups (v4.9.1)
R package ggsci (v3.2.0)
R package glue (1.8.0)
R package viridislite (v.0.4.2)
R package viridis (v0.6.5)
R package svglite (v2.1.3)
R package DESeq2 (v1.48.1)
featureCounts(v2.0.2)
bowtie2(v2.4.2)
EggNOG(v2.1.12,webserver)
mzCloud
R packages from jrtools(https://github.com/jir88/jrtools)
R package GGtree(v3.16.3)
R package gcplyr (v1.11.0)






