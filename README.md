# POS-tags, lemmatization, and feature frequencies: stylometric analyses in Turkish using stylo

[![paper](https://img.shields.io/badge/paper-click%20here-green)](https://rdcu.be/d7ix5)
[![paper](https://img.shields.io/badge/DOI-10.1007/s42803--024--00094--1-blue)](https://doi.org/10.1007/s42803-024-00094-1)

## Organization of files

* conllu_generate.sh: Shell script that uses UDPipe 2 for tokenization, lemmatization, and POS-tagging. Run before running project.R. Requires [udpipe2_client.py](https://github.com/ufal/udpipe/blob/udpipe-2/udpipe2_client.py).
    * Before running, make sure you have a directory named "corpus" with raw text files in Turkish. The raw text files of the 94 Turkish novels used in the study have not been included in this repository so as to not infringe on copyright laws. These raw text files may be provided upon request under the condition that they are used only for academic use.
* project.R: The main project file. Requires the R packages tools, udpipe, stylo, and ggplot2.
* csvs: Directory containing various CSV files. These files demonstrate the performance of each classifier and each feature type in the LOOCV tests conducted in this study, showing the accuracy, precision, recall, and F1 score for each frequency stratum. The filenames point to what classifier and what feature type is being used.
    * The classifiers are:
        * cosine: Cosine Delta (Eder's method)
        * cosine_c: Cosine Delta (Burrows' method) 
        * delta: Burrows' Delta (Eder's method)
        * delta_c: Burrows' Delta (Burrows' method)
        * nsc: Nearest Shrunken Centroid
        * svm: Support Vector Machines
    * and the feature types are:
        * char_3g: Character 3-grams
        * lemmas: Lemmatized words
        * lemmas_EG: Lemmas + POS-tags
        * pos_full: POS-tags
        * pos_full_2: POS-tag 2-grams
        * tokens: Tokenized words
