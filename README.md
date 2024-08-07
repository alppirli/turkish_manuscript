# Project

## Organization of files

* conllu_boun: Directory containing the files in the corpus directory as parsed by UDPipe 2. Includes only placeholder files so as to not infringe on copyright laws. The parsed files may be provided upon request under the condition that they are used only for academic use.
* conllu_generate.sh: Shell script that uses UDPipe 2 for tokenization, lemmatization, and POS-tagging. Run before running project.R. Requires [udpipe2_client.py](https://github.com/ufal/udpipe/blob/udpipe-2/udpipe2_client.py).
* corpus: Directory containing raw text files for the 94 novels. Includes only placeholder files so as to not infringe on copyright laws. The raw text files may be provided upon request under the condition that they are used only for academic use.
* project.R: The main project file. Requires the R packages tools, udpipe, stylo, and ggplot2.
* Various CSV files: These files demonstrate the performance of each classifier and each feature type in the LOOCV tests conducted in this study, showing the accuracy, precision, recall, and F1 score for each frequency stratum. The filenames point to what classifier and what feature type is being used. The classifiers are:
1. cosine: Cosine Delta (Eder's method)
2. cosine_c: Cosine Delta (Burrows' method) 
3. delta: Burrows' Delta (Eder's method)
4. delta_c: Burrows' Delta (Burrows' method)
5. nsc: Nearest Shrunken Centroid
6. svm: Support Vector Machines
* and the feature types are:
1. char_3g: Character 3-grams
2. lemmas: Lemmatized words
3. lemmas_EG: Lemmas + POS-tags
4. pos_full: POS-tags
5. pos_full_2: POS-tag 2-grams
6. tokens: Tokenized words