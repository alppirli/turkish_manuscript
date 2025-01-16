library(tools)
library(udpipe)
library(stylo)
library(ggplot2)

# cosine delta weighting fix, part 1: code adapted from https://rdrr.io/cran/stylo/src/R/dist.wurzburg.R
# stylo
# dist.wurzburg.R
# (C) 2025 by Alp Eren Pirli (alppirli@iu.edu)
# (C) 2024 by Maciej Eder (maciejeder@gmail.com), Jan Rybicki, Mike Kestemont, Steffen Pielström
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
#   This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
#   You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

dist.wurzburg.fixed = function(x, scale = TRUE){
  
  # test if the input dataset is acceptable
  if(is.matrix(x) == FALSE & is.data.frame(x) == FALSE) {
    stop("cannot apply a distance measure: wrong data format!")
  }
  # then, test whether the number of rows and cols is >1
  if(length(x[1,]) < 2 | length(x[,1]) < 2) {
    stop("at least 2 cols and 2 rows are needed to compute a distance!")
  }
  
  # !! fixed part -- only had x = scale(x) before without an if statement
  # you should never have scale == TRUE to begin with but...
  if(scale == TRUE) {
    x = scale(x)
     # checking for all-zeros columns: they spoil the final results!
    x = x[ , attr(x ,"scaled:scale") != 0]
    # ^ no need for this, the function below does the exact same thing
  }
  
  # btw: this is needed because the R dist() function used for other distance measures removes all missing values and infinites from the rows they occur
  # that being said, this is not necessary if one properly extracts the vector of most frequent features using ONLY the training set
  # if you extract this vector using both sets then that's not only data leakage but also introduces NaN/Inf values
  x <- x[, apply(x, 2, function(cc) !all(cc %in% c(NaN, NA, Inf)))]
  
  # to get Centered Cosine dist (=Pearson Correlation Coeff.), one needs 
  # to normalize the feature vectors by subtracting the vector means
  # x = t( t(x) - colMeans(x) )
  
  # this computes cosine dissimilarity; to have similarity, 1- applies
  y = 1 - as.dist( x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))) ) 
  # alternative way of approaching it:
  # crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
  
  return(y)
}

# cosine delta weighting fix, part 2: code adapted from https://rdrr.io/cran/stylo/src/R/perform.delta.R
# stylo
# perform.delta.R
# (C) 2025 by Alp Eren Pirli (alppirli@iu.edu)
# (C) 2024 by Maciej Eder (maciejeder@gmail.com), Jan Rybicki, Mike Kestemont, Steffen Pielström
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
#   This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
#   You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
perform.delta.fixed = function(training.set, 
                         test.set,
                         classes.training.set = NULL,
                         classes.test.set = NULL,
                         distance = "delta",
                         no.of.candidates = 3,
                         z.scores.both.sets = TRUE) {

  # first, sanitizing the type of input data
  if(length(dim(training.set)) != 2) {
    stop("train set error: a 2-dimensional table (matrix) is required")
  }
  # if a vector (rather than a matrix) was used as a test set, a fake row
  # will be added; actually, this will be a duplicate of the vector
  if(is.vector(test.set) == TRUE) {
    test.set = rbind(test.set, test.set)
    rownames(test.set) = c("unknown", "unknown-copy")
    # additionally, duplicating ID of the test classes (if specified)
    if(length(classes.test.set) == 1) {
      classes.test.set = c(classes.test.set, "unknown-copy")
    }
  }
  
  
  # checking if the two sets are of the same size
  if(length(test.set[1,]) != length(training.set[1,]) ) {
    stop("training set and test set must have the same number of variables!")
  }
  
  
  # assigning classes, if not specified
  if(length(classes.training.set) != length(rownames(training.set))) {
    classes.training.set = c(gsub("_.*", "", rownames(training.set)))
  }
  
  if(length(classes.test.set) != length(rownames(test.set))) {
    classes.test.set = c(gsub("_.*", "", rownames(test.set)))
  }
  
  #
  
  
  
  
  # calculating z-scores either of training set, or of both sets
  if(z.scores.both.sets == FALSE) {
    # mean and standard dev. for each word (in the training set)
    training.set.mean = c(sapply(as.data.frame(training.set), mean))
    training.set.sd = c(sapply(as.data.frame(training.set), sd))
    # z-scores for training.set
    zscores.training.set = scale(training.set)
    rownames(zscores.training.set) = rownames(training.set)
    # z-scores for test.set, using means and st.devs of the training set
    zscores.test.set = 
      scale(test.set, center=training.set.mean, scale=training.set.sd)
    rownames(zscores.test.set) = rownames(test.set)
    # the two tables with calculated z-scores should be put together
    zscores.table.both.sets = rbind(zscores.training.set, zscores.test.set)
  } else {
    # the z-scores can be calculated on both sets as alternatively  
    zscores.table.both.sets = scale(rbind(training.set, test.set))
    # a dirty trick to get the values and nothing else
    zscores.table.both.sets = zscores.table.both.sets[,]
  }
  
  
  # some distances require just freqs
  input.freq.table = rbind(training.set, test.set)
  
  
  
  
  
  supported.measures = c("dist.euclidean", "dist.manhattan", "dist.canberra",
                         "dist.delta", "dist.eder", "dist.argamon",
                         "dist.simple", "dist.cosine", "dist.wurzburg",
                         "dist.entropy", "dist.minmax")
  
  
  
  # if the requested distance name is confusing, stop
  if(length(grep(distance, supported.measures)) > 1 ) {
    stop("Ambiguous distance method: which one did you want to use, really?")
    
    # if the requested distance name was not found invoke a custom plugin
  } else if(length(grep(distance, supported.measures)) == 0 ){
    
    # first, check if a requested custom function exists 
    if(is.function(get(distance)) == TRUE) {
      # if OK, then use the value of the variable 'distance.measure' to invoke 
      # the function of the same name, with x as its argument
      distance.table = do.call(distance, list(x = input.freq.table))
      # check if the invoked function did produce a distance
      if(class(distance.table) != "dist") {
        # say something nasty here, if it didn't:
        stop("it wasn't a real distance measure function applied, was it?")
      }
    }
    
    # when the chosen distance measure is among the supported ones, use it
  } else {
    
    # extract the long name of the distance (the "official" name) 
    distance = supported.measures[grep(distance, supported.measures)]
    # then check if this is one of standard methods supported by dist()
    if(distance %in% c("dist.manhattan", "dist.euclidean", "dist.canberra")) {
      # get rid of the "dist." in the distance name
      distance = gsub("dist.", "", distance)
      # apply a standard distance, using the generic dist() function
      distance.table = as.matrix(dist(input.freq.table, method = distance))
      # then, check for the non-standard methods but still supported by 'stylo':
    } else if(distance %in% c("dist.simple", "dist.cosine", "dist.entropy", "dist.minmax")) {
      
      # invoke one of the distance measures functions from Stylo    
      distance.table = do.call(distance, list(x = input.freq.table))
      
    } else if(distance == "dist.wurzburg") { # !! fixed part
      
      # invoke one of the distance measures functions from Stylo
      distance.table = do.call("dist.wurzburg.fixed", list(x = zscores.table.both.sets, scale = FALSE))
      
    } else { 
      # invoke one of the distances supported by 'stylo'; this is slightly
      # different from the custom functions invoked above, since it uses
      # another argument: z-scores can be calculated outside of the function
      distance.table = do.call(distance, list(x = zscores.table.both.sets, scale = FALSE))
    }
    
  }
  
  # convert the table to the format of matrix
  distance.table = as.matrix(distance.table)
  
  
  
  
  # selecting an area of the distance table containing test samples (rows),
  # contrasted against training samples (columns)
  no.of.candid = length(training.set[,1])
  no.of.possib = length(test.set[,1])
  selected.dist = 
    as.matrix(distance.table[no.of.candid+1:no.of.possib,1:no.of.candid])
  # assigning class ID to train samples
  colnames(selected.dist) = classes.training.set
  #
  
  
  
  if(no.of.candidates > length(classes.training.set)) {
    no.of.candidates = length(classes.training.set)
  }
  
  
  # starting final variables
  classification.results = c()
  classification.scores = c()
  classification.rankings = c()
  
  for(h in 1:length(selected.dist[,1])) {
    ranked.c = order(selected.dist[h,])[1:no.of.candidates]
    current.sample = classes.training.set[ranked.c[1]]
    classification.results = c(classification.results, current.sample)
    #
    current.ranking = classes.training.set[ranked.c]
    current.scores = selected.dist[h,ranked.c]
    classification.scores = rbind(classification.scores, current.scores)
    classification.rankings = rbind(classification.rankings, current.ranking)
  }
  
  
  
  
  names(classification.results) = rownames(test.set)
  rownames(classification.rankings) = rownames(test.set)
  rownames(classification.scores) = rownames(test.set)
  colnames(classification.rankings) = 1:no.of.candidates
  colnames(classification.scores) = 1:no.of.candidates
  
  
  
  # preparing a confusion table
  predicted_classes = classification.results
  expected_classes = classes.test.set
  
  classes_all = sort(unique(as.character(c(expected_classes, classes.training.set))))
  predicted = factor(as.character(predicted_classes), levels = classes_all)
  expected  = factor(as.character(expected_classes), levels = classes_all)
  confusion_matrix = table(expected, predicted)
  
  
  # shorten the names of the variables
  y = classification.results
  ranking = classification.rankings
  scores = classification.scores
  distance_table = selected.dist
  # predicted = predicted_classes
  # expected = expected_classes
  # misclassified = cv.misclassifications
  
  attr(y, "description") = "classification results in a compact form"
  # attr(misclassified, "description") = "misclassified samples [still not working properly]"
  attr(predicted, "description") = "a vector of classes predicted by the classifier"
  attr(expected, "description") = "ground truth, or a vector of expected classes"
  attr(ranking, "description") = "predicted classes with their runner-ups"
  attr(scores, "description") = "Delta scores, ordered according to candidates"
  attr(distance_table, "description") = "raw distance table"
  attr(confusion_matrix, "description") = "confusion matrix for all cv folds"
  
  
  results = list()
  results$y = y
  # results$misclassified = misclassified
  results$predicted = predicted
  results$expected = expected
  results$ranking = ranking
  results$scores = scores
  results$distance_table = distance_table
  results$confusion_matrix = confusion_matrix
  
  
  # adding some information about the current function call
  # to the final list of results
  results$call = match.call()
  results$name = call("perform.delta")
  
  class(results) = "stylo.results"
  
  return(results)
  
}
# end of modified GPLv3 licensed code

F_labels <- c("lemmas" = "Lemmas", "lemmas_EG" = "Lemmas + POS-tags", "tokens" = "Tokens",
              "char_3g" = "Character 3-grams", "pos_full" ="POS-tags", "pos_full_2" = "POS-tag 2-grams")

files <- list.files("conllu_boun/", full.names = TRUE) # to see how these files were obtained, refer to conllu_generate.sh

features <- c("tokens", "lemmas", "pos_full", "lemmas_EG", "char_3g")
for (feature in features) {
  assign(feature, list())
}


id_checker_token <- function(vect) {
  result <- c()
  hyphen_check <- FALSE
  for (i in seq_len(length(vect))) {
    id <- vect[i]
    if (grepl("-", id, fixed = TRUE)) {
        hyphen_check <- TRUE
        limit <- as.integer(strsplit(id, "-", fixed = TRUE)[[1]][2])
    } else if (hyphen_check) {
      current <- as.integer(id)
      if (current <= limit) {
        result <- c(result, i)
      } else {
        hyphen_check <- FALSE
      }
    }
  }
  return(result)
}

id_checker_lemma <- function(vect) {
  result <- c()
  hyphen_check <- FALSE
  for (i in seq_len(length(vect))) {
    id <- vect[i]
    if (grepl("-", id, fixed = TRUE)) {
      hyphen_check <- TRUE
      lower_limit <- as.integer(strsplit(id, "-", fixed = TRUE)[[1]][1])
      limit <- as.integer(strsplit(id, "-", fixed = TRUE)[[1]][2])
      result <- c(result, i)
    } else if (hyphen_check) {
      current <- as.integer(id)
      if (current != lower_limit) { # keep the lexical lemma in
        if (current <= limit) {
          result <- c(result, i)
        } else {
          hyphen_check <- FALSE
        }
      }
    }
  }
  return(result)
}
    
for (file in files) {
  filename <- file_path_sans_ext(basename(file))
  print(filename)
  conllu_data <- udpipe_read_conllu(file)
  exclude_token <- id_checker_token(conllu_data$token_id)
  exclude_lemma <- id_checker_lemma(conllu_data$token_id)
  exclude_postag <- grep("-", conllu_data$token_id, fixed = TRUE)
  tokens[[filename]] <- tolower(conllu_data$token[-exclude_token])
  # ^ here, atomizations of words go because we are interested in generic text tokenization
  # ^ tokens are made lowercase in imitation of Eder&Gorski
  lemmas[[filename]] <- conllu_data$lemma
  pos_full[[filename]] <- paste(conllu_data$upos, conllu_data$feats, sep = "|")
  lemmas_EG[[filename]] <- paste(conllu_data$lemma, pos_full[[filename]], sep = "|")[-exclude_lemma]
  # ^ the lemmatizer Eder and Gorski use outputs this sort of feature -- lemma&POS-tag combinations
  lemmas[[filename]] <- lemmas[[filename]][-exclude_lemma]
  # ^ here, aux, part, etc. go because we are interested in lexical choice
  pos_full[[filename]] <- pos_full[[filename]][-exclude_postag]
  # ^ here, aux, part, etc. can stay because we are interested in grammar/syntax and not lexical choice
}

files2 <- list.files("corpus/", full.names = TRUE) # raw texts for character 3-grams
for (file in files2) {
  filename <- file_path_sans_ext(basename(file))
  char_3g[[filename]] <- readLines(file)
  char_3g[[filename]] <- tolower(char_3g[[filename]]) # since tokens are lowercase
  char_3g[[filename]] <- txt.to.features(char_3g[[filename]], "c", 3)
  char_3g[[filename]] <- char_3g[[filename]][!grepl("^\\s*$", char_3g[[filename]])]
  # ^more necessary preprocessing...
}

pos_full_2 <- txt.to.features(pos_full, "w", 2)
fig_features <- c("lemmas", "tokens", "lemmas_EG", "char_3g", "pos_full", "pos_full_2")

increments <- seq(50, 2000, 50)
methods <- c("delta", "cosine", "svm", "nsc", "delta_c", "cosine_c")
all_works <- names(tokens)
author_table <- table(c(gsub("_.*", "", all_works)))
authors <- names(author_table)
  
  # confusion matrix initialization
init_matrix <- matrix(0, nrow = length(authors), ncol = length(authors))
colnames(init_matrix) <- authors
rownames(init_matrix) <- authors
  
LOOCV <- list()
for (feature in fig_features) {
  for (method in methods) {
    for (increment in increments) {
      LOOCV[[feature]][[method]][[as.character(increment)]] <- init_matrix
    }
  }
}

# LOOCV start
for (feature in fig_features) {
  for (work in all_works) {
    # if (length(freq_table) > 5000) {
    #   freq_table <- freq_table[1:5000]
    # }
    print(paste(feature, work))
    uncombined <- get(feature)
    uncombined_wo <- uncombined[names(uncombined) != work]
    combined <- list()
    combined[[work]] <- uncombined[[work]]
    which_author <- strsplit(work,"_", fixed = TRUE)[[1]][1]
    for (author in authors) {
      comb_name <- paste0(author, "_Combined")
      combined[[comb_name]] <- unlist(uncombined_wo[startsWith(names(uncombined_wo), author)],
                                      use.names = FALSE)
    }
    freq_table <- sort(table(unlist(uncombined_wo, use.names = FALSE)), decreasing = TRUE)
    u_freqs <- make.table.of.frequencies(uncombined, names(freq_table), absent.sensitive = FALSE)
    u_training_freqs <- u_freqs[rownames(u_freqs) != work, ]
    u_test_freqs <- u_freqs[rownames(u_freqs) == work, ]
    c_freqs <- make.table.of.frequencies(combined, names(freq_table), absent.sensitive = FALSE)
    c_training_freqs <- c_freqs[rownames(c_freqs) != work, ]
    c_test_freqs <- c_freqs[rownames(c_freqs) == work, ]
    for (method in methods) {
      for (increment in increments) {
        print(paste(feature, work, method, increment))
        if (increment <= length(colnames(c_freqs))) { # could have also said colnames(u_freqs)--this is just
          # so that pos_full doesn't go beyond 1000 (only 1015 unique features--would cause an error)
          u_training_freqs_used <- u_training_freqs[,1:increment]
          u_test_freqs_used <- u_test_freqs[1:increment]
          c_training_freqs_used <- c_training_freqs[,1:increment]
          c_test_freqs_used <- c_test_freqs[1:increment]
          if (method == "delta") {
            result <- perform.delta.fixed(
                                training.set = u_training_freqs_used,
                                test.set = u_test_freqs_used,
                                distance = "delta",
                                classes.test.set = which_author,
                                z.scores.both.sets = FALSE)
          } else if (method == "cosine") {
            result <- perform.delta.fixed(
                                training.set = u_training_freqs_used,
                                test.set = u_test_freqs_used,
                                distance = "wurzburg",
                                classes.test.set = which_author,
                                z.scores.both.sets = FALSE)
          } else if (method == "delta_c") {
            result <- perform.delta.fixed(
              training.set = c_training_freqs_used,
              test.set = c_test_freqs_used,
              distance = "delta",
              classes.test.set = which_author,
              z.scores.both.sets = FALSE)
          } else if (method == "cosine_c") {
            result <- perform.delta.fixed(
              training.set = c_training_freqs_used,
              test.set = c_test_freqs_used,
              distance = "wurzburg",
              classes.test.set = which_author,
              z.scores.both.sets = FALSE)
          } else if (method == "svm") {
            result <- perform.svm(
              training.set = u_training_freqs_used,
              test.set = u_test_freqs_used,
              classes.test.set = which_author)
          } else if (method == "nsc") {
            result <- perform.nsc(
              training.set = u_training_freqs_used,
              test.set = u_test_freqs_used,
              classes.test.set = which_author)
          }
          exp <- as.character(result$expected[1])
          pre <- as.character(result$predicted[1])
          LOOCV[[feature]][[method]][[as.character(increment)]][[exp,pre]] <-
            LOOCV[[feature]][[method]][[as.character(increment)]][[exp,pre]] + 1
        }
      }
    }
  }
}
        
LOOCV_DFs <- list()
for (f in names(LOOCV)) {
  for (m in names(LOOCV[[f]])) {
    for (MFW in names(LOOCV[[f]][[m]])){
      cmatrix <- LOOCV[[f]][[m]][[MFW]]
      total <- sum(cmatrix)
      expected <- apply(cmatrix, 1, sum) # TP+FN
      predicted <- apply(cmatrix, 2, sum) # TP+FP
      correct <- diag(cmatrix) # TP
      accuracy <- sum(correct)/total
      precision <- correct/predicted
      precision[is.nan(precision)] <- 0
      recall <- correct/expected
      recall[is.nan(recall)] <- 0
      F1 <- 2*precision*recall/(precision+recall)
      F1[is.nan(F1)] <- 0
      precision <- mean(precision) # macro-averages
      recall <- mean(recall)
      F1 <- mean(F1)
      MFW <- as.integer(MFW)
      DF <- data.frame(MFW, accuracy, precision, recall, F1)
      if (!is.nan(accuracy)) { # this is to stop pos-tags which end at 1000. no other acc. value is nan
        LOOCV_DFs[[f]][[m]] <- rbind(LOOCV_DFs[[f]][[m]], DF)
      }
    }
  }
}      
      
# write CSVs
for (f in names(LOOCV_DFs)) {
  for (m in names(LOOCV_DFs[[f]])) {
    name <- paste("LOOCV", m, f, ".csv", sep = "-")
    write.csv(LOOCV_DFs[[f]][[m]], name)
  }
}

# --- figures ---

plot_data <- data.frame()
for (f in names(LOOCV_DFs)) {
  for (m in names(LOOCV_DFs[[f]])) {
    plot_data <- rbind(plot_data, cbind("classifier"=m, "feature"=f, LOOCV_DFs[[f]][[m]]))
  }
}

plot_data_1 <- plot_data[!plot_data$classifier %in% c("delta_c", "cosine_c"),]

# figure 1: different classifiers, color coded
class_colors <- c("cosine"="#c77cff", "delta"="#7cae00", "nsc"="#f8766d", "svm"="#00bfc4", "others" = "#d0d0d080")
shape_values <- c("cosine"=15, "delta"=17, "nsc"=16, "svm"=18, "others"=NA)
line_values <- c("cosine"="solid", "delta"="longdash", "nsc"="twodash", "svm"="dotdash")

ggplot(plot_data_1,
       aes(x = MFW, y = F1, color = classifier, group = interaction(feature, classifier))) +
  geom_line(aes(linetype = classifier), linewidth = 0.5) +  # Added linetype aesthetic here
  geom_point(data = subset(plot_data_1, MFW %% 200 == 0), 
             aes(shape = classifier), size = 2) +
  scale_shape_manual(name="", values = shape_values) +
  labs(x = "Number of features tested",
       # caption = "Figure 1",
       y = "F1 score") +
  theme_minimal() +
  scale_color_manual(name="", values = class_colors) +
  scale_linetype_manual(name="", values = line_values) +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2)
  ) +
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.1), limits = c(0.562, 1)) +
  scale_x_continuous(breaks = seq(0, 2000, by = 500))
# ggsave("Fig1.tiff", units="px", width = 3600, height = 2400, dpi=600)
ggsave("Fig1.eps", device = cairo_ps, dpi=1200, height = 4.4, width = 6.6)

# figures 2-7: features, other features greyed out
F_labels <- c("lemmas" = "Lemmas", "lemmas_EG" = "Lemmas +\nPOS-tags", "tokens" = "Tokens",
              "char_3g" = "Character 3-grams", "pos_full" ="POS-tags", "pos_full_2" = "POS-tag 2-grams")
fig_n <- 2
for (feat in fig_features) {
  ggplot(plot_data_1,
         aes(x = MFW, y = F1, group = interaction(feature, classifier))) +
    geom_line(aes(color = ifelse(feature == feat, classifier, "others"), linetype = classifier), linewidth = 0.5) +
    geom_point(data = subset(plot_data_1, feature == feat & MFW %% 200 == 0 & classifier != "others"), 
               aes(color = classifier, shape = classifier), size = 2) +
    labs(x = "Number of features tested",
	     # caption = paste("Figure", fig_n),
         y = "F1 score",
         title = F_labels[feat]) +
    theme_minimal() +
    scale_color_manual(name = "", values = class_colors, breaks = sort(methods)) +
    scale_shape_manual(name = "", values = shape_values, 
                       na.translate = FALSE) +
    scale_linetype_manual(name="", values = line_values) +
    theme(
      legend.position = c(.95, .05),
      legend.justification = c("right", "bottom"),
      legend.box.just = "right",
      legend.margin = margin(2, 2, 2, 2),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_y_continuous(breaks = seq(0.5, 1, by = 0.1), limits = c(0.562, 1)) +
    scale_x_continuous(breaks = seq(0, 2000, by = 500))
  # ggsave(paste0("Fig",fig_n,".tiff"), units = "px", width = 3600, height = 2400, dpi=600)
  ggsave(paste0("Fig",fig_n,".eps"), device = cairo_ps, dpi=1200, height = 4.4, width = 6.6)
  fig_n <- fig_n + 1
} #fig_n = 8

# ------------------

class_colors <- c("cosine_c"="#c77cff", "delta_c"="#7cae00", "cosine"="#f8766d", "delta"="#00bfc4", "others" = "#d0d0d080")
class_labels <- c("cosine_c"="cosine (Burrows' method)", "delta_c"="delta (Burrows' method)",
                  "cosine"="cosine (Eder's method)", "delta"="delta (Eder's method)")
shape_values <- c("cosine_c"=15, "delta_c"=17, "cosine"=16, "delta"=18, "others"=NA)
line_values <- c("cosine_c"="solid", "delta_c"="longdash", "cosine"="twodash", "delta"="dotdash")
# figure 8: different classifiers, color coded
plot_data_2 <- plot_data[plot_data$classifier %in% c("cosine_c", "delta_c"),]

ggplot(plot_data_2,
       aes(x = MFW, y = F1, color = classifier, group = interaction(feature, classifier))) +
  geom_line(aes(linetype = classifier), linewidth = 0.5) +
  geom_point(aes(shape = classifier), data = plot_data_2[plot_data_2$MFW %% 200 == 0, ],
             size = 2) +
  labs(x = "number of features tested",
       # caption = paste("Figure", fig_n),
       y = "F1 score") +
  theme_minimal() +
  scale_color_manual(name="", values = class_colors, labels = class_labels) +
  scale_shape_manual(name="", values = shape_values, labels = class_labels) +
  scale_linetype_manual(name="", values = line_values, labels = class_labels) +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2)
  ) +
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.1), limits = c(0.562, 1)) +
  scale_x_continuous(breaks = seq(0, 2000, by = 500))
# ggsave(paste0("Fig",fig_n,".tiff"), units = "px", width = 3600, height = 2400, dpi=600)
ggsave(paste0("Fig",fig_n,".eps"), device = cairo_ps, dpi=1200, height = 4.4, width = 6.6)
fig_n <- fig_n + 1

# figure 9-14, alt.
fig_features <- c("lemmas", "char_3g", "tokens", "lemmas_EG", "pos_full", "pos_full_2")
fig_n <- 9
plot_data_3 <- plot_data[!plot_data$classifier %in% c("svm", "nsc"),]
for (feat in fig_features) {
  ggplot(plot_data_3,
         aes(x = MFW, y = F1, group = interaction(feature, classifier))) +
    geom_line(aes(color = ifelse(feature == feat, classifier, "others"), linetype = classifier), linewidth = 0.5) +
    geom_point(data = subset(plot_data_3, feature == feat & MFW %% 200 == 0 & classifier != "others"), 
               aes(color = classifier, shape = classifier), size = 2) +
    labs(x = "number of features tested",
	     # caption = paste("Figure", fig_n),
         y = "F1 score",
         title = F_labels[feat]) +
    theme_minimal() +
    scale_color_manual(name = "", values = class_colors, breaks = names(class_labels),
                       labels = class_labels) +
    scale_shape_manual(name = "", values = shape_values, 
                       breaks = names(class_labels),
                       labels = class_labels) +
    scale_linetype_manual(name="", values = line_values,
                          breaks = names(class_labels),
                          labels = class_labels) +
    theme(
      legend.position = c(.95, .05),
      legend.justification = c("right", "bottom"),
      legend.box.just = "right",
      legend.margin = margin(2, 2, 2, 2),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_y_continuous(breaks = seq(0.5, 1, by = 0.1), limits = c(0.562, 1)) +
    scale_x_continuous(breaks = seq(0, 2000, by = 500))
  # ggsave(paste0("Fig",fig_n,".tiff"), units = "px", width = 3600, height = 2400, dpi=600)
  ggsave(paste0("Fig",fig_n,".eps"), device = cairo_ps, dpi=1200, height = 4.4, width = 6.6)
  fig_n <- fig_n + 1
}

############
