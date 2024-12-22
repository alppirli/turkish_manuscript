#!/bin/bash

mkdir conllu_boun
for FILENAME in corpus/*; do
  BASENAME=$(basename "$FILENAME" .txt)
  echo "working on $FILENAME"
  # cat corpus/Adıvar_Handan.txt | python udpipe2_client.py --model="turkish-boun-ud-2.12-230717" --tokenize="" --tag="" > "conllu_boun/Adıvar_Handan.conllu"
  cat "$FILENAME" | python udpipe2_client.py --model="turkish-boun-ud-2.12-230717" --tokenize="" --tag="" > "conllu_boun/$BASENAME.conllu"
done

