# sentim140_classif

## About

A real-time monitoring project that shows how many women are interviewed and determines where they need more space in the news.

## Main aim

This project aims to classify sentiment140 data base based on a methodology developed as part of a work named 'Mining large amount of short text from your desktop' developed at *Universidade Federal de Minas Gerais* - UFMG at Belo Horizonte, MG - Brazil.


## How to use it

### To get data

Download it from [http://help.sentiment140.com/for-students](http://help.sentiment140.com/for-students)

### To use the methodology

Run: scripts at SRC


## Data

#### sentim140

It is available at [Google Drive link](https://docs.google.com/file/d/0B04GJPshIjmPRnZManQwWEdTZjg/edit?resourcekey=0-betyQkEmWZgp8z0DFxWsHw).

#### Text annotation 

It is possible by using *Linguakit*. 

1. Install it following [this steps](https://github.com/citiususc/Linguakit).

a. git clone  https://github.com/citiususc/Linguakit

b. cd Linguakit

c. sudo make deps

d. sudo make install

e. sudo make test-me

2. Run *03_txtFilesWithTextsForLinguakit.R*

3. Copy *fileToBatch.sh* from *SRC* and paste it into *Linguakit* directory.

4. Open the terminal and go to *Linguakit* directory.

5. Give execute permission to your script: chmod +x fileToBatch.sh

6. Run the script: ./fileToBatch.sh

## R Libraries

* doParallel

* dplyr

* future

* future.apply

* slam

* stringr

* tidytext

* tm



## Last update

February, 2022


## Author

Larissa Sayuri Futino Castro dos Santos

Marcos Oliveira Prates
