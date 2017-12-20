# timebank-mining
R scripts for collecting and analyzing data from the TimeBank corpus

### Usage ###
At the moment, the path to the timebank directory `<path/to/timebank_1_2/data/extra>` must be set manually by changing the value of the `path` variable in the file `preprocess.R'`.  Note that these scripts require the extended annotations found in the files in the `data/extra` directory.  Also required is the file `index.txt`, but this should already be present in the distribution from the LDC.

From R interpreter:
`source('<path/to/main.R>')`

To see all data structures and avalable functions:
`ls()`

