# timebank-mining
R and Python scripts for collecting and analyzing data from the TimeBank corpus

## preprocessing.R
A variety of functions are defined in order to prepare the timebank data for analysis, and the entire corpus is loaded into a single xml tree.

To use this code, open the R interpreter and enter `source(preprocessing.R)`.  You should do this within the directory `/.../timebank_1_2/data/extra` rather than `/.../timebank_1_2/data/timeml`.  The texts in the former directory have *extra* annotations; the only one relevant to this script is the `<s>...</s>` which demarcates sentence boundaries.  N.B. The file `preprocessing.R` must be in this directory as well.  (So must the file `index.txt`, but this is already present in the distribution from the LDC.)

If all goes well, you now have xml tree called `docs` which you may query with XPATH / getNodeSet().  You also have at your disposal the following functions:

More to come...

##
Some data structures are built as illustrations.

## Python scripts

Still need to be uploaded to repository, after I find them on previous hard drive.
