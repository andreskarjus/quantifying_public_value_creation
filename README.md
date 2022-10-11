**Code and data accompanying<br>"Quantifying public value creation by public service media using big programming data"<br>by Indrek Ibrus, Andres Karjus, Vejune Zemaityte, Ulrike Rohn, Maxilian Schich** <br>The paper is submitted and in review; currently available as a preprint at https://osf.io/preprints/socarxiv/hysma. 

The dataset provided here covers only the Estonian Public Broadcasting part of our analysis (the main focus of the paper), in a semi-anonymized and aggregated form, as per agreement with the data provider. That means not all variables from the original dataset are made available (such as titles of the programmes), but enough to reproduce our analyses. The elements in the code pertaining to other datasets described in the paper have been commented out accordingly.

The code is written in [R](https://www.r-project.org/). To reproduce our analyses and graphs:

1. Unpack the quantifying_public_value_data.csv.zip - inside is a CSV file with a subset of the ERR data necessary to reproduce out analyses.
2. Open the quantifying_public_value_code.R file in an instance of R (we recommend using the RStudio IDE). Follow the instructions, including package installation and loading the data, in the header of this code file. Either place the CSV file and the quantifying_public_value_scripts.R file in the working directory of your R installation (run getwd() to see where), or provide full paths where indicated in the code file.
3. Run the code line by line to reproduce the analyses and graphs as they appear in the paper.

The code is authored and maintained by Andres Karjus; for questions and comments find contacts at https://andreskarjus.github.io

If you make use of this code and/or data, kindly cite:<br>
Ibrus, Indrek, Andres Karjus, Vejune Zemaityte, Ulrike Rohn, and Maximilian Schich. 2022. "Quantifying Public Value Creation by Public Service Media Using Big Programming Data." SocArXiv. https://osf.io/preprints/socarxiv/hysma
