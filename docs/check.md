# R check log and remedies
## Things you should address
### Missing data sets

Problem: Some documented datasets are not present in the package (bhatarah09e, cowan98, klauer04)

Solution: Add the data sets! For now, I had to move the documentation of these datasets out of the package into `docs` so I could continue to auto-check the rest.


### Missing documentation
farrell04.rda has no .Rd file. I've moved the .rda into `docs` so I could continue package checking.

- [healey14.Rd, kane04.Rd, kane04t.Rd, thalmann19b.Rd],  have no example code - is there's nothing in the paper that can be reproduced from these data?

- [thalmann19c.Rd, thalman19d.Rd]  have incomplete example code.

### Misspelt dataset
Problem: Name of dataset (mudrock70) different to name of documentation (and wrong)

Solution: I've manually renamed. You should change your build code.

### Example code in Rd files should work
farrell04.Rd - Example code not working in a way I could not quickly diagnose. I had to move the Rd out of the package so I could continue auto-checking.

### Build scripts should be reproducible
- Your build scripts don't seem reproducible. For example, the one to make jarrold10.rda throws errors, and assumes the presence of `rhyme_ids` despite this not being defined in the script. You should probably fix this. Also, this is why I created post-build.R rather than modifying your build scripts. Ideally the stuff in the post-build script would be part of your main scripts.
### Multiple copies of same data?
- Is kane04 just a summary of kane04t? If so that's bad practice, you shouldn't have the same dataset twice. If necessary, include example code for deriving what kane04 gives you from kane04t.
### Dodgy datasets?
- murdock70.Rd - "It is very much possible the second plot is erroneous" - What's the issue? Should we not trust the latency data? Why not? Sounds like something that should appear more prominently than a note in the example?

- oezteken10 - "Vaguely reproduce Figure B1 in Oberauer et al. (2018)" - this is an odd comment. What is vague about the reproduction? Should that vagueness make us concerned about the veracity of the data?"

- rerko14 - " very imprecise reconstruction of Figure 10B in Oberauer et al. (2018)" - see previous comment.

- thalman19a - "Approximate reproduction of Figure 1" - see previous comment

- vandenberg12 - " Approximate reproduction of Figure 11" - see previous comment

- vergauwe10 - "## Approximately reproduce Fig. 1 in Vergauwe et al. (2010) (CogLoad measure seems different)" - so in this case, it seems like you don't reproduce the figure. That probably calls for further investiagtion, perhaps with the author if necessary?






## Things I've fixed (for information)
### Use of commands from default packages

Problem:  R has some default packages (e.g. "stats") i.e. not part of core R but always installed. If you use commands from these packages in your own package's R scripts, you need to tell R you're doing that, otherwise it can potentially get confused on some systems. 

Solution: Add to the NAMESPACE file the code provided by the R CMD check

### Line widths in Rd files
Problem: In several Rd files, your example code had a line length of over 100
characters. This is a problem because it screws up the formatting of the PDF
manual for the package. It's also a bad idea to go wider than 100 characters
anyway, for readibility of your code.

Solution: Use a text editor that forces or has a reformat option for a line
width of less than 100 characters.
### Use of packages dependencies
If you're using packages, these need to be declared. If they're being used only in example code, you add them to `Suggests:` in `DESCRIPTION`. For a data package, this is the way to go in order to make it as easy as possible to install the data package for the end user.

### Non-ASCII in Rd files

- This was mainly use of em-dash rather than a hyphen in reference section. It's a problem because em-dash is not part of the basic ASCII character set and so reduces compatibility across platforms. Detectable using the command below (Linux command line) and edited by hand (could have written a script but by hand was quicker). Best solution here is to replace with hyphen as increases compatibility without losing information.

`iconv -f utf8 -t ascii B1.3VWMCapacity.Rd`

- In a few cases, converting to ASCII would have lost something (mainly accents, e.g. Röer). Here we fix by adding:

`\encoding{UTF-8}`

### Non-ASCII in data

Problem: One dataset (jarrold10) contained a few non-ASCII characters in the response column. This can cause compatibility problems. 

Solution: Added script in build to remove these 7 offending data points.
  
### Broken cross-reference links in Rd file
Problem: Mis-spelling \link{farel04} -> \link{farrell04}. This means link will fail.

Solution: Correct spelling.


### Misdescribed datasets

Problem: The columns listed in the documentation are different to the columns in dataset. This is obviously a problem in terms of the resusability of the data.

Solution: Make sure these two things match!

### Dataset not in long format

Problem: healey14 has 102 columns. This deviation from long format makes it
difficult to write machine-checkable documentation, because the .Rd file would
need 102 entries, one for each column. It also violates the principles of tidy
data because the column labels denote more than one thing. 

Solution: Convert dataset to long format (code in `post-build.R`)

### Too smart for the auto-checking
Problem: Putting \item in Rd files inside of \itemize{} breaks the automated
checking of the Rd file against the date file. Similarly, using something like
\item{digitX} or \item{digit1} - \item{digit5} would require the auto-check to
have semantic understanding of your documentation, which obviously it
doesn't. Being too smart for the autochecking is a problem because you lose the
ability to detect human errors in the documentation.

Solution: Don't use \itemize. Do make sure you have exactly one \item entry per column.

### Undocumented function arguments

Problem: The function you're documenting has more arguments than the
documentation describes. This is obviously a problem for usability of the
function.

Solution: Document all of the arguments!

### Using TRUE rather than T
Problem: R allows you to use T for TRUE and F for FALSE. Doing so is a very bad idea. Why? Type `T <- 2` and now re-run your code. R allows you to redefine what T means, which causes hard to trace bugs if someone happens to have done this in their code (for example, using T or F as a variable name). 

Solution: Use TRUE and FALSE; these cannot be re-defined.

	  
### The % symbol in Rd files
Unfortunately, the % symbol has a special meaning in Rd files, i.e. it means the start of a comment. This means that if you use a command like `%in%` in example code in a Rd file, it'll be interpreted as a comment. This means you have to replace any `%` with `\%` for the Rd file to compile correctly. This in turn means you can't then execute the code interactively within the Rd file. Very irritating.
### Example code in Rd files should work
barrouillet11.Rd had the code `Tasks <- unique(Data$task)`. Not only is `Tasks` not used anywhere else in the code, `Data` is not defined anywhere so it throws an error.

chen09 had a comment that wasn't commented (no #)

grenfell12.Rd - Failed to declare `library(dplyr)`, so code throws error due to using base R version of `filter`.

jarrold10.Rd - Data referred to as 'jar' rather than 'jarrold10', so code does not run.

madigan71.Rd - data referred to as `magidan71`, so code does not run.


## Things I still need to address
### Compression issues

Problem: The data files are not compressed using the optimal compression technique. This is a problem because it makes the package larger than it needs to be. One might be tempted to argue that this doesn't matter that much when the package is less than 10MB while your hard drive has 1TB, but this misses the point that CRAN archives every version ever released of the 15,000 R packages, and this archive is mirrored to hundreds of sites around the world to ensure it's never lost. 

Solution: The solution is given in the Rcheck output.

