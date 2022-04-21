# documentation of fixed bugs as reported by Andy Wills

### missing datasets
1. bhatarah09e
--> fixed so far
2. cowan98
--> fixed so far
3. farrell04
--> not fixed, check out later on.
--> now fixed. problem was in example code: spos instead of serpos
4. klauer04
--> should be fixed. check out in another run of R Check
--> data structure makes sense, R script is fixed

### missing documentations

farrell04 again. this should be something weird. maybe try to re-save the dataset and then create a new documentation.

healey14.Rd, kane04.Rd, kane04t.Rd, thalmann19b.Rd and thalmann19c.Rd, thalman19d.Rd documentations extended to include a summary() command. admittedly this is not ideal, but should be fixed for the moment.

### Misspelt dataset
Problem: Name of dataset (mudrock70) different to name of documentation (and wrong)

Solution: I've manually renamed. You should change your build code.
--> did that!

### Example code in Rd files should work
farrell04.Rd - Example code not working in a way I could not quickly diagnose. I had to move the Rd out of the package so I could continue auto-checking.
--> fixed! 
--> I've moved the Rd and rda files back into the respective folders


### Dodgy datasets?
- murdock70.Rd - "It is very much possible the second plot is erroneous" - What's the issue? Should we not trust the latency data? Why not? Sounds like something that should appear more prominently than a note in the example?

--> solved by going to the library at the UZH and looking at the original plot which is not available online. The figure now is pretty close to the original and the Rd file notes the differences. The general exponential connection is clearly visible in the figure. This was pretty cool. The journal issue was dusty and clearly acquired in the 1970s.

- oezteken10 - "Vaguely reproduce Figure B1 in Oberauer et al. (2018)" - this is an odd comment. What is vague about the reproduction? Should that vagueness make us concerned about the veracity of the data?"

--> unnecessary comment of yours truly. The figure just lacks CIs, that's all.
--> fixed by changing the comment

- rerko14 - " very imprecise reconstruction of Figure 10B in Oberauer et al. (2018)" - see previous comment.

--> fixed, see previous comment

- thalman19a - "Approximate reproduction of Figure 1" - see previous comment

--> removed the word "approximate"

- vandenberg12 - " Approximate reproduction of Figure 11" - see previous comment

--> removed the word "approximate"

- vergauwe10 - "## Approximately reproduce Fig. 1 in Vergauwe et al. (2010) (CogLoad measure seems different)" - so in this case, it seems like you don't reproduce the figure. That probably calls for further investiagtion, perhaps with the author if necessary?

--> the data pattern stays the same. I can contact Evie Vergauwe, but for the moment, I think noting that difference should be enough. Also from a theoretical perspective, the difference in the cognitive load measure isn't that important. The important finding is that with less free time (related to the ratio of processing time vs. total time) memory performance declines.
--> generally, a fix that is more content oriented and less aboput the package and it's usability

--> better descrived in the Rd file now. The original cogload measure cannot be reproduced however because the data available are already aggregated.

### Multiple copies of same data?
- Is kane04 just a summary of kane04t? If so that's bad practice, you shouldn't have the same dataset twice. If necessary, include example code for deriving what kane04 gives you from kane04t.

--> could be integrated in the example code. T means trial level.
--> fixed by renaming kane04t to kane04 and removing initial kane04.
--> the reason behind this was that the aggregated data is not equal to the aggregate of the non-aggregated data, presumably due to exclusion criteria. As is, users have the trial data and if they want to exclude any data, they need to read into Kane et al. (2004) or find the Excel-Sheet of their data online or in the Data folder.


### Build scripts should be reproducible
- Your build scripts don't seem reproducible. For example, the one to make jarrold10.rda throws errors, and assumes the presence of `rhyme_ids` despite this not being defined in the script. You should probably fix this. Also, this is why I created post-build.R rather than modifying your build scripts. Ideally the stuff in the post-build script would be part of your main scripts.

--> this might take longer.
--> added AJ Wills fixes from post-build into build scripts.















