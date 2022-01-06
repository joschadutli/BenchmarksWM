# R check log and remedies
## NOTEs
### New submission (no action needed)
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Joscha Dutli <joscha.dutli@psychologie.uzh.ch>’

New submission

Size of tarball: 8623669 bytes

### Installed package size (make case to CRAN on submission)
* checking installed package size ... NOTE
  installed size is  9.3Mb
  sub-directories of 1Mb or more:
    data   8.9Mb

### No visible functions/bindings

* checking R code for possible problems ... NOTE
lineplot.ci: no visible global function definition for ‘aggregate’
lineplot.ci: no visible binding for global variable ‘sd’
lineplot.ci: no visible global function definition for ‘errbar’
lineplot.ci: no visible global function definition for ‘axis’
lineplot.ci: no visible global function definition for ‘par’
Undefined global functions or variables:
  aggregate axis errbar par sd
Consider adding
  importFrom("graphics", "axis", "par")
  importFrom("stats", "aggregate", "sd")
to your NAMESPACE file.


### Line widths in Rd files
* checking Rd line widths ... NOTE
Rd file 'barrouillet11.Rd':
  \examples lines wider than 100 characters:
     pt=c(24, 24, 23, 23, 23, 23, 21, 21, 21, 24, 24, 22, 22, 22) # markers corresponding to Barrouillet et al (2011)
     ptcol=c("red", "red", "blue", "blue", "blue", "blue", "black", "black", "black", "red", "red", "green", "green", "green")

Rd file 'farrell04.Rd':
  \examples lines wider than 100 characters:
     legend(6,2900,c("No interference","Interference"),lty=ltyk,pch=20+c(1:2),pt.bg=bgk,cex=1.,pt.cex=1.3, xjust=1)

Rd file 'frankish89.Rd':
  \examples lines wider than 100 characters:
     legend(1,.5,c("Ungrouped","0.25 s", "0.5 s", "1 s", "2 s"),lty=c(1:5),pch=20+c(1:5),pt.bg=bgk,cex=0.6,pt.cex=1.0)

Rd file 'grenfell12.Rd':
  \examples lines wider than 100 characters:
       legend(10,.6,unique(post$output),lty=c(1:k),pch=c(21:(20+k)),pt.bg=colors,pt.cex=0.6,y.intersp = 1, cex = 0.6)
       axis(1, at=seq(from=0,to=max(post$listlength)+1,by=3), labels=seq(from=0,to=max(post$listlength)+1,by=3),

Rd file 'keshvari13.Rd':
  \examples lines wider than 100 characters:
     ## Approximate reproduction of Figure 2c in Keshvari et al. (2013) and and Figure B3 in Oberauer et al. (2018)

Rd file 'lewandowsky10.Rd':
  \examples lines wider than 100 characters:
     axis(side = 1, at = c(0,1,2,3), labels = c("Quiet", "1 distractor", "3 identical distr.", "3 different distr."))

Rd file 'oberauer06.Rd':
  \examples lines wider than 100 characters:
     legend(50,1800,c("no-switch", "switch"),lty=c(1:2),pch=c(22,19),pt.bg=bgk,cex=0.6,pt.cex=1.0, yjust = 1, xjust = 1)

Rd file 'oeztekin10.Rd':
  \examples lines wider than 100 characters:
     legend(maxx,miny,c("SP=1", "SP=2", "SP=3", "SP=4", "SP=5", "SP=6"), pch=21, pt.bg = bgk, xjust=1, yjust=0)

Rd file 'rerko14.Rd':
  \examples lines wider than 100 characters:
     hist(pd$dist, breaks=10, xlab = "Euclidean distance", main = "Prob. for Spatial Distances of Errors", freq=F)

Rd file 'tan08.Rd':
  \examples lines wider than 100 characters:
     lines(x = plotd$serpos[plotd$ptime == 1], y = plotd$corr[plotd$ptime == 1], type = "b", lty = 1, pch = 15)
     lines(x = plotd$serpos[plotd$ptime == 2.5], y = plotd$corr[plotd$ptime == 2.5], type = "b", lty = 2, pch = 16)
     lines(x = plotd$serpos[plotd$ptime == 5], y = plotd$corr[plotd$ptime == 5], type = "b", lty = 3, pch = 17)

Rd file 'vergauwe15.Rd':
  \examples lines wider than 100 characters:
     pd <- aggregate(cbind(acc, CL) ~  domain + num, data = vergauwe15[which(vergauwe15$condition == "easy"),], FUN = mean)
     pd <- aggregate(cbind(acc, CL) ~  domain + num, data = vergauwe15[which(vergauwe15$condition == "hard"),], FUN = mean)

These lines will be truncated in the PDF manual.

## WARNINGs
### Dependencies
* checking dependencies in R code ... WARNING
'library' or 'require' call not declared from: ‘Hmisc’
'library' or 'require' call to ‘Hmisc’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.

### Non-ASCII and empty sections in Rd files
* checking Rd files ... WARNING
man/B1.2SetsizeRT.Rd: non-ASCII input and no declared encoding
man/B1.3VWMCapacity.Rd: non-ASCII input and no declared encoding
man/B10PrioritizationOfInformation.Rd: non-ASCII input and no declared encoding
man/B11EffectsOfKnowledge.Rd: non-ASCII input and no declared encoding
man/B12IndividualDifferences.Rd: non-ASCII input and no declared encoding
man/B2.1RetentionInterval.Rd: non-ASCII input and no declared encoding
man/B2.4PresentationDuration.Rd: non-ASCII input and no declared encoding
man/B3SerialPositionEffects.Rd: non-ASCII input and no declared encoding
man/B4ErrorCharacteristics.Rd: non-ASCII input and no declared encoding
man/B5MultipleDemands.Rd: non-ASCII input and no declared encoding
man/B6IrrelevantSounds.Rd: non-ASCII input and no declared encoding
man/B7WordLengthEffect.Rd: non-ASCII input and no declared encoding
man/B8SimilarityEffects.Rd: non-ASCII input and no declared encoding
man/B9DistinctivenessGrouping.Rd: non-ASCII input and no declared encoding
man/barrouillet07.Rd: non-ASCII input and no declared encoding
man/barrouillet11.Rd: non-ASCII input and no declared encoding
man/bays11.Rd: non-ASCII input and no declared encoding
man/bell19.Rd: non-ASCII input and no declared encoding
man/bhatarah09a.Rd: non-ASCII input and no declared encoding
man/bhatarah09b.Rd: non-ASCII input and no declared encoding
man/bhatarah09c.Rd: non-ASCII input and no declared encoding
man/bhatarah09d.Rd: non-ASCII input and no declared encoding
man/bhatarah09e.Rd: non-ASCII input and no declared encoding
man/chein11.Rd: non-ASCII input and no declared encoding
man/chen09.Rd: non-ASCII input and no declared encoding
man/cowan02.Rd: non-ASCII input and no declared encoding
man/cowan07.Rd: non-ASCII input and no declared encoding
man/cowan98.Rd: non-ASCII input and no declared encoding
man/donkin12b.Rd: non-ASCII input and no declared encoding
man/farrell03.Rd: non-ASCII input and no declared encoding
man/farrell04.Rd: non-ASCII input and no declared encoding
man/farrell09.Rd: non-ASCII input and no declared encoding
man/farrell13.Rd: non-ASCII input and no declared encoding
man/floden10.Rd: non-ASCII input and no declared encoding
man/frankish89.Rd: non-ASCII input and no declared encoding
man/gilchrist14.Rd: non-ASCII input and no declared encoding
man/grenfell12.Rd: non-ASCII input and no declared encoding
man/harvey07.Rd: non-ASCII input and no declared encoding
prepare_Rd: healey14.Rd:25-27: Dropping empty section \details
man/jarrold10.Rd: non-ASCII input and no declared encoding
man/jarrold13.Rd: non-ASCII input and no declared encoding
man/kane04.Rd: non-ASCII input and no declared encoding
man/kane04t.Rd: non-ASCII input and no declared encoding
man/klauer04.Rd: non-ASCII input and no declared encoding
man/lange11.Rd: non-ASCII input and no declared encoding
man/lewandowsky10.Rd: non-ASCII input and no declared encoding
prepare_Rd: lineplot.ci.Rd:28-30: Dropping empty section \details
prepare_Rd: lineplot.ci.Rd:43-45: Dropping empty section \note
prepare_Rd: lineplot.ci.Rd:49-51: Dropping empty section \seealso
man/macnamara11.Rd: non-ASCII input and no declared encoding
man/madigan71.Rd: non-ASCII input and no declared encoding
man/morin10.Rd: non-ASCII input and no declared encoding
man/murdock70.Rd: non-ASCII input and no declared encoding
man/nimmo06.Rd: non-ASCII input and no declared encoding
man/oberauer03b.Rd: non-ASCII input and no declared encoding
man/oberauer06.Rd: non-ASCII input and no declared encoding
man/oeztekin10.Rd: non-ASCII input and no declared encoding
man/page06.Rd: non-ASCII input and no declared encoding
man/peteranderl17.Rd: non-ASCII input and no declared encoding
man/portrat16.Rd: non-ASCII input and no declared encoding
man/quinlan17.Rd: non-ASCII input and no declared encoding
man/rerko14.Rd: non-ASCII input and no declared encoding
man/ricker14.Rd: non-ASCII input and no declared encoding
prepare_Rd: ricker17.Rd:28-30: Dropping empty section \details
man/schlittmeier12.Rd: non-ASCII input and no declared encoding
man/shepherdson18.Rd: non-ASCII input and no declared encoding
man/tan08.Rd: non-ASCII input and no declared encoding
man/thalmann19a.Rd: non-ASCII input and no declared encoding
man/thalmann19b.Rd: non-ASCII input and no declared encoding
man/thalmann19c.Rd: non-ASCII input and no declared encoding
man/thalmann19d.Rd: non-ASCII input and no declared encoding
man/towse08.Rd: non-ASCII input and no declared encoding
man/unsworth10.Rd: non-ASCII input and no declared encoding
man/vandenberg12.Rd: non-ASCII input and no declared encoding
man/vergauwe10.Rd: non-ASCII input and no declared encoding
man/vergauwe12.Rd: non-ASCII input and no declared encoding
man/vergauwe15.Rd: non-ASCII input and no declared encoding
problems found in ‘B1.2SetsizeRT.Rd’, ‘B1.3VWMCapacity.Rd’, ‘B10PrioritizationOfInformation.Rd’, ‘B11EffectsOfKnowledge.Rd’, ‘B12IndividualDifferences.Rd’, ‘B2.1RetentionInterval.Rd’, ‘B2.4PresentationDuration.Rd’, ‘B3SerialPositionEffects.Rd’, ‘B4ErrorCharacteristics.Rd’, ‘B5MultipleDemands.Rd’, ‘B6IrrelevantSounds.Rd’, ‘B7WordLengthEffect.Rd’, ‘B8SimilarityEffects.Rd’, ‘B9DistinctivenessGrouping.Rd’, ‘barrouillet07.Rd’, ‘barrouillet11.Rd’, ‘bays11.Rd’, ‘bell19.Rd’, ‘bhatarah09a.Rd’, ‘bhatarah09b.Rd’, ‘bhatarah09c.Rd’, ‘bhatarah09d.Rd’, ‘bhatarah09e.Rd’, ‘chein11.Rd’, ‘chen09.Rd’, ‘cowan02.Rd’, ‘cowan07.Rd’, ‘cowan98.Rd’, ‘donkin12b.Rd’, ‘farrell03.Rd’, ‘farrell04.Rd’, ‘farrell09.Rd’, ‘farrell13.Rd’, ‘floden10.Rd’, ‘frankish89.Rd’, ‘gilchrist14.Rd’, ‘grenfell12.Rd’, ‘harvey07.Rd’, ‘jarrold10.Rd’, ‘jarrold13.Rd’, ‘kane04.Rd’, ‘kane04t.Rd’, ‘klauer04.Rd’, ‘lange11.Rd’, ‘lewandowsky10.Rd’, ‘macnamara11.Rd’, ‘madigan71.Rd’, ‘morin10.Rd’, ‘murdock70.Rd’, ‘nimmo06.Rd’, ‘oberauer03b.Rd’, ‘oberauer06.Rd’, ‘oeztekin10.Rd’, ‘page06.Rd’, ‘peteranderl17.Rd’, ‘portrat16.Rd’, ‘quinlan17.Rd’, ‘rerko14.Rd’, ‘ricker14.Rd’, ‘schlittmeier12.Rd’, ‘shepherdson18.Rd’, ‘tan08.Rd’, ‘thalmann19a.Rd’, ‘thalmann19b.Rd’, ‘thalmann19c.Rd’, ‘thalmann19d.Rd’, ‘towse08.Rd’, ‘unsworth10.Rd’, ‘vandenberg12.Rd’, ‘vergauwe10.Rd’, ‘vergauwe12.Rd’, ‘vergauwe15.Rd’


### Non-ASCII in data
* checking data for non-ASCII characters ... WARNING
  Warning: found non-ASCII strings
  'res<85>' in object 'jarrold10'
  'r<85>.' in object 'jarrold10'
  '<85>ice' in object 'jarrold10'
  'a<85>..' in object 'jarrold10'
  'r<85><85>' in object 'jarrold10'
  'm<85><85>.' in object 'jarrold10'
  's<85>..' in object 'jarrold10'
  
### Broken cross-reference links
* checking Rd cross-references ... WARNING
Missing link or links in documentation object 'B3SerialPositionEffects.Rd':
  ‘farell04’

See section 'Cross-references' in the 'Writing R Extensions' manual.

### Code/documentation mismatches
* checking for code/documentation mismatches ... WARNING
Data with usage in documentation object 'cowan98' but not in code:
  ‘cowan98’

Data with usage in documentation object 'klauer04' but not in code:
  ‘klauer04’

Data with usage in documentation object 'murdock70' but not in code:
  ‘murdock70’

Data codoc mismatches from documentation object 'donkin12b':
Variables in data frame 'donkin12b'
  Code: pos rate rt size subject
  Docs: id pos rate rt size

Data codoc mismatches from documentation object 'healey14':
Variables in data frame 'healey14'
  Code: recinpos1 recinpos10 recinpos11 recinpos12 recinpos13
        recinpos14 recinpos15 recinpos16 recinpos17 recinpos18
        recinpos19 recinpos2 recinpos20 recinpos21 recinpos22
        recinpos23 recinpos24 recinpos25 recinpos26 recinpos27
        recinpos28 recinpos3 recinpos4 recinpos5 recinpos6 recinpos7
        recinpos8 recinpos9 rectime1 rectime10 rectime11 rectime12
        rectime13 rectime14 rectime15 rectime16 rectime17 rectime18
        rectime19 rectime2 rectime20 rectime21 rectime22 rectime23
        rectime24 rectime25 rectime26 rectime27 rectime28 rectime3
        rectime4 rectime5 rectime6 rectime7 rectime8 rectime9 recword1
        recword10 recword11 recword12 recword13 recword14 recword15
        recword16 recword17 recword18 recword19 recword2 recword20
        recword21 recword22 recword23 recword24 recword25 recword26
        recword27 recword28 recword3 recword4 recword5 recword6
        recword7 recword8 recword9 session subject word1 word10 word11
        word12 word13 word14 word15 word16 word2 word3 word4 word5
        word6 word7 word8 word9
  Docs: recinposY rectimeY recwordY session subject wordX

Data codoc mismatches from documentation object 'kane04':
Variables in data frame 'kane04'
  Code: afq.analogies afq.reading.comprehension afq.rotated.blocks
        arrow.span ball.span beta3.matrices counting.span
        dat.space.relations digit.span ets.form.board ets.inference
        ets.nonsense.syllogisms ets.surface.development letter.span
        matrix.span navigation.span operation.span paper.folding
        raven.matrices reading.span remote.associates rotation.span
        subj symmetry.span wasi.matrices word.span
  Docs: subj

Data codoc mismatches from documentation object 'kane04t':
Variables in data frame 'kane04t'
  Code: afq.analogies afq.reading.comprehension afq.rotated.blocks
        arrow.span ball.span beta3.matrices counting.span
        dat.space.relations digit.span ets.form.board ets.inference
        ets.nonsense.syllogisms ets.surface.development letter.span
        matrix.span navigation.span operation.span paper.folding
        raven.matrices reading.span remote.associates rotation.span
        subj symmetry.span trial wasi.matrices word.span
  Docs: subj trial

Data codoc mismatches from documentation object 'oberauer06':
Variables in data frame 'oberauer06'
  Code: acc column digit dist init.dig1 init.dig2 init.dig3 init.dig4
        init.dig5 lag ptype row rt serpos session setsize subj trial
        update_type
  Docs: - acc column digit dist init.dig1 init.dig5 lag ptype row rt
        serpos session setsize subj trial update_type

Data codoc mismatches from documentation object 'rerko14':
Variables in data frame 'rerko14'
  Code: correct dist dist1 dist2 dist3 dist4 dist5 id session trial
  Docs: correct dist dist1:dist5 id session trial

Data codoc mismatches from documentation object 'unsworth10':
Variables in data frame 'unsworth10'
  Code: free.recall number.series operation.span prim.mem.1 prim.mem.2
        quantitative.SAT reading.span second.mem.1 second.mem.2 serpos1
        serpos10 serpos2 serpos3 serpos4 serpos5 serpos6 serpos7
        serpos8 serpos9 subj symmetry.span verbal.SAT verbal.analogies
  Docs: - free.recall number.series operation.span prim.mem.1
        prim.mem.2 quantitative.SAT reading.span second.mem.1
        second.mem.2 serpos1 serpos10 subj symmetry.span verbal.SAT
        verbal.analogies

### Undocumented arguments in Rd usage
* checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'lineplot.ci'
  ‘off’ ‘Bakeman’ ‘upper’ ‘lower’ ‘ylim’ ‘xlim’ ‘pt’ ‘col’ ‘ptcol’
  ‘na.rm’ ‘cex’ ‘...’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
* checking Rd contents ... WARNING
Auto-generated content requiring editing in Rd object 'lineplot.ci':
  \keyword{~kwd1}
  \keyword{~kwd2}

### Compression issues
* checking LazyData ... WARNING
  LazyData DB of 8.8 MB without LazyDataCompression set
  See §1.1.6 of 'Writing R Extensions'
* checking data for ASCII and uncompressed saves ... WARNING
  
  Note: significantly better compression could be obtained
        by using R CMD build --resave-data
                    old_size new_size compress
  adam15.rda            12Kb      7Kb    bzip2
  bays11.rda           371Kb    322Kb       xz
  bell19.rda            95Kb     30Kb    bzip2
  bhatarah09a.rda       27Kb     15Kb    bzip2
  bhatarah09b.rda       28Kb     12Kb    bzip2
  bhatarah09c.rda       14Kb      6Kb    bzip2
  bhatarah09d.rda       21Kb     13Kb    bzip2
  cowan02.rda          119Kb     49Kb    bzip2
  cowan07.rda           33Kb     20Kb    bzip2
  farrell03.rda        108Kb     64Kb    bzip2
  farrell04.rda         34Kb     24Kb       xz
  farrell09.rda        159Kb     98Kb       xz
  farrell13.rda        176Kb     79Kb    bzip2
  frankish89.rda        26Kb      9Kb    bzip2
  gilchrist14.rda      108Kb     62Kb    bzip2
  grenfell13.rda        79Kb     31Kb    bzip2
  healey14.rda         2.3Mb    1.6Mb    bzip2
  hedge13.rda          143Kb    105Kb       xz
  jarrold10.rda         80Kb     31Kb    bzip2
  kane04.rda            16Kb     12Kb       xz
  kane04t.rda           39Kb     18Kb       xz
  keshvari13.rda        49Kb     29Kb    bzip2
  lange11.rda          429Kb    182Kb       xz
  mudrock70.rda         35Kb     22Kb    bzip2
  nimmo06.rda          225Kb    129Kb       xz
  oberauer01.rda       1.2Mb    542Kb    bzip2
  oberauer06.rda       577Kb    285Kb    bzip2
  oberauer17.rda       187Kb    122Kb    bzip2
  oeztekin10.rda       420Kb    223Kb       xz
  peteranderl17.rda    107Kb     64Kb    bzip2
  quinlan17.rda        128Kb     63Kb    bzip2
  rerko14.rda           80Kb     42Kb       xz
  ricker14.rda          38Kb     24Kb    bzip2
  ricker17.rda         143Kb     83Kb       xz
  shepherdson18.rda    358Kb    210Kb       xz
  thalmann19a.rda      201Kb     80Kb       xz
  thalmann19b.rda      168Kb     84Kb       xz
  thalmann19c.rda      171Kb     88Kb       xz
  thalmann19d.rda      208Kb    103Kb       xz
  vandenberg12.rda     375Kb    203Kb       xz
  vergauwe15.rda        24Kb     18Kb       xz

## ERRORs
### Parse error
* checking for unstated dependencies in examples ... WARNING
Warning: parse error in file 'BenchmarksWM-Ex.R':
1822:1: unexpected symbol
1821: 
1822: pd
      ^
### Parse error 2 (T instead of TRUE)
* checking examples ... ERROR
Running examples in ‘BenchmarksWM-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: barrouillet07
> ### Title: Barrouillet et al. (2007) Data on the Cognitive Load Effect in
> ###   Complex Span Tasks
> ### Aliases: barrouillet07
> ### Keywords: datasets
> 
> ### ** Examples
> 
> ## Approximate reproduction of Figure 1 in Barrouillet et al. (2007)
> data(barrouillet07)
> pd <- aggregate(cbind(span, CL) ~ task + cogload, data = barrouillet07, FUN = mean)
> plot(c(0.25,0.6), c(3.0,6.5), type = "n", xlab = "Total Procesing Time / Total Time",
+      ylab = "Mean Span", main = "Effects of Cognitive Load", xaxt = "n")
> axis(side = 1, at = c(0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6), labels = T)
Error in as.graphicsAnnot(labels) : T used instead of TRUE
Calls: axis -> as.graphicsAnnot
Execution halted

