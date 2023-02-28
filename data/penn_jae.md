# Pennsylvania 'reemployment bonus' demonstration data set

## Description of the data

The data file used for this paper was extracted from the master file of the study. The master file and documentation may be obtained from the U.S. Department of Labor, 200 Constitution Avenue, NW Washington, DC 20210, USA

## Relevant information can be found in the Final Report

Corson, W., Decker, P., Dunstan, S., and Keransky, S. (1992), "Pennsylvania 'reemployment bonus' demonstration: Final Report", Unemployment Insurance Occasional Paper}, 92--1, (Washington D.C.: US Dept of Labor, Employment and Training Administration).

## Format

Our extract has 13913 observations on 23 variables (some of which are dummies constructed from the original definitions.) These data are in the file penn_jae.dat, which is an ASCII file in DOS format that is zipped in bilias-data.zip.

The 23 variables (columns) of the datafile utilized in the article may be described as follows:

**abdt**: chronological time of enrollment of each claimant in the Pennsylvania reemployment bonus experiment.

**tg**: indicates the treatment group (bonus amount - qualification period) of each claimant. if tg=0, then claimant enrolled in the control group if tg=1, then claimant enrolled in the group 1, and so on. (for definitions of the each group see the article, or the Final Report).

**inuidur1**: a measure of length (in weeks) of the first spell of unemployment; this measure was used in the empirical analysis of this article. (this is a constructed variable and the following is a quote from the documentation: "This variable reflected the number of weeks in the claimant's initial UI duration using a break-in-payments definition of a spell. If a claimant did not collect any weeks of UC, INUIDUR1 was set to 1 because he/she must have signed for at least a waiting week in order to have been selected for the demonstration. If a claimant had a gap of at least 7 weeks between the AB_DT and the first claim week paid, INUIDUR1 was also set to 1 to capture the waiting week. Otherwise, the initial UI duration was deemed to have ended if there was a break in payments of at least 3 weeks' duration. In this instance, INUIDUR1 was set equal to the duration of the spell up to the break, plus one for the waiting week. For all other cases, INUIDUR1 equalled the length of the spell plus one for the waiting week."

**inuidur2**: a second measure for the length (in weeks) of the first spell of unemployment; it was not used in our data analysis.

**female**: dummy variable; it indicates if the claimant's sex is female (=1) or male (=0).

**black**: dummy variable; it indicates a person of black race (=1).

**hispanic**: dummy variable; it indicates a person of hispanic race (=1).

**othrace**: dummy variable; it indicates a non-white, non-black, not-hispanic person (=1).

**dep**: the number of dependents of each claimant; In case the claimant has 2 or more dependents, it is equal to 2. Else it is 0 or 1 accordingly.

**q1-q6**: six dummy variables indicating the quarter of experiment during which each claimant enrolled.

**recall**: takes the value of 1 if the claimant answered \`\`yes'' when was asked if he/she had any expectation to be recalled.

**agelt35**: takes the value of 1 if the claimant's age is less than 35 and 0 otherwise.

**agegt54**: takes the value of 1 if the claimant's age is more than 54 and 0 otherwise.

**durable**: it takes the value of 1 if the occupation of the claimant was in the sector of durable manufacturing and 0 otherwise.

**nondurable**: it takes the value of 1 if the occupation of the claimant was in the sector of nondurable manufacturing and 0 otherwise.

**lusd**: it takes the value of 1 if the claimant filed in Coatesville, Reading, or Lancaster and 0 otherwise. These three sites were considered to be located in areas characterized by low unemployment rate and short duration of unemployment.

**husd**: it takes the value of 1 if the claimant filed in Lewistown, Pittston, or Scranton and 0 otherwise. These three sites were considered to be located in areas characterized by high unemployment rate and short duration of unemployment.

**muld**: it takes the value of 1 if the claimant filed in Philadelphia-North, Philadelphia-Uptown, McKeesport, Erie, or Butler and 0 otherwise. These three sites were considered to be located in areas characterized by moderate unemployment rate and long duration of unemployment.

## References

Yannis Bilias, "Sequential Testing of Duration Data: The Case of Pennsylvania 'Reemployment Bonus' Experiment", Journal of Applied Econometrics, Vol. 15, No. 6, 2000, pp. 575-594. Yannis Bilias, Univ of Cyprus, [bilias\@ucy.ac.cy](mailto:bilias@ucy.ac.cy){.email}
