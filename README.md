# Euro 2024 vs. Copa America 2024: A Statistical Comparison

A hypothesis-testing project in R that checks whether the 2024 UEFA European Championship and the 2024 CONMEBOL Copa America really played differently, as fans claimed during the summer both tournaments ran. Match-level stats from both tournaments are compared with Welch two-sample t-tests.

## The question

During summer 2024 the common view on soccer social media was that Euro matches were slow, organized "tactical chess matches" and Copa America matches were fast, chaotic "street fights." The project asks whether that difference shows up in the match statistics or is just a perception shaped by viewing schedules and stereotypes.

- **Null hypothesis:** per-match means are the same in both tournaments.
- **Alternative:** per-match means differ.

## Data

- 81 matches from the 2024 tournaments: 51 Euro matches and 30 Copa America matches.
- One row per match with home and away team stats: goals, expected goals (xG), shots, passes (total, own half, opposition half), fouls, yellow and red cards, plus stadium, attendance and many other columns.
- Home and away values are summed to give match totals for goals, xG, shots, fouls, yellow cards, red cards, passes, passes in own half and passes in the opposition half.
- The source CSV (`EuroVsCopa.csv`) is not included in this repository. The R Markdown file reads it from a local absolute path.

## Approach

1. Load the match data and build combined (home + away) match totals for nine metrics.
2. Split the rows into Euro (rows 1-51) and Copa (rows 52-81) samples.
3. Run a Welch two-sample t-test (`t.test`) for each metric.
4. Plot each comparison as a bar chart of the tournament means with standard-error bars (ggplot2), with the t-statistic and p-value in the title.

## Key findings

| Metric (per match) | Euro mean | Copa mean | t | p-value |
|---|---|---|---|---|
| Goals | 2.29 | 2.17 | 0.37 | 0.711 |
| Expected goals (xG) | 2.61 | 2.43 | 0.90 | 0.370 |
| Shots | 25.73 | 23.10 | 2.04 | 0.045 |
| Fouls | 22.31 | 26.20 | -2.94 | 0.0049 |
| Yellow cards | 4.41 | 3.67 | 1.51 | 0.134 |
| Red cards | 0.10 | 0.30 | -1.68 | 0.100 |
| Passes | 972.92 | 777.90 | 7.50 | 8.6e-11 |
| Passes in own half | 413.39 | 343.37 | 3.77 | 0.00036 |
| Passes in opposition half | 423.86 | 285.13 | 8.21 | 3.9e-12 |

- **Scoring was about the same.** Goals (p = 0.71) and xG (p = 0.37) showed no significant difference.
- **Euro matches had more shots** (25.7 vs. 23.1 per match, p = 0.045), and **Copa matches had more fouls** (26.2 vs. 22.3, p = 0.005).
- **Passing showed the largest gap.** Euro matches averaged about 195 more passes per match (973 vs. 778, p < 0.001).
- **The biggest single difference was passing in the opposition half** (424 vs. 285 per match, t = 8.21, p = 3.9e-12), which fits the "more controlled, build-up style" view of the Euros.
- The write-up concludes that the null hypothesis is rejected: the two tournaments were measurably different, mainly in passing volume and fouls, not in scoring.

## Tools

R, R Markdown, ggplot2 (Welch t-tests via base R `t.test`). Presentation in PowerPoint.

## Repository contents

| Path | Description |
|---|---|
| `CopaVSEuroStats.Rmd` | R Markdown analysis: feature construction, t-tests and plots |
| `docs/CopaVSEuroStats.pdf` | Knitted PDF of the analysis with all test output and charts |
| `docs/CopaVSEuroStats.html` | Knitted HTML version of the same analysis |
| `docs/EuroVsCopaWriteUp.pdf` | One-page write-up of the motivation, method and conclusion |
| `docs/EuroVsCopaPresentation.pptx` | Slide deck summarizing the project |

Start with the [write-up](docs/EuroVsCopaWriteUp.pdf), then see the [full analysis with charts](docs/CopaVSEuroStats.pdf).

## Limitations and next steps

- Matches are treated as independent samples; team strength and tournament stage are not controlled for.
- The write-up names player tracking data (distance covered, speed) and possession statistics as the next data to add. These were not publicly available.
