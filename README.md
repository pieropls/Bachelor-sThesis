# Work, Income & Sleep
<p align="center"><i>Bachelor’s Thesis in Mathematics & Economics</i></p>

## Project overview

### Context

<p align="justify">
This repository gathers the code and documentation for a project I completed during the final year of my <b>bachelor's degree in mathematics and economics</b> at <b>Paris-Saclay University</b>. Although the program didn’t require a formal thesis, this applied research project, part of the dual-degree curriculum, serves as an informal "thesis" tying together both fields through econometrics. The research takes inspiration from the paper <i>"Sleep and the Allocation of Time"</i> by <b>Jeff E. Biddle</b> and <b>Daniel S. Hamermesh</b>, and aims to answer an open question:
</p>

<p align="center"><i>How do working hours and income influence sleep, and what are the implications for health and productivity?</i></p>

<p align="justify">
To dig into this, I used data from the <b>US Time Use Study</b> and built a set of <b>linear regression models in R</b> to explore how work schedules and economic pressure might shape the way we sleep and what that means for well-being and efficiency.
</p>

### Motivation

<p align="justify">
Sleep is something we all need, but somehow never get enough of, especially when life gets busy. As our daily lives speed up and our schedules get tighter, sleep increasingly feels like a luxury. And it's not just about feeling tired. Between 2001 and 2020, the age-adjusted prevalence of diabetes in the U.S. kept rising (CDC), highlighting a broader public health issue tied to long work hours, stress, and the erosion of rest.
<br><br>
Meanwhile, the so-called <i>sleep economy</i> was worth $432 billion in 2019 and is projected to reach $585 billion by 2024 (Statista). That number says a lot: sleep isn’t just a personal matter, it’s a societal one, with real economic stakes. The health consequences of chronic sleep deprivation are well-documented. Obesity, cardiovascular disease, diabetes, all of them have been linked to poor sleep quality or duration. According to <i>Our World in Data</i>, the global obesity crisis is getting worse, and lack of rest, driven by economic pressure and long workdays, is part of the problem.
<br><br>
On a more personal note: as a student juggling a demanding dual major, I’ve lived through what it means to trade sleep for productivity. Long nights, early alarms, never quite enough time. That experience, paired with everything the data says, made this project feel both relevant and urgent. So this report is more than just an academic exercise. It’s a mix of curiosity, real-life experience, and a desire to better understand how work, money, and rest fit together and how that balance (or imbalance) shapes our health and efficiency.
</p>

### Contributors & supervision

- **Contributor:** A. Augé  
- **Supervisor:** Professor C. Lelarge

## Repository structure

```
Work.Sleep.Income/
│
├── Code.R                         # R script
│
├── paper&data/
│   ├── data.csv                   # Raw data from US Time Use Study
│   ├── data-explained.pdf         # Documentation of dataset variables and structure
│   ├── paper.pdf                  # Foundational paper: “Sleep and the Allocation of Time”
│   └── cover.png                  # AI-generated project cover image
│
├── thesis/
│   ├── thesis.pdf                 # Final written thesis
│   ├── rmd-code-visual.pdf        # Rendered R Markdown
│   └── presentation.pdf           # Slide deck for thesis defense
```

## License

This project is licensed under the terms of the [MIT License](./LICENSE).
