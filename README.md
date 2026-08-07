# MA 214: Applied Statistics — Course Materials

Teaching materials for **MA 214 Applied Statistics** at Boston University: an applied, simulation-based second course in statistics covering regression, statistical inference, and Bayesian methods in R.

These materials come out of a multi-year redesign of BU's introductory statistics sequence, supported by the Shipley Center for Digital Learning & Innovation. They are shared so that other instructors can use and adapt them.

## What's here

| Directory | Contents |
| --- | --- |
| `Module1/` | Modeling and linear regression — lectures 1–7, labs 1–2, Project 1 |
| `Module2/` | Statistical inference — lectures 8–12 |
| `Module3/` | Regression inference — lectures 13–16, Project 2 |
| `Module4/` | Bayesian statistics — lectures 17–21, labs 3–5 |
| `common/` | Shared LaTeX style (`lec_style.tex`) used by every lecture |
| `instructor_inputs/` | Schedule and summary source files that drive the course site |
| `course_site_builder/`, `docs/`, `mkdocs.yml` | Generator and sources for the course website |

Each lecture folder holds the slide source, an agenda (recap plus learning objectives), skeletal notes for students, an in-class worksheet, and — where relevant — an R demo script with its data and figures.

The course is organized around a published list of **learning objectives** (`docs/learning_objectives.md`); lecture agendas and assessments refer to them by number.

## Building

**Lecture slides** are LaTeX/Beamer and depend on `common/lec_style.tex`, which they reach by relative path. Compile from inside a lecture directory:

```
cd Module1/lecture01 && pdflatex Lecture1.tex
```

**Labs and demos** are R Markdown and R scripts; figures are committed, so the slides build without re-running any R.

**The course site** is built with MkDocs:

```
pip install -r requirements.txt && pip install -e .
python generate_schedule_table.py
mkdocs build
```

## Assessments

Quizzes, exams, and their solutions are **not** in this repository — they are kept private so the questions stay usable. Instructors at other institutions who would like access are welcome to get in touch.

## License and attribution

Original material here is released under the MIT License (see `LICENSE`).

Some content is adapted from, or refers to, third-party sources that are **not** covered by that license and remain under their own terms — in particular the two course texts, [*Introduction to Modern Statistics*](https://openintro.org/book/ims/) (Çetinkaya-Rundel and Hardin) and [*Bayes Rules!*](https://www.bayesrulesbook.com/) (Johnson, Ott, and Dogucu), including their cover images and any figures or datasets drawn from them. Please respect the original licenses when reusing.

The reading distributed with the Lecture 11 activity — Wong et al., "Reduced-Frequency GLP1 Therapy Maintains Weight, Body Composition, and Metabolic Syndrome Improvements: A Case Series," *Obesity* (2026), [doi:10.1002/oby.70137](https://doi.org/10.1002/oby.70137) — is redistributed here under its own CC BY license.
