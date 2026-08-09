# MA 214 Lab Instructor Guide

How the lab program runs, what students submit, and what to do when you change something. For what each session covers, see [`instructor_inputs/lab_summary.md`](instructor_inputs/lab_summary.md).

## Lab program

Two tracks: five **skills labs** and two **projects**. Project 1 ends in a video presentation, Project 2 in a written writeup.

| Week | Session | Track | Folder |
| --- | --- | --- | --- |
| 1 | Lab 1 — Working with R and Data | Skills lab | `Module1/lab1/` |
| 2 | Lab 2 — Regression Models | Skills lab | `Module1/lab2/` |
| 3–6 | Project 1, parts 1–4 | Project (video) | `Module1/project1-part1..4/` |
| 7–10 | Project 2, parts 1–4 | Project (writeup) | `Module3/project2-part1..4/` |
| 11 | Lab 3 — Bayes' Rule | Skills lab | `Module4/lab3/` |
| 12 | Lab 4 — Beta-Binomial Conjugate Models | Skills lab | `Module4/lab4/` |
| 14 | Lab 5 — Bayesian Linear Regression | Skills lab | `Module4/lab5/` |

Labs 1–2 sit in `Module1/` and labs 3–5 in `Module4/` — the folder does not follow from the lab number. Week 13 has no lab.

Each `lab<N>/` holds `lab-activity.tex`/`.pdf` (the printed in-lab worksheet), `lab-starter.R` (student skeleton), and `lab<N>_plan.md` (the run sheet). Project folders use `project-work.tex`; part 1 of each project also carries the outline and rubric.

## Deliverables

**Skills labs** — two submissions, both graded by completion:

| When | What |
| --- | --- |
| Before lab | Tutorial hash |
| After lab | In-lab worksheet — counts as one deliverable |

Students do **not** submit `lab-starter.R`. It is working code for use during lab, not a collected deliverable.

Deliverables are due Tuesdays at 10:00 PM, set by the `Lab Deliverable` row in [`instructor_inputs/Lab_schedules.md`](instructor_inputs/Lab_schedules.md). Lab 1 is the exception — the preceding Tuesday falls before Classes Begin, so it is due on the lab day itself.

**Projects** — three deliverables each:

1. Project plan — shared Google Doc (`P1_GroupNumber_Outline`, `P2_GroupNumber_Outline`)
2. Project slides
3. Final output — video presentation for Project 1, written writeup for Project 2

## Tutorials

Tutorial *N* pairs with lab *N* and is an R warm-up, not a rehearsal — each uses a different scenario from its lab. Students run the **MA 214 Tutorial Launcher** desktop app; no local R install is needed.

```
.Rmd (learnr)  ->  rendered .html  ->  Docker image  ->  launcher pulls  ->  Shiny on localhost:3838  ->  student generates hash
```

At the bottom of each tutorial, **Submit your work** takes a name and student ID and emits a `learnrhash` string that students paste into Blackboard. Decode it with `learnrhash::decode_obj()`.

Solutions are released on a timer, not by a button. Each lesson's `setup` chunk sets `exercise.reveal_solution = FALSE` and one line:

```r
my_reveal_time <- "2026-09-15 15:00:00"   # America/New_York
```

**These are placeholders — reset them to the real lab dates each term.** Hints stay available throughout. The launcher has a *separate* gate, `available_at` in its `lesson_schedule.json`, which controls when a lesson can be opened at all. Two different clocks.

## Updating tutorial questions

Tutorials live in a separate repo: [`BU-Intro-Stats/MA214-tutorials-materials`](https://github.com/BU-Intro-Stats/MA214-tutorials-materials).

1. Edit the `.Rmd`.
2. Re-render — the container serves the tracked `.html`, not the `.Rmd`:
   ```r
   rmarkdown::render("03-lesson/01-03-lesson.Rmd")
   ```
3. Preview:
   ```bash
   docker run --rm -p 3838:3838 -e RMD_FILE=03-lesson/01-03-lesson.Rmd yongholim/ma214tutorial:latest
   ```
4. Publish, then push the source.

Both pushes matter, and they do different jobs:

| Push | Target | Effect |
| --- | --- | --- |
| `PUBLISH=1 ./build_ma214_docker.sh` | Docker Hub | What students actually pull on next launch |
| `git push` | GitHub | Version control and maintenance — **students see nothing** |

**Gotcha:** the launcher hard-codes `IMAGE = "yongholim/ma214tutorial:latest"`. Publishing under a different `DOCKERHUB_USERNAME` produces an image nobody pulls — either publish as `yongholim`, or update the launcher and reissue the app.

## Updating lab materials

Edit under `Module*/lab*/` or `Module*/project*-part*/`, rebuild the PDF from inside that folder (`pdflatex lab-activity.tex`), then commit and push. If the change alters what a session covers, update the matching entry in [`instructor_inputs/lab_summary.md`](instructor_inputs/lab_summary.md) too — that file feeds the course site, and its `### Lab N:` / `### ProjectN-M:` headings are parsed, so keep the heading text intact.

## Where things live

| Channel | Role |
| --- | --- |
| Blackboard | Everything students touch — materials, hash forms, submissions, grades |
| GitHub | Version control and maintenance |
| Docker Hub | Tutorial delivery to students |
| Course site | Public reference view of the schedule, lectures, labs, and objectives |

Three repositories, all under [`BU-Intro-Stats`](https://github.com/BU-Intro-Stats):

| Repository | Contents |
| --- | --- |
| `MA214-applied-statistics-materials` | This repo — lectures, labs, projects, course site |
| `MA214-tutorials-materials` | The five learnr tutorials and the Docker image |
| `Tutorial-Launcher` | The desktop app students run |
