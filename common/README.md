# MA213/214 slide template

Shared Beamer style for the lecture decks (`Module*/lecture*/Lecture*.tex`).
Each deck loads it with `\input{../../common/lec_style.tex}`. The look is a low-key
"Calm & Cool" palette on the `metropolis` theme, with `tcolorbox` callout boxes so
students can recognize content types at a glance.

See `template-showcase.tex` for a deck that exercises every element — build it to preview
the design (`pdflatex template-showcase.tex`).

## Two build modes (instructor vs. student handout)

Every deck begins with a two-option block. The active option is the **instructor** build
(solutions shown); comment it and uncomment the second for the **student handout** build
(solutions hidden):

```latex
% Option 1: Slides with solutions
\documentclass[slidestop,compress,mathserif]{beamer}
\newcommand{\soln}[1]{\textit{#1}}
\newcommand{\solnGr}[1]{#1}

% Option 2: Handouts without solutions
%\documentclass[11pt,containsverbatim,handout]{beamer}
%\newcommand{\soln}[1]{}
%\newcommand{\solnGr}[1]{}
```

The `\soln` macro is the master switch: `\solnbox{...}` and `\solnMult{...}` are gated by it,
so everything marked as a solution disappears from the handout build automatically.

## Content boxes

| Command | Color | Use for | Hidden in handout? |
|---|---|---|---|
| `\workedexample[subtitle]{body}` | blue | a one-off worked example | no |
| `\examplehead{name}` | blue | running-example cue at the top of each frame of a multi-slide example | no |
| `\activity[subtitle]{body}` | green | interactive in-class activity | no |
| `\dq{body}` | green | discussion question | no |
| `\solnbox{body}` | purple | a solution / answer | **yes** |
| `\worksheet[subtitle]{body}` | terracotta | focused worksheet work time | no |
| `\formula{title}{body}` or `\formula{body}` | slate | a key formula or definition | no |
| `\caution[subtitle]{body}` | red | a common pitfall / misconception | no |
| `\keyidea[subtitle]{body}` | gold | a key takeaway to remember | no |
| `\begin{rblock}[subtitle] ... \end{rblock}` | gray | R code / console output | no |

Notes:

- The `[subtitle]` argument is optional, e.g. `\activity[3.7]{...}` titles the box "Activity 3.7".
- **Running examples**: use `\examplehead{Airbnb prices}` at the top of each frame in the run
  (lightweight, so it does not overwhelm a multi-slide example). Use `\workedexample{...}` for a
  single self-contained example box. (The command is `\workedexample`, not `\example`, because
  beamer reserves `\example`.)
- **`\solnbox` vs `\soln`**: `\soln{...}` is the bare toggle (italic inline text, no box);
  `\solnbox{...}` is the purple titled box. Both vanish in the handout build.
- **R code requires a fragile frame**: any frame containing an `rblock` must be declared
  `\begin{frame}[fragile]` (a Beamer requirement for verbatim content).

## Inline emphasis

`\hl{...}` (blue highlight — the workhorse), `\green{...}`, `\orange{...}` (terracotta),
`\red{...}`, `\mathhl{...}` (highlighted math). Use `\alert{...}` for a metropolis-style alert.

## Title slide

`\maketitleframe` builds the de-branded title slide from the deck's `\title`, `\subtitle`, and
the eyebrow macro `\coursetag` (default "MA214: Applied Statistics"; `\renewcommand` it per deck
if desired). The big title is the deck's `\title{...}`, so set it to just the lecture name —
`\title[Lecture 7]{Lecture 7}` — not `MA214: Lecture 7`. Attribution is centralized in
`\courseattribution` and rendered automatically (in fine print) — all
decks are released under CC BY-SA, crediting both the instructor and OpenIntro (the basis for the
earlier material). The `\author` and `\institute` fields are not used by the title slide.

## Palette (Calm & Cool)

| Name | Hex | Role |
|---|---|---|
| `ccSlate` | `#2E3742` | chrome: frame titles, structure, section pages |
| `ccBlue` | `#2F6690` | running examples, inline `\hl` |
| `ccGreen` | `#4E8A6B` | activities, discussion |
| `ccPurple` | `#6F5499` | solutions |
| `ccTerra` | `#C2693E` | worksheets |
| `ccGray` | `#5C6B7A` | formulas / definitions |
| `ccRed` | `#B23A48` | cautions / pitfalls |
| `ccGold` | `#9C7A28` | key ideas |
| `ccCode` | `#5F5E5A` | R code blocks |

Each box also has a light fill (`...Bg`). To retune the palette, edit the `\definecolor` block
near the top of `lec_style.tex` — colors propagate to all decks.

## Compatibility

All commands from the original OpenIntro style are preserved (`\dq`, `\hl`, `\formula`, `\pq`,
`\app`, `\solnMult`, `\twocol`, `\Note`, ...), so existing decks compile unchanged and pick up the
new look automatically. Old color names (`oiB`, `hlblue`, etc.) are aliased to the new palette.

Decks compile with **pdflatex** (the theme falls back gracefully without the Fira font).
Requires `tcolorbox`, `fontawesome5`, and `pgfplots` (all in a standard TeX Live install).
