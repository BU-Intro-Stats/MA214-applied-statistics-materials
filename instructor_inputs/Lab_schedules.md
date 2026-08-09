<!--
Instructor notes:
- Everything about lab and deadline dates is set in this file. No Python edits needed.
- Weekday and times set the regular pattern for every session.
- Weeks After sets the gap between a session and a deadline:
    0 = the session's own week, 1 = the following week.
  Labs meet Wednesday and deadlines fall Tuesday, so 0 lands the day BEFORE
  the lab and 1 lands six days after it.
- That is why the deadlines differ: the tutorial hash is due the day before its
  own lab, the worksheet at the end of the lab day, and project deliverables the
  Tuesday of the following week.
- Lab Deliverable covers the five skills labs; Project Deliverable covers the
  eight project parts. They are separate rows so changing one leaves the other
  alone.
- Use Session Overrides below to pin an exact date for one session. Anything left
  blank keeps deriving from the pattern, so only fill in what is wrong.
- After editing, run: python generate_schedule_table.py
-->

## Lab Meeting Pattern

The course has multiple lab sections. Update the day and times here when the
section schedule is finalized.

| Event Type | Weekday | Start Time | End Time | Weeks After |
| --- | --- | --- | --- | --- |
| Lab / Project | Wednesday |  |  |  |
| Tutorial Hash | Tuesday |  | 10:00PM | 0 |
| Lab Deliverable | Wednesday |  | 10:00PM | 0 |
| Project Deliverable | Tuesday |  | 10:00PM | 1 |

## Session Overrides

One row per session that needs a date the pattern does not produce — a holiday
shift, a make-up week, or a deadline you want moved. Leave a cell blank to keep
deriving that date automatically. Delete a row once it is no longer needed.

Session names match the headings in `lab_summary.md`: `Lab 1` through `Lab 5`,
and `Project1-1` through `Project2-4`. Dates can be written as `2026-09-08`,
`9/8/2026`, or `9/8`.

Lab 1 is the standing exception: its tutorial cannot be due before the first lab
of the term, so both its hash and its worksheet are collected a week later and
can be submitted together.

| Session | Meets | Deliverable Due | Tutorial Due |
| --- | --- | --- | --- |
| Lab 1 |  | 2026-09-08 | 2026-09-08 |
