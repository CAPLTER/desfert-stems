# Project Guidelines

## Code Style
- Prefer small, purpose-specific helper scripts with names following the existing pattern (for example, helper_read_data.R, helper_safe_transaction.R).
- Preserve existing R style in this repository: explicit package qualification (dplyr::, DBI::, purrr::), pipeline-based transforms, and step-wise comments for workflow scripts.
- In R code, always namespace function calls (for example, dplyr::mutate, purrr::map).
- In R code, prefer purrr approaches over explicit for-loops unless a loop is clearly safer or more readable for the specific case.
- Use roxygen-style comments for function and script documentation in R files.
- For SQL embedded in R, keep statements readable and multi-line; avoid changing query semantics unless requested.
- In SQL, use partial table aliasing for column references (for example, stems.stem_id) and avoid schema-qualified column references (for example, urbancndep.stems.stem_id).
- Avoid introducing broad refactors in workflow files; make the smallest safe change needed for the task.

## Architecture
- This project is a data collection and processing workflow for DesFert stems data, centered on KoBo exports and PostgreSQL updates.
- Typical data flow:
  1. Process KoBo export in kobo_workflow.R.
  2. Upload and merge data via populate_database.qmd using temporary staging tables in stems_temp.
  3. Apply or validate schema/data migrations with urbancndep_comment_migration.sql and urbancndep_comment_migration_verify.sql.
- Keep historical archive workflows in archive/ unchanged unless the task explicitly targets them.

## Build and Test
- There is no single package build/test harness. Validate by running the relevant script or SQL for the changed component.
- Common execution targets:
  - kobo_workflow.R for KoBo export processing.
  - populate_database.qmd for DB upload logic.
  - urbancndep_comment_migration.sql for migration changes.
  - urbancndep_comment_migration_verify.sql for post-migration verification.
- For database work, prefer explicit connection parameters in examples/commands (host, user, db) to avoid running against the wrong database.

## Conventions
- Run data-processing steps sequentially when files indicate ordered workflow.
- Use safe transaction semantics for mutating DB steps in R when row-count congruence is expected (see helper_safe_transaction.R).
- Preserve key join/date anchors used by the current workflow:
  - stems.pre_date for pre-measurement context.
  - stems.post_date for post-measurement context.
- Treat note fields as sensitive text data: preserve normalization/cleanup behavior and avoid tokenization-style transformations unless explicitly requested.

## References
- Repository overview and workflow context: README.md
- Upload pipeline details: populate_database.qmd
- KoBo extraction workflow: kobo_workflow.R
- Migration design notes: migration_runbook.md
