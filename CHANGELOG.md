# Changelog

All notable repository changes should be documented in this file.

## 2026-04-14
- Added workspace AI instructions in .github/copilot-instructions.md.
- Added migration artifacts for comment redesign and verification:
  - urbancndep_comment_migration.sql
  - urbancndep_comment_migration_verify.sql
  - migration_runbook.md
- Updated verification script behavior for schema-aware checks and clearer preflight diagnostics.
- Added style requirements to workspace instructions:
  - R: namespaced functions, purrr-first iteration, roxygen-style documentation.
  - SQL: partial table aliasing for column references.
