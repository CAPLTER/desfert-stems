#!/usr/bin/env bash
set -euo pipefail

###############################################################################
# Rebuild workflow
#
# Required environment variable:
#   PGUSER=<postgres_user>
#
# Optional environment variables:
#   PGHOST=<host>                  # default: localhost
#   PGDATABASE=<database_name>     # default: caplter
#   DUMP_PATH=<path_to_dump_file>  # default: $HOME/databaseDumps/urbancndep_20260407
#   MIGRATION_PATH=<path_to_sql>   # default: $HOME/localRepos/desfert-stems/migration_202604/urbancndep_comment_migration.sql
#
# Example:
#   PGUSER=srearl ./migration_202604/rebuild.sh
# 
# quarto render populate_database.qmd
# 
###############################################################################

: "${PGUSER:?PGUSER is required. Example: PGUSER=srearl ./migration_202604/rebuild.sh}"
PGHOST="${PGHOST:-localhost}"
PGDATABASE="${PGDATABASE:-caplter}"
DUMP_PATH="${DUMP_PATH:-$HOME/databaseDumps/urbancndep_20260407}"
MIGRATION_PATH="${MIGRATION_PATH:-$HOME/localRepos/desfert-stems/migration_202604/urbancndep_comment_migration.sql}"

echo "[1/3] Drop schema objects (if present)"
psql -h "$PGHOST" -U "$PGUSER" -d "$PGDATABASE" -v ON_ERROR_STOP=1 <<'SQL'
DROP SCHEMA IF EXISTS urbancndep CASCADE;
SQL

echo "[2/3] Restore database dump"
pg_restore -h "$PGHOST" -U "$PGUSER" -d "$PGDATABASE" --no-owner --no-privileges "$DUMP_PATH"

echo "[3/3] Run migration"
psql -h "$PGHOST" -U "$PGUSER" -d "$PGDATABASE" -v ON_ERROR_STOP=1 -f "$MIGRATION_PATH"

echo "[done] rebuild completed"