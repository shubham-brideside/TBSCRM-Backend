# Planning, Decor & BTS categories — backend changes

Summary of changes that add three business-unit categories (**Planning**, **Decor**, **BTS**) alongside existing **Photography**, **Makeup**, and legacy **Planning and Decor**.

## New API / enum codes

| Code (`TargetCategory` / pipeline options) | Display / DB label (organizations) |
|-------------------------------------------|--------------------------------------|
| `PLANNING` | Planning |
| `DECOR` | Decor |
| `BTS` | BTS |

`PLANNING_AND_DECOR` (*Planning and Decor*) remains for backward compatibility.

## Code updates

- **`TargetCategory`** (`com.brideside.crm.entity`) — Added `PLANNING`, `DECOR`, `BTS`. Removed the standalone `DECOR` alias from `PLANNING_AND_DECOR` so the word “Decor” maps to `DECOR`, not the combined category.
- **`Organization.OrganizationCategory`** — Same three enum constants with matching `getDbValue()` strings.
- **`PipelineDtos.allCategoryOptions()`** — Lists all six options (including `PLANNING_AND_DECOR`).
- **`ActivityService`** — Activity filter mapping includes `PLANNING`, `DECOR`, `BTS`.
- **`CategoryServiceImpl.resolveCategoryName`** — Code → display name map extended for the new codes.
- **`ActivityDTO` / `Activity`** — Schema/comments updated for organization and service category fields.

## Database

- **Reference migration:** `src/main/resources/db/migration/V20260411_01__add_planning_decor_bts_org_and_categories.sql`
  - Extends MySQL `organizations.category` `ENUM` with `'Planning'`, `'Decor'`, `'BTS'`.
  - Inserts into `categories` (`Planning`, `Decor`, `BTS`) when missing.

**Note:** This project does not run Flyway by default; `.sql` files under `db/migration` are **not** executed automatically unless you add Flyway (or run SQL manually).

## Startup bootstrap

- **`CategoryBusinessUnitBootstrap`** (`com.brideside.crm.config`) — On application startup, idempotently:
  - Alters `organizations.category` when the enum does not yet include `BTS` (detection via `information_schema`).
  - Inserts the three `categories` rows if absent.

Restart the app after deploy to apply DB changes without a separate migration tool.

## Manual SQL (optional)

If you prefer to apply changes directly in MySQL (e.g. before deploy), run the statements in:

`V20260411_01__add_planning_decor_bts_org_and_categories.sql`

## Frontend

Update any UI that hardcodes only three categories so it loads options from the pipeline/category APIs (`PHOTOGRAPHY`, `MAKEUP`, `PLANNING`, `DECOR`, `BTS`, `PLANNING_AND_DECOR`).
