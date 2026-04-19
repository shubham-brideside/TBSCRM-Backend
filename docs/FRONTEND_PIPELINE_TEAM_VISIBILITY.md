# Frontend notes: pipeline / team visibility (backend change)

This backend update enforces **deal and pipeline visibility** from **team ↔ pipeline** links (and **organization ownership** for category managers). No REST path or JSON schema changes were required for compatibility, but **behavior** changes below affect what each role sees.

## Who sees what (summary)

| Role | Pipelines (e.g. `GET /api/pipelines`) | Deals / counts / revenue / stage totals |
|------|----------------------------------------|----------------------------------------|
| **ADMIN** | All active pipelines | All non-deleted deals (unchanged). |
| **CATEGORY_MANAGER** | All active pipelines whose **organization** is owned by the CM **or anyone in their existing report tree** (same owner model as “accessible organizations”). | Same scope: deals must sit on one of those pipelines **or** (legacy) have **no `pipeline_id`** but `organization_id` in that accessible org set. |
| **SALES** | Pipelines for teams where the user is the **team manager** **or** a **team member**. | Deals whose `pipeline_id` is one of those pipelines. |
| **PRESALES** | Pipelines for teams where the user is a **member** (multiple teams ⇒ union of pipelines). | Same as Sales, member-based. |

## UX / integration implications

1. **Sales users added only as team members** (not as `manager_id` on the team) now receive the correct pipelines and deals for that team. Ensure the UI assigns members to teams when onboarding multi-pipeline setups.

2. **Category managers** no longer depend only on “teams whose manager is a sales report.” They see pipelines tied to **every organization** they already reach via the owner/report model (aligned with organization pickers). If the product expectation was strictly **service category** (e.g. Photography vs Makeup) rather than **org ownership tree**, that would require a separate user field or setting—this release uses the **existing** accessible-organization logic.

3. **Stage totals** (`pipelineId` required): returns **404** if the requested pipeline is outside the user’s scope (same as “hidden” pipeline).

4. **Deal diversion** (`GET` available pipelines for a deal): only pipelines in scope are offered. Unauthorized deal ⇒ **404** (“Deal not found”) to avoid leaking IDs.

5. **Global search**: **Deal** hits use the same pipeline/org scope; **person** search is still **not** narrowed by pipeline in this release (same as before for persons).

6. **Teamless or org-less pipelines**: a **SALES/PRESALES** user with **no team membership** sees **no** pipelines and **no** deals until they are added to a team linked to the pipeline.

## Optional frontend improvements (not mandatory for API correctness)

- After login, refresh pipeline and deal lists when admin changes **team membership**.
- Surface empty states when `GET /pipelines` returns `[]` for sales/presales with missing team assignment.
- Avoid calling **stage totals** or **diversion** endpoints with pipeline IDs not present in the filtered pipeline list.

## Verification checklist

- User in **two teams** ⇒ pipeline list includes both teams’ pipelines; deal board shows deals for **both** pipelines.
- **Admin** ⇒ unchanged full visibility.
- **Category manager** ⇒ pipelines for all orgs under their accessible owners; deals match those pipelines.
