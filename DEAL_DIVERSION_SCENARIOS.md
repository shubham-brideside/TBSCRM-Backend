# Deal Diversion - Scenarios Handled

This document explains all the scenarios handled by the pipeline history tracking system for deal diversion.

---

## Database Changes

### New Columns Added to `deals` Table:

1. **`source_pipeline_id`** (BIGINT, nullable, FK to pipelines.id)
   - Stores the initial/source pipeline where the deal was first created
   - Always points to the very first pipeline, regardless of how many times the deal is diverted

2. **`pipeline_history`** (JSON, nullable)
   - Stores a JSON array of all pipeline IDs the deal has been in
   - Format: `[1, 2, 3]` (array of pipeline IDs)
   - Tracks the complete history of pipeline movements

---

## Scenarios Handled

### Scenario 1: Initial Deal Creation
**Action:** Create a new deal in Pipeline 1

**What Happens:**
- `source_pipeline_id` = Pipeline 1
- `pipeline_history` = `[1]`
- `is_diverted` = `false`
- `referenced_deal_id` = `null`
- `referenced_pipeline_id` = `null`

**Result:** Deal is created with Pipeline 1 as the source.

---

### Scenario 2: First Diversion
**Action:** Divert Deal from Pipeline 1 → Pipeline 2

**What Happens:**
- New diverted deal is created
- `source_pipeline_id` = Pipeline 1 (from original deal)
- `pipeline_history` = `[1]` (from original deal) + Pipeline 1 (current pipeline of original) = `[1]`
- `is_diverted` = `true`
- `referenced_deal_id` = Original Deal ID
- `referenced_pipeline_id` = Pipeline 1 (original pipeline)

**Available Pipelines for Further Diversion:**
- ✅ Pipeline 3, Pipeline 4, etc. (any pipeline not in history)
- ❌ Pipeline 1 (already in history - the original pipeline)
- ❌ Pipeline 2 (current pipeline - will be added to history check)

**Result:** Deal can be diverted to any pipeline except Pipeline 1.

---

### Scenario 3: Second Diversion (Pipeline 1 → Pipeline 2 → Pipeline 3)
**Action:** Divert Deal from Pipeline 2 → Pipeline 3

**What Happens:**
- New diverted deal is created
- `source_pipeline_id` = Pipeline 1 (traversed from original deal)
- `pipeline_history` = `[1]` (from Pipeline 2 deal) + Pipeline 2 (current pipeline) = `[1, 2]`
- `is_diverted` = `true`
- `referenced_deal_id` = Pipeline 2 Deal ID
- `referenced_pipeline_id` = Pipeline 1 (original pipeline, traversed up the chain)

**Available Pipelines for Further Diversion:**
- ✅ Pipeline 4, Pipeline 5, etc. (any pipeline not in history)
- ❌ Pipeline 1 (in history - the original pipeline)
- ❌ Pipeline 2 (in history - previous pipeline)
- ❌ Pipeline 3 (current pipeline - will be added to history check)

**Result:** Deal can be diverted to any pipeline except Pipeline 1 and Pipeline 2.

---

### Scenario 4: Third Diversion (Pipeline 1 → Pipeline 2 → Pipeline 3 → Pipeline 4)
**Action:** Divert Deal from Pipeline 3 → Pipeline 4

**What Happens:**
- New diverted deal is created
- `source_pipeline_id` = Pipeline 1 (traversed from original deal)
- `pipeline_history` = `[1, 2]` (from Pipeline 3 deal) + Pipeline 3 (current pipeline) = `[1, 2, 3]`
- `is_diverted` = `true`
- `referenced_deal_id` = Pipeline 3 Deal ID
- `referenced_pipeline_id` = Pipeline 1 (original pipeline, traversed up the chain)

**Available Pipelines for Further Diversion:**
- ✅ Pipeline 5, Pipeline 6, etc. (any pipeline not in history)
- ❌ Pipeline 1 (in history - the original pipeline)
- ❌ Pipeline 2 (in history - previous pipeline)
- ❌ Pipeline 3 (in history - previous pipeline)
- ❌ Pipeline 4 (current pipeline - will be added to history check)

**Result:** Deal can be diverted to any pipeline except Pipeline 1, 2, and 3.

---

### Scenario 5: Trying to Divert Back to Previous Pipeline
**Action:** From Pipeline 3, try to divert back to Pipeline 2

**What Happens:**
- `GET /api/deals/{dealId}/available-pipelines` is called
- System checks `pipeline_history` = `[1, 2]`
- System adds current pipeline (Pipeline 3) to check = `[1, 2, 3]`
- Pipeline 2 is in the history, so it's **excluded** from available pipelines

**Result:** Pipeline 2 is **NOT** shown in the available pipelines dropdown.

---

### Scenario 6: Trying to Divert Back to Original Pipeline
**Action:** From Pipeline 2, try to divert back to Pipeline 1 (original)

**What Happens:**
- `GET /api/deals/{dealId}/available-pipelines` is called
- System checks `pipeline_history` = `[1]`
- System adds current pipeline (Pipeline 2) to check = `[1, 2]`
- Pipeline 1 is in the history, so it's **excluded** from available pipelines

**Result:** Pipeline 1 is **NOT** shown in the available pipelines dropdown.

---

### Scenario 7: Diverting from Original Deal Again
**Action:** From Pipeline 1 (original), try to divert to Pipeline 2 again (after already diverting once)

**What Happens:**
- `GET /api/deals/{originalDealId}/available-pipelines` is called
- System checks if a diverted deal already exists for Pipeline 2
- If `existsByReferencedDealAndPipeline(originalDeal, pipeline2)` returns `true`, Pipeline 2 is excluded
- Pipeline 2 is also in history (if it was diverted before), so it's excluded

**Result:** Pipeline 2 is **NOT** shown in the available pipelines dropdown (already diverted there).

---

## Key Features

### 1. Source Pipeline Tracking
- **Always points to the initial pipeline** where the deal was first created
- Traverses up the diversion chain to find the original source
- Never changes, even after multiple diversions

### 2. Pipeline History Tracking
- **Tracks all pipelines** the deal has been in
- Includes the original pipeline and all subsequent pipelines
- Stored as JSON array: `[1, 2, 3]`
- Automatically updated when creating diverted deals

### 3. Exclusion Logic
The `getAvailablePipelinesForDiversion` method excludes:
- ✅ Pipelines where a diverted deal already exists (prevents duplicate diversions)
- ✅ All pipelines in the history (prevents diverting back to any previous pipeline)
- ✅ Current pipeline (added to history check)

### 4. History Propagation
When creating a diverted deal:
- Gets pipeline history from the referenced deal
- Adds the referenced deal's current pipeline to the history
- Stores the complete history in the new diverted deal

---

## Example Flow

```
1. Create Deal A in Pipeline 1
   - source_pipeline_id = 1
   - pipeline_history = [1]

2. Divert Deal A → Pipeline 2 (creates Deal B)
   - Deal B: source_pipeline_id = 1, pipeline_history = [1]
   - Available for Deal B: Pipeline 3, 4, 5... (NOT Pipeline 1 or 2)

3. Divert Deal B → Pipeline 3 (creates Deal C)
   - Deal C: source_pipeline_id = 1, pipeline_history = [1, 2]
   - Available for Deal C: Pipeline 4, 5, 6... (NOT Pipeline 1, 2, or 3)

4. Divert Deal C → Pipeline 4 (creates Deal D)
   - Deal D: source_pipeline_id = 1, pipeline_history = [1, 2, 3]
   - Available for Deal D: Pipeline 5, 6, 7... (NOT Pipeline 1, 2, 3, or 4)
```

---

## API Response Fields

When fetching a deal, the response includes:

```json
{
  "id": 215,
  "name": "Diverted Deal",
  "isDiverted": true,
  "referencedDealId": 123,
  "referencedPipelineId": 1,
  "sourcePipelineId": 1,
  "pipelineHistory": "[1, 2, 3]",
  ...
}
```

- `sourcePipelineId`: The initial pipeline (always Pipeline 1 in the example)
- `pipelineHistory`: JSON array of all pipelines the deal has been in
- `referencedPipelineId`: The pipeline from which it was directly diverted (Pipeline 1 in the example, traversed up the chain)

---

## Summary

✅ **Prevents diverting back to original pipeline**
✅ **Prevents diverting back to any previous pipeline**
✅ **Tracks complete pipeline history**
✅ **Maintains source pipeline reference**
✅ **Prevents duplicate diversions to the same pipeline**
✅ **Works for multiple levels of diversion**

All scenarios are handled automatically by the backend. The frontend just needs to call `GET /api/deals/{dealId}/available-pipelines` to get the filtered list.

