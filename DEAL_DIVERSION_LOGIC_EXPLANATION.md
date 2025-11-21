# Deal Diversion Logic - Detailed Explanation

This document explains how the pipeline history tracking and diversion exclusion logic works step-by-step.

---

## Overview

The system prevents diverting a deal back to any pipeline it has already been in, at any level of the diversion chain. It does this by:
1. **Tracking pipeline history** - Storing all pipelines a deal has been in
2. **Checking diversion chain** - Examining all deals in the diversion hierarchy
3. **Cascading exclusion** - Excluding pipelines where any deal in the chain has been diverted

---

## Key Components

### 1. Database Columns

- **`source_pipeline_id`**: The initial pipeline where the deal was first created
- **`pipeline_history`**: JSON array of all pipeline IDs the deal has been in: `[1, 2, 3]`
- **`referenced_deal_id`**: Points to the deal from which this deal was diverted
- **`referenced_pipeline_id`**: The original pipeline from which the deal was diverted

### 2. Helper Methods

#### `getPipelineHistory(Deal deal)`
Recursively traverses up the diversion chain to collect all pipelines the deal has been in.

#### `getAllDealsInChain(Deal deal)`
Returns all deals in the diversion chain: [current deal, referenced deal, referenced deal's referenced deal, ...]

#### `getSourcePipeline(Deal deal)`
Traverses up the chain to find the initial/source pipeline.

---

## Step-by-Step Flow

### Example Scenario Setup

Let's trace through this scenario:
- **Deal A** created in **Pipeline 1**
- **Deal B** diverted from Deal A to **Pipeline 2**
- **Deal C** diverted from Deal B to **Pipeline 3**

---

## Part 1: Creating a Diverted Deal

### Step 1.1: Create Deal A (Original Deal)

**Request:** `POST /api/deals` with `pipelineId: 1`

**What Happens:**
```java
// In create() method:
if (request.referencedDealId == null && deal.getPipeline() != null) {
    deal.setSourcePipeline(deal.getPipeline());  // Pipeline 1
    List<Long> initialHistory = new ArrayList<>();
    initialHistory.add(deal.getPipeline().getId());  // [1]
    deal.setPipelineHistory(pipelineHistoryToJson(initialHistory));  // "[1]"
}
```

**Result:**
- Deal A: `source_pipeline_id = 1`, `pipeline_history = "[1]"`

---

### Step 1.2: Divert Deal A → Pipeline 2 (Creates Deal B)

**Request:** `POST /api/deals` with `label: "DIVERT"`, `referencedDealId: Deal A's ID`, `pipelineId: 2`

**What Happens:**

```java
// 1. Get referenced deal (Deal A)
Deal referencedDeal = dealRepository.findById(request.referencedDealId);  // Deal A

// 2. Set referenced deal
deal.setReferencedDeal(referencedDeal);  // Deal B.referencedDeal = Deal A

// 3. Get original pipeline (traverses up chain)
Pipeline originalPipeline = getOriginalPipeline(referencedDeal);
// getOriginalPipeline(Deal A):
//   - Deal A has no referencedPipeline, so returns Deal A's current pipeline
//   - Returns Pipeline 1
deal.setReferencedPipeline(Pipeline 1);  // Deal B.referencedPipeline = Pipeline 1

// 4. Get source pipeline (traverses up chain)
Pipeline sourcePipeline = getSourcePipeline(referencedDeal);
// getSourcePipeline(Deal A):
//   - Deal A has sourcePipeline = Pipeline 1
//   - Returns Pipeline 1
deal.setSourcePipeline(Pipeline 1);  // Deal B.sourcePipeline = Pipeline 1

// 5. Build pipeline history
List<Long> pipelineHistory = getPipelineHistory(referencedDeal);
// getPipelineHistory(Deal A):
//   - Deal A has no referencedDeal, so history = []
//   - Parses Deal A's pipeline_history = "[1]" → [1]
//   - Returns [1]
pipelineHistory = [1]

// 6. Add referenced deal's current pipeline to history
if (referencedDeal.getPipeline() != null) {  // Deal A is in Pipeline 1
    Long currentPipelineId = referencedDeal.getPipeline().getId();  // 1
    if (!pipelineHistory.contains(currentPipelineId)) {  // [1] contains 1? Yes, skip
        // Skip adding
    }
}
// pipelineHistory remains [1]

deal.setPipelineHistory(pipelineHistoryToJson([1]));  // Deal B.pipeline_history = "[1]"
```

**Result:**
- Deal B:
  - `source_pipeline_id = 1`
  - `pipeline_history = "[1]"`
  - `referenced_deal_id = Deal A's ID`
  - `referenced_pipeline_id = 1`
  - `is_diverted = true`

---

### Step 1.3: Divert Deal B → Pipeline 3 (Creates Deal C)

**Request:** `POST /api/deals` with `label: "DIVERT"`, `referencedDealId: Deal B's ID`, `pipelineId: 3`

**What Happens:**

```java
// 1. Get referenced deal (Deal B)
Deal referencedDeal = dealRepository.findById(request.referencedDealId);  // Deal B

// 2. Set referenced deal
deal.setReferencedDeal(referencedDeal);  // Deal C.referencedDeal = Deal B

// 3. Get original pipeline (traverses up chain)
Pipeline originalPipeline = getOriginalPipeline(referencedDeal);
// getOriginalPipeline(Deal B):
//   - Deal B has referencedPipeline = Pipeline 1
//   - Returns Pipeline 1
deal.setReferencedPipeline(Pipeline 1);  // Deal C.referencedPipeline = Pipeline 1

// 4. Get source pipeline (traverses up chain)
Pipeline sourcePipeline = getSourcePipeline(referencedDeal);
// getSourcePipeline(Deal B):
//   - Deal B has sourcePipeline = Pipeline 1
//   - Returns Pipeline 1
deal.setSourcePipeline(Pipeline 1);  // Deal C.sourcePipeline = Pipeline 1

// 5. Build pipeline history
List<Long> pipelineHistory = getPipelineHistory(referencedDeal);
// getPipelineHistory(Deal B):
//   - Deal B has referencedDeal = Deal A, so recursively call getPipelineHistory(Deal A)
//   - getPipelineHistory(Deal A):
//     - Deal A has no referencedDeal, so history = []
//     - Parses Deal A's pipeline_history = "[1]" → [1]
//     - Returns [1]
//   - history = [1] (from Deal A)
//   - Parses Deal B's pipeline_history = "[1]" → [1]
//   - Merges: [1] + [1] = [1] (no duplicates)
//   - Returns [1]
pipelineHistory = [1]

// 6. Add referenced deal's current pipeline to history
if (referencedDeal.getPipeline() != null) {  // Deal B is in Pipeline 2
    Long currentPipelineId = referencedDeal.getPipeline().getId();  // 2
    if (!pipelineHistory.contains(currentPipelineId)) {  // [1] contains 2? No
        pipelineHistory.add(currentPipelineId);  // Add 2
    }
}
// pipelineHistory = [1, 2]

deal.setPipelineHistory(pipelineHistoryToJson([1, 2]));  // Deal C.pipeline_history = "[1, 2]"
```

**Result:**
- Deal C:
  - `source_pipeline_id = 1`
  - `pipeline_history = "[1, 2]"`
  - `referenced_deal_id = Deal B's ID`
  - `referenced_pipeline_id = 1`
  - `is_diverted = true`

---

## Part 2: Getting Available Pipelines for Diversion

### Step 2.1: Check Available Pipelines from Deal A (Pipeline 1)

**Request:** `GET /api/deals/{Deal A ID}/available-pipelines`

**What Happens:**

```java
// 1. Get the deal
Deal deal = dealRepository.findById(dealId);  // Deal A

// 2. Get pipeline history
List<Long> pipelineHistory = getPipelineHistory(deal);
// getPipelineHistory(Deal A):
//   - Deal A has no referencedDeal, so history = []
//   - Parses Deal A's pipeline_history = "[1]" → [1]
//   - Returns [1]
pipelineHistory = [1]

// 3. Add current pipeline to history
if (deal.getPipeline() != null) {  // Deal A is in Pipeline 1
    Long currentPipelineId = deal.getPipeline().getId();  // 1
    if (!pipelineHistory.contains(currentPipelineId)) {  // [1] contains 1? Yes, skip
        // Skip
    }
}
// pipelineHistory = [1]

// 4. Get all active pipelines
List<Pipeline> allPipelines = [Pipeline 1, Pipeline 2, Pipeline 3, Pipeline 4, ...]

// 5. Get all deals in chain
List<Deal> dealsInChain = getAllDealsInChain(deal);
// getAllDealsInChain(Deal A):
//   - current = Deal A
//   - chain = [Deal A]
//   - Deal A has no referencedDeal, so stop
//   - Returns [Deal A]
dealsInChain = [Deal A]

// 6. Filter pipelines
for each pipeline in allPipelines:
    
    // Check Pipeline 1:
    if (pipelineHistory.contains(1)) {  // [1] contains 1? Yes
        EXCLUDE Pipeline 1  // ❌ Excluded (in history)
    }
    
    // Check Pipeline 2:
    if (pipelineHistory.contains(2)) {  // [1] contains 2? No, continue
        // Continue
    }
    // Check if Deal A has been diverted to Pipeline 2
    if (existsByReferencedDealAndPipeline(Deal A, Pipeline 2)) {
        // Check: Is there a deal with referencedDeal = Deal A AND pipeline = Pipeline 2?
        // Yes! Deal B exists
        EXCLUDE Pipeline 2  // ❌ Excluded (already diverted there)
    }
    // Check if deals diverted FROM Deal A have been diverted to Pipeline 2
    List<Deal> divertedDeals = findByReferencedDeal(Deal A);  // [Deal B]
    for (Deal divertedDeal : divertedDeals) {  // Deal B
        if (existsByReferencedDealAndPipeline(Deal B, Pipeline 2)) {
            // Check: Is there a deal with referencedDeal = Deal B AND pipeline = Pipeline 2?
            // No, Deal B is IN Pipeline 2, not diverted FROM Pipeline 2
            // Continue
        }
    }
    INCLUDE Pipeline 2  // ✅ Available
    
    // Check Pipeline 3:
    if (pipelineHistory.contains(3)) {  // [1] contains 3? No, continue
        // Continue
    }
    // Check if Deal A has been diverted to Pipeline 3
    if (existsByReferencedDealAndPipeline(Deal A, Pipeline 3)) {
        // Check: Is there a deal with referencedDeal = Deal A AND pipeline = Pipeline 3?
        // No, Deal C has referencedDeal = Deal B, not Deal A
        // Continue
    }
    // Check if deals diverted FROM Deal A have been diverted to Pipeline 3
    List<Deal> divertedDeals = findByReferencedDeal(Deal A);  // [Deal B]
    for (Deal divertedDeal : divertedDeals) {  // Deal B
        if (existsByReferencedDealAndPipeline(Deal B, Pipeline 3)) {
            // Check: Is there a deal with referencedDeal = Deal B AND pipeline = Pipeline 3?
            // Yes! Deal C exists
            EXCLUDE Pipeline 3  // ❌ Excluded (Deal B was diverted to Pipeline 3)
        }
    }
    
    // Check Pipeline 4:
    // ... similar checks ...
    INCLUDE Pipeline 4  // ✅ Available
```

**Result:** Available pipelines = [Pipeline 2, Pipeline 4, Pipeline 5, ...]
- ❌ Pipeline 1 (in history)
- ❌ Pipeline 2 (already diverted there - Deal B exists)
- ❌ Pipeline 3 (Deal B was diverted to Pipeline 3 - Deal C exists)
- ✅ Pipeline 4, 5, ... (available)

---

### Step 2.2: Check Available Pipelines from Deal B (Pipeline 2)

**Request:** `GET /api/deals/{Deal B ID}/available-pipelines`

**What Happens:**

```java
// 1. Get the deal
Deal deal = dealRepository.findById(dealId);  // Deal B

// 2. Get pipeline history
List<Long> pipelineHistory = getPipelineHistory(deal);
// getPipelineHistory(Deal B):
//   - Deal B has referencedDeal = Deal A, so recursively call getPipelineHistory(Deal A)
//   - Returns [1] (from Deal A)
//   - Parses Deal B's pipeline_history = "[1]" → [1]
//   - Merges: [1] + [1] = [1]
//   - Returns [1]
pipelineHistory = [1]

// 3. Add current pipeline to history
if (deal.getPipeline() != null) {  // Deal B is in Pipeline 2
    Long currentPipelineId = deal.getPipeline().getId();  // 2
    if (!pipelineHistory.contains(currentPipelineId)) {  // [1] contains 2? No
        pipelineHistory.add(currentPipelineId);  // Add 2
    }
}
// pipelineHistory = [1, 2]

// 4. Get all deals in chain
List<Deal> dealsInChain = getAllDealsInChain(deal);
// getAllDealsInChain(Deal B):
//   - current = Deal B
//   - chain = [Deal B]
//   - Deal B has referencedDeal = Deal A, so current = Deal A
//   - chain = [Deal B, Deal A]
//   - Deal A has no referencedDeal, so stop
//   - Returns [Deal B, Deal A]
dealsInChain = [Deal B, Deal A]

// 5. Filter pipelines
for each pipeline in allPipelines:
    
    // Check Pipeline 1:
    if (pipelineHistory.contains(1)) {  // [1, 2] contains 1? Yes
        EXCLUDE Pipeline 1  // ❌ Excluded (in history)
    }
    
    // Check Pipeline 2:
    if (pipelineHistory.contains(2)) {  // [1, 2] contains 2? Yes
        EXCLUDE Pipeline 2  // ❌ Excluded (in history - current pipeline)
    }
    
    // Check Pipeline 3:
    if (pipelineHistory.contains(3)) {  // [1, 2] contains 3? No, continue
        // Continue
    }
    // Check if Deal B has been diverted to Pipeline 3
    if (existsByReferencedDealAndPipeline(Deal B, Pipeline 3)) {
        // Check: Is there a deal with referencedDeal = Deal B AND pipeline = Pipeline 3?
        // Yes! Deal C exists
        EXCLUDE Pipeline 3  // ❌ Excluded (already diverted there)
    }
    // Check if Deal A has been diverted to Pipeline 3
    if (existsByReferencedDealAndPipeline(Deal A, Pipeline 3)) {
        // No, Deal C has referencedDeal = Deal B, not Deal A
        // Continue
    }
    // Check if deals diverted FROM Deal B have been diverted to Pipeline 3
    List<Deal> divertedDeals = findByReferencedDeal(Deal B);  // [Deal C]
    for (Deal divertedDeal : divertedDeals) {  // Deal C
        if (existsByReferencedDealAndPipeline(Deal C, Pipeline 3)) {
            // Check: Is there a deal with referencedDeal = Deal C AND pipeline = Pipeline 3?
            // No, Deal C is IN Pipeline 3, not diverted FROM Pipeline 3
            // Continue
        }
    }
    
    // Check Pipeline 4:
    // ... similar checks ...
    INCLUDE Pipeline 4  // ✅ Available
```

**Result:** Available pipelines = [Pipeline 4, Pipeline 5, ...]
- ❌ Pipeline 1 (in history)
- ❌ Pipeline 2 (in history - current pipeline)
- ❌ Pipeline 3 (already diverted there - Deal C exists)
- ✅ Pipeline 4, 5, ... (available)

---

### Step 2.3: Check Available Pipelines from Deal C (Pipeline 3)

**Request:** `GET /api/deals/{Deal C ID}/available-pipelines`

**What Happens:**

```java
// 1. Get the deal
Deal deal = dealRepository.findById(dealId);  // Deal C

// 2. Get pipeline history
List<Long> pipelineHistory = getPipelineHistory(deal);
// getPipelineHistory(Deal C):
//   - Deal C has referencedDeal = Deal B, so recursively call getPipelineHistory(Deal B)
//   - getPipelineHistory(Deal B):
//     - Deal B has referencedDeal = Deal A, so recursively call getPipelineHistory(Deal A)
//     - Returns [1] (from Deal A)
//     - Parses Deal B's pipeline_history = "[1]" → [1]
//     - Merges: [1] + [1] = [1]
//     - Returns [1]
//   - history = [1] (from Deal B)
//   - Parses Deal C's pipeline_history = "[1, 2]" → [1, 2]
//   - Merges: [1] + [1, 2] = [1, 2] (no duplicates)
//   - Returns [1, 2]
pipelineHistory = [1, 2]

// 3. Add current pipeline to history
if (deal.getPipeline() != null) {  // Deal C is in Pipeline 3
    Long currentPipelineId = deal.getPipeline().getId();  // 3
    if (!pipelineHistory.contains(currentPipelineId)) {  // [1, 2] contains 3? No
        pipelineHistory.add(currentPipelineId);  // Add 3
    }
}
// pipelineHistory = [1, 2, 3]

// 4. Filter pipelines
for each pipeline in allPipelines:
    
    // Check Pipeline 1:
    if (pipelineHistory.contains(1)) {  // [1, 2, 3] contains 1? Yes
        EXCLUDE Pipeline 1  // ❌ Excluded (in history)
    }
    
    // Check Pipeline 2:
    if (pipelineHistory.contains(2)) {  // [1, 2, 3] contains 2? Yes
        EXCLUDE Pipeline 2  // ❌ Excluded (in history)
    }
    
    // Check Pipeline 3:
    if (pipelineHistory.contains(3)) {  // [1, 2, 3] contains 3? Yes
        EXCLUDE Pipeline 3  // ❌ Excluded (in history - current pipeline)
    }
    
    // Check Pipeline 4:
    // ... similar checks ...
    INCLUDE Pipeline 4  // ✅ Available
```

**Result:** Available pipelines = [Pipeline 4, Pipeline 5, ...]
- ❌ Pipeline 1 (in history)
- ❌ Pipeline 2 (in history)
- ❌ Pipeline 3 (in history - current pipeline)
- ✅ Pipeline 4, 5, ... (available)

---

## Key Logic Points

### 1. Pipeline History Building

When creating a diverted deal:
- Gets history from the referenced deal (recursively traverses up the chain)
- Adds the referenced deal's current pipeline to the history
- Stores the complete history in the new diverted deal

**Example:**
- Deal A: history = `[1]`
- Deal B (diverted from A): history = `[1]` (from A) + Pipeline 1 (A's current) = `[1]`
- Deal C (diverted from B): history = `[1]` (from B, which got from A) + Pipeline 2 (B's current) = `[1, 2]`

### 2. Exclusion Logic (Three-Level Check)

When getting available pipelines, the system checks:

**Level 1: Pipeline History**
- Excludes all pipelines in the deal's history
- Prevents going back to any previous pipeline

**Level 2: Direct Diversions**
- Checks if any deal in the chain has been directly diverted to the pipeline
- Uses `existsByReferencedDealAndPipeline(dealInChain, pipeline)`

**Level 3: Cascading Diversions**
- Checks if any deal that was diverted FROM deals in the chain has been diverted to the pipeline
- This is the key fix: When checking from Pipeline1, it finds Deal B (diverted from Pipeline1), then checks if Deal B has been diverted to Pipeline3 (yes, Deal C exists)

### 3. Chain Traversal

The `getAllDealsInChain()` method builds the complete chain:
- Starts with the current deal
- Traverses up via `referencedDeal` until it reaches the original deal
- Returns: [current deal, referenced deal, referenced deal's referenced deal, ..., original deal]

**Example for Deal C:**
- Chain = [Deal C, Deal B, Deal A]

---

## Why It Works

### Scenario: Pipeline1 → Pipeline2 → Pipeline3

**When checking from Pipeline1 (Deal A):**
1. History check: Pipeline1 is in history → ❌ Excluded
2. Direct diversion: Deal A diverted to Pipeline2? Yes (Deal B) → ❌ Pipeline2 excluded
3. Cascading check: 
   - Finds Deal B (diverted from Deal A)
   - Checks if Deal B diverted to Pipeline3? Yes (Deal C) → ❌ Pipeline3 excluded

**When checking from Pipeline2 (Deal B):**
1. History check: Pipeline1 and Pipeline2 in history → ❌ Both excluded
2. Direct diversion: Deal B diverted to Pipeline3? Yes (Deal C) → ❌ Pipeline3 excluded

**When checking from Pipeline3 (Deal C):**
1. History check: Pipeline1, Pipeline2, and Pipeline3 in history → ❌ All excluded

---

## Summary

The system works by:
1. **Tracking complete history** - Every diverted deal inherits and extends the pipeline history
2. **Three-level exclusion** - History + Direct diversions + Cascading diversions
3. **Chain traversal** - Examines the entire diversion hierarchy, not just the current deal
4. **Cascading prevention** - Prevents diverting to pipelines where any deal in the chain (or deals diverted from them) has already been diverted

This ensures that once a deal has been in a pipeline, it can never be diverted back to that pipeline, regardless of how many times it's been diverted.

