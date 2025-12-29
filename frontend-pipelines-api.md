## Pipelines & Stages API Cheatsheet

The backend exposes a full set of REST endpoints for managing pipelines and their stages.  All responses share the envelope:

```json
{
  "success": true|false,
  "message": "Human readable message",
  "data": { ... } // optional
}
```

Validation errors return HTTP `400` with `success: false` and a descriptive `message`.

---

### Pipeline endpoints

#### Create pipeline
- **Method / URL:** `POST /api/pipelines`
- **Body (`PipelineRequest`):**
  ```json
  {
    "name": "New Pipeline",             // required, max 255 chars
    "category": "Photography",          // optional, max 255 chars
    "teamId": 42,                       // optional, references internal team identifier
    "organizationId": 12                // optional; organization must exist
  }
  ```
- **Response (201 Created):**
  ```json
  
  {
    "success": true,
    "message": "Pipeline created",
    "data": {
      "id": 7,
      "name": "...",
      "category": "Photography",
      "teamId": 42,
      "team": {
        "id": 42,
        "name": "North America Sales"
      },
      "organization": {
        "id": 12,
        "name": "Brideside Chicago"
      },
      "isDeleted": false,
      "stages": [
        { "id": 201, "name": "Lead In", "order": 0, "...": "..." },
        { "id": 202, "name": "Qualified", "order": 1, "...": "..." },
        { "id": 203, "name": "Contact Made", "order": 2, "...": "..." },
        { "id": 204, "name": "Follow Up", "order": 3, "...": "..." },
        { "id": 205, "name": "Meeting Scheduled", "order": 4, "...": "..." },
        { "id": 206, "name": "Meeting Done", "order": 5, "...": "..." },
        { "id": 207, "name": "Diversion", "order": 6, "...": "..." }
      ]
    }
  }
  ```

#### List pipelines
- **Method / URL:** `GET /api/pipelines?includeStages={boolean}`
- **Query params:**
  - `includeStages` (default `false`): set `true` to embed stage arrays.
- **Response (200 OK):** array of `PipelineResponse` objects sorted by name.

#### List archived pipelines
- **Method / URL:** `GET /api/pipelines/archived?includeStages={boolean}`
- **Query params:**
  - `includeStages` (default `false`): set `true` to embed stage arrays.
- **Response (200 OK):** array of archived `PipelineResponse` objects (where `isDeleted=true`) sorted by name.
- **Frontend notes:**
  - Use this endpoint to display archived pipelines in a separate view or section.
  - Archived pipelines can be restored using the `/api/pipelines/{pipelineId}/unarchive` endpoint.

#### Get single pipeline
- **Method / URL:** `GET /api/pipelines/{pipelineId}?includeStages={boolean}`
- **Response:** single `PipelineResponse`. `includeStages` defaults to `true`.

#### Update pipeline (partial)
- **Method / URL:** `PATCH /api/pipelines/{pipelineId}`
- **Body (`PipelineUpdateRequest`):**
  ```json
  {
    "name": "Renamed Pipeline",
    "category": "Makeup",
    "teamId": 99,
    "organizationId": 15,
    "deleted": false                // optional toggle to restore or archive
  }
  ```
- **Response:** updated `PipelineResponse`.

#### Delete pipeline
- **Method / URL:** `DELETE /api/pipelines/{pipelineId}?hard={boolean}`
- **Behavior:** `hard=true` removes the pipeline and its stages. Without `hard=true`, the pipeline is marked as deleted (`isDeleted=true`) and excluded from list results.
- **Response:** `{ "success": true, "message": "Pipeline deleted" }`

#### Archive pipeline
- **Method / URL:** `PATCH /api/pipelines/{pipelineId}/archive`
- **Behavior:** Soft deletes a pipeline by marking `isDeleted=true`. The pipeline will be excluded from regular list results but can be retrieved via `/api/pipelines/archived`.
- **Response:** updated `PipelineResponse` with `isDeleted: true`.

#### Unarchive pipeline
- **Method / URL:** `PATCH /api/pipelines/{pipelineId}/unarchive?includeStages={boolean}`
- **Query params:**
  - `includeStages` (default `true`): set `false` to exclude stage arrays from response.
- **Behavior:** Restores an archived pipeline by marking `isDeleted=false`. The pipeline will appear in regular list results again.
- **Response:** updated `PipelineResponse` with `isDeleted: false`.
- **Frontend notes:**
  - Use this endpoint to restore archived pipelines from the archived pipelines view.
  - Returns error if the pipeline is not archived.

---

### Stage endpoints

#### List stages
- **Method / URL:** `GET /api/pipelines/{pipelineId}/stages?includeInactive={boolean}`
- **Query params:**
  - `includeInactive` (default `false`): set `true` to include archived (soft-deleted) stages.
- **Response:** array of `StageResponse`.

#### Create stage
- **Method / URL:** `POST /api/pipelines/{pipelineId}/stages`
- **Body (`StageRequest`):**
  ```json
  {
    "name": "Qualified",            // required, max 255 chars
    "order": 1,                     // optional position, defaults to end
    "probability": 50,              // optional (0-100)
    "active": true                  // optional, defaults true
  }
  ```
- **Response (201 Created):** `StageResponse` for the new stage.

#### Update stage (partial)
- **Method / URL:** `PATCH /api/pipelines/{pipelineId}/stages/{stageId}`
- **Body (`StageUpdateRequest`):** same fields as create, all optional.
- **Response:** updated `StageResponse`.

#### Delete stage
- **Method / URL:** `DELETE /api/pipelines/{pipelineId}/stages/{stageId}?hard={boolean}`
- **Behavior:** soft delete by default (`active=false`). `hard=true` removes the record.
- **Response:** `{ "success": true, "message": "Stage deleted" }`

#### Reorder stages
- **Method / URL:** `POST /api/pipelines/{pipelineId}/stages/reorder`
- **Body (`StageOrderRequest`):**
  ```json
  {
    "orderedStageIds": [12, 18, 5, 7]
  }
  ```
  Provide **all** stage IDs for the pipeline in the desired sequence.
- **Response:** updated list of `StageResponse` in new order.

---

### Response DTOs (for reference)

`PipelineResponse`
```json
{
  "id": 7,
  "name": "Enterprise Pipeline",
  "category": "Photography",
  "teamId": 42,
  "team": {
    "id": 42,
    "name": "North America Sales"
  },
  "organization": {
    "id": 12,
    "name": "Brideside Chicago"
  },
  "isDeleted": false,
  "createdAt": "2025-11-07T13:11:32Z",
  "updatedAt": "2025-11-07T13:11:32Z",
  "stages": []
}
```

`StageResponse`
```json
{
  "id": 12,
  "pipelineId": 7,
  "name": "Qualified",
  "order": 0,
  "probability": 40,
  "active": true,
  "createdAt": "2025-11-07T13:11:32Z",
  "updatedAt": "2025-11-07T13:11:32Z"
}
```

---

### Team options (for dropdown)

- **Method / URL:** `GET /api/teams`
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Teams fetched",
    "data": [
      { "id": 42, "name": "North America Sales" },
      { "id": 43, "name": "EMEA Enterprise" }
    ]
  }
  ```
- **Frontend notes:**
  - Use the `name` for the select label and the `id` for `teamId`.
  - Cache locally; teams list rarely changes.

---

### Category options (for dropdown)

- **Method / URL:** `GET /api/pipelines/categories`
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Pipeline categories fetched",
    "data": [
      { "code": "PHOTOGRAPHY", "label": "Photography" },
      { "code": "MAKEUP", "label": "Makeup" },
      { "code": "PLANNING_AND_DECOR", "label": "Planning and Decor" }
    ]
  }
  ```
- **Frontend notes:**
  - Submit the `code` value in `category` when creating/updating pipelines.
  - Use the `label` for display. Values are static but you can request them per form load to keep future-proof.

---

### Integration Tips
1. **Authentication:** Include whatever auth headers your app already uses for other API calls.
2. **Default stages:** New pipelines ship with the default sequence (Lead In → Qualified → Contact Made → Follow Up → Meeting Scheduled → Meeting Done → Diversion). Update or reorder as needed via the stages endpoints.
3. **Soft delete:** Pipelines set `isDeleted=true` when deleted without `hard=true`; listings automatically exclude them.
4. **Archived pipelines:** Use `/api/pipelines/archived` to fetch archived pipelines. Use `/api/pipelines/{pipelineId}/unarchive` to restore an archived pipeline.
5. **Error handling:** read `success` flag and message. For validation errors the message includes the field issue.
6. **Reordering:** after drag-and-drop, post the list of stage IDs in the new order; the backend persists the new `stage_order`.
7. **Hard delete:** Pass `hard=true` only when you intend to permanently remove a pipeline and its stages.
8. **Teams dropdown:** Fetch `/api/teams` once when opening the create/edit form and populate the team selector; send the selected `teamId` in requests.

This sheet should be enough to wire up the pipelines UI. Let me know if you want sample fetch/axios calls as well.

