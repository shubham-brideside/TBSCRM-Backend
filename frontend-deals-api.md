# Deals API Reference (Frontend)

All deal endpoints sit under `POST /api/deals` etc. Use these notes to wire the CRM UI.

---

## Create Deal

- **Method / URL:** `POST /api/deals`
- **Body (`DealDtos.CreateRequest`):**
  ```json
  {
    "name": "Wedding Package - Sharma",
    "value": 125000,
    "personId": 101,
    "pipelineId": 8,
    "stageId": 37,
    "sourceId": 4,
    "organizationId": 12,
    "categoryId": 3,
    "eventType": "Mehendi",
    "status": "IN_PROGRESS",
    "commissionAmount": 7500,
    "venue": "The Taj Palace",
    "phoneNumber": "+91 98765 43210",
    "finalThankYouSent": false,
    "eventDateAsked": true,
    "contactNumberAsked": true,
    "venueAsked": false,
    "eventDate": "2025-12-19",
    "label": "DIRECT",
    "source": "Direct",
    "subSource": "Instagram",
    "referencedDealId": null
  }
  ```
  - `name` (required, text).  
  - `value` (optional, decimal). Defaults to `0` if omitted.  
  - `personId`, `pipelineId`, `stageId`, `sourceId`, `organizationId`, `categoryId` are optional but IDs must exist when provided—backend returns 404 otherwise.  
  - `status` (optional) — enum `IN_PROGRESS | WON | LOST`; default is `IN_PROGRESS`.  
  - `commissionAmount` (optional) — overrides auto commission calculation (otherwise derived from selected source).  
  - `eventDate` expects ISO `yyyy-MM-dd`. Boolean flags default to null when omitted.
  - `label` (optional) — enum: `DIRECT`, `DIVERT`, `DESTINATION`, `PARTY MAKEUP`, `PRE WEDDING`, `BRIDAL MAKEUP`. Accepts both formats (e.g., "PARTY MAKEUP" or "PARTY_MAKEUP", "Bridal Makeup" or "BRIDAL_MAKEUP").
  - `source` (optional) — enum: `Direct`, `Divert`, `Reference`, `Planner`. Case-insensitive (e.g., "direct", "Direct", "DIRECT" all work).
  - `subSource` (optional) — enum: `Instagram`, `Whatsapp`, `Landing Page`, `Email`. **Only valid when `source` is `"Direct"`**. Case-insensitive. If `source` is not `"Direct"`, `subSource` will be ignored or cleared.
  - `referencedDealId` (optional) — ID of the original deal when creating a diverted deal. **Required when `label` is `DIVERT`**.

- **Response (200 OK):** `DealResponse`
  ```json
  {
    "id": 215,
    "name": "Wedding Package - Sharma",
    "value": 125000,
    "personId": 101,
    "pipelineId": 8,
    "stageId": 37,
    "sourceId": 4,
    "organizationId": 12,
    "categoryId": 3,
    "eventType": "Mehendi",
    "status": "IN_PROGRESS",
    "commissionAmount": 7500,
    "createdAt": "2025-11-12T07:05:41.114Z",
    "venue": "The Taj Palace",
    "phoneNumber": "+91 98765 43210",
    "finalThankYouSent": false,
    "eventDateAsked": true,
    "contactNumberAsked": true,
    "venueAsked": false,
    "eventDate": "2025-12-19",
    "label": "DIRECT",
    "source": "Direct",
    "subSource": "Instagram",
    "isDiverted": false,
    "referencedDealId": null,
    "referencedPipelineId": null
  }
  ```

---

## Fetch Deals

| Endpoint | Description | Notes |
| --- | --- | --- |
| `GET /api/deals` | List all deals | Returns array of `DealResponse` with optional sorting. Supports `sort` query parameter. Default: `nextActivity,asc`. |
| `GET /api/deals/{id}` | Fetch single deal | 404 if missing. |
| `GET /api/deals/won` | Deals with status `WON` | |
| `GET /api/deals/lost` | Deals with status `LOST` | |
| `GET /api/deals/inprogress` | Deals with status `IN_PROGRESS` | |
| `GET /api/deals/person/{personId}` | Deals linked to a person | Person must exist. |
| `GET /api/deals/organization/{organizationId}` | Deals for an organization | Organization must exist. |
| `GET /api/deals/category/{categoryId}` | Deals for a category | Category must exist. |

### Sorting Deals

The `GET /api/deals` endpoint supports sorting via the `sort` query parameter.

**Query Parameter:**
- `sort` (string, optional): Sort field and direction in the format `field,direction`
  - Format: `{field},{direction}`
  - Direction: `asc` (ascending) or `desc` (descending)
  - Default: `nextActivity,asc` (if not specified)

**Supported Sort Fields:**

1. **`nextActivity`** (default)
   - Sort by the date/time of the next upcoming activity (activities that are not done)
   - If no next activity exists, deals appear at the end (or beginning, depending on direction)
   - Field type: DateTime

2. **`name`** (aliases: `dealTitle`)
   - Sort by deal name/title alphabetically
   - Field type: String

3. **`value`** (aliases: `dealValue`)
   - Sort by deal value (monetary amount)
   - Field type: Number

4. **`personName`** (aliases: `linkedPerson`)
   - Sort by the name of the linked person
   - Field type: String (person's name)

5. **`organizationName`** (aliases: `linkedOrganization`)
   - Sort by the name of the linked organization
   - Field type: String (organization's name)

6. **`eventDate`** (aliases: `expectedCloseDate`)
   - Sort by expected close date / event date
   - Field type: Date

7. **`createdAt`** (aliases: `dealCreated`)
   - Sort by deal creation date/time
   - Field type: DateTime

8. **`updatedAt`** (aliases: `dealUpdateTime`)
   - Sort by deal last update date/time
   - Field type: DateTime

9. **`completedActivitiesCount`** (aliases: `doneActivities`)
   - Sort by count of completed/done activities
   - Field type: Number

10. **`pendingActivitiesCount`** (aliases: `activitiesToDo`)
    - Sort by count of pending/not done activities
    - Field type: Number

11. **`productsCount`** (aliases: `numberOfProducts`)
    - Sort by number of products (currently always returns 0, reserved for future use)
    - Field type: Number

12. **`ownerName`** (aliases: `personOwnerName`)
    - Sort by the owner/manager name (person's owner)
    - Field type: String (owner's name)

**Example Requests:**

```javascript
// Sort by deal update time (descending)
GET /api/deals?sort=updatedAt,desc

// Sort by deal value (ascending)
GET /api/deals?sort=value,asc

// Sort by next activity (ascending - default)
GET /api/deals?sort=nextActivity,asc

// Sort by linked person name (ascending)
GET /api/deals?sort=personName,asc

// Sort by done activities count (descending)
GET /api/deals?sort=completedActivitiesCount,desc

// Using aliases
GET /api/deals?sort=dealTitle,asc
GET /api/deals?sort=dealValue,desc
GET /api/deals?sort=linkedPerson,asc
```

**Error Handling:**

- Invalid sort field → `400 Bad Request` with message: "Invalid sort field: {field}. Supported fields: ..."
- Invalid sort direction → Defaults to `asc` if not `asc` or `desc`
- Missing sort parameter → Defaults to `nextActivity,asc`

**Implementation Notes:**

- **Next Activity Sorting**: Finds the earliest pending activity (not done and not COMPLETED) for each deal and sorts by that date. Deals with no pending activities are sorted to the end (or beginning for descending).

- **Activity Counts**: Counts activities where `done = true` or `status = COMPLETED` for completed activities, and activities where `done = false` and `status != COMPLETED` for pending activities.

- **Related Entity Sorting**: Joins with persons/organizations/users tables and sorts by the name field from the joined table.

- **Null Handling**: Null values are sorted to the end (or beginning for descending) using `nullsLast` comparator.

---

## Update Deal

- **Method / URL:** `PATCH /api/deals/{id}`
- **Description:** Partially update deal details. Only provided fields will be updated. All other fields remain unchanged.
- **Body (`DealDtos.UpdateRequest`):**
  ```json
  {
    "name": "Updated Wedding Package - Sharma",
    "value": 150000,
    "personId": 102,
    "pipelineId": 9,
    "stageId": 38,
    "sourceId": 5,
    "organizationId": 13,
    "categoryId": "PHOTOGRAPHY",
    "eventType": "Wedding",
    "status": "IN_PROGRESS",
    "commissionAmount": 9000,
    "venue": "The Grand Hotel",
    "phoneNumber": "+91 98765 43211",
    "finalThankYouSent": true,
    "eventDateAsked": false,
    "contactNumberAsked": false,
    "venueAsked": true,
    "eventDate": "2025-12-20",
    "label": "DIRECT",
    "source": "Direct",
    "subSource": "Whatsapp"
  }
  ```
  - All fields are **optional** - only include the fields you want to update
  - `name` (optional, text)
  - `value` (optional, decimal)
  - `personId`, `pipelineId`, `stageId`, `sourceId`, `organizationId`, `categoryId` are optional but IDs must exist when provided
  - `categoryId` can be a numeric ID or a string code like "PHOTOGRAPHY", "MAKEUP", "PLANNING_AND_DECOR"
  - `status` (optional) — enum `IN_PROGRESS | WON | LOST`
  - `commissionAmount` (optional) — overrides auto commission calculation
  - `eventDate` expects ISO `yyyy-MM-dd` format
  - `label` (optional) — enum: `DIRECT`, `DIVERT`, `DESTINATION`, `PARTY MAKEUP`, `PRE WEDDING`, `BRIDAL MAKEUP`. Accepts both formats (e.g., "PARTY MAKEUP" or "PARTY_MAKEUP", "Bridal Makeup" or "BRIDAL_MAKEUP")
  - `source` (optional) — enum: `Direct`, `Divert`, `Reference`, `Planner`. Case-insensitive
  - `subSource` (optional) — enum: `Instagram`, `Whatsapp`, `Landing Page`, `Email`. **Only valid when `source` is `"Direct"`**. Case-insensitive. If `source` is not `"Direct"`, `subSource` will be ignored or cleared.
  - **Note:** When updating `label` to `DIVERT`, you cannot set it via the update endpoint. Diverted deals should be created using the create endpoint with `label: "DIVERT"` and `referencedDealId`

- **Response (200 OK):** `DealResponse` (same structure as Create Deal response)
  ```json
  {
    "id": 215,
    "name": "Updated Wedding Package - Sharma",
    "value": 150000,
    "personId": 102,
    "pipelineId": 9,
    "stageId": 38,
    "sourceId": 5,
    "organizationId": 13,
    "categoryId": 3,
    "eventType": "Wedding",
    "status": "IN_PROGRESS",
    "commissionAmount": 9000,
    "createdAt": "2025-11-12T07:05:41.114Z",
    "venue": "The Grand Hotel",
    "phoneNumber": "+91 98765 43211",
    "finalThankYouSent": true,
    "eventDateAsked": false,
    "contactNumberAsked": false,
    "venueAsked": true,
    "eventDate": "2025-12-20",
    "label": "DIRECT",
    "source": "Direct",
    "subSource": "Whatsapp",
    "isDiverted": false,
    "referencedDealId": null,
    "referencedPipelineId": null,
    "sourcePipelineId": null,
    "pipelineHistory": null,
    "isDeleted": false
  }
  ```

- **Example Usage:**
  ```javascript
  // Update only the deal name and value
  const response = await fetch('/api/deals/215', {
    method: 'PATCH',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      name: 'Updated Deal Name',
      value: 200000
    })
  });
  ```

- **Error Responses:**
  - `404 Not Found`: Deal with the given ID does not exist or has been deleted
  - `400 Bad Request`: Invalid field values (e.g., invalid enum values for `label` or `source`)
  - `404 Not Found`: Referenced entity (person, pipeline, stage, etc.) not found

---

## Move Deal to Another Stage

- **Method / URL:** `PUT /api/deals/{dealId}/stage`
- **Body:**
  ```json
  {
    "stageId": 42
  }
  ```
- **Response:** Updated `DealResponse`. Stage must belong to the same pipeline (backend does not currently validate association, so ensure UI only offers compatible stages).

---

## Update Deal Status

- **Method / URL:** `PATCH /api/deals/{dealId}/status`
- **Description:** Update deal status. 
  - **When marking as LOST:** `lostReason` is required.
  - **When marking as WON:** `value` is required if deal doesn't have a value. Commission is automatically calculated but can be edited.
- **Body:**
  ```json
  {
    "status": "WON",
    "value": 125000,
    "commissionAmount": 12500
  }
  ```
  - `status` (required) — Allowed values: `IN_PROGRESS`, `WON`, `LOST`
  - `value` (optional, but required when `status` is `WON` and deal doesn't have a value) — Deal value (decimal). If deal already has a value, this can be used to update it.
  - `commissionAmount` (optional) — Commission amount (decimal). If not provided, will be calculated automatically based on deal source:
    - **10%** of deal value for `Direct`, `Reference`, or `Planner` sources
    - **15%** of deal value for `Divert` source
  - `lostReason` (required when `status` is `LOST`, optional otherwise) — Allowed values:
    - `"Slot not opened"`
    - `"Not Interested"`
    - `"Date postponed"`
    - `"Not Available"`
    - `"Ghosted"`
    - `"Budget"`
    - `"Booked Someone else"`
  
  **Important:** 
  - When `status` is `LOST`, the `lostReason` field is **mandatory**.
  - When `status` is `WON`, the `value` field is **required** if the deal doesn't already have a value. If the deal already has a value, you can optionally update it.
  - Commission is automatically calculated but can be overridden by providing `commissionAmount`.

- **Response:** Updated `DealResponse` including the `lostReason` field (if set) and updated `value` and `commissionAmount` (if provided).

- **Example Requests:**
  ```json
  // Mark as WON with new value (deal doesn't have value)
  {
    "status": "WON",
    "value": 125000
    // commissionAmount will be calculated automatically (10% or 15% based on source)
  }
  
  // Mark as WON with value and custom commission
  {
    "status": "WON",
    "value": 125000,
    "commissionAmount": 15000  // Override default calculation
  }
  
  // Mark as WON updating existing value
  {
    "status": "WON",
    "value": 150000  // Update existing value, commission recalculated
  }
  
  // Mark as LOST (lostReason is required)
  {
    "status": "LOST",
    "lostReason": "Budget"
  }
  
  // Mark as IN_PROGRESS (lostReason is cleared automatically)
  {
    "status": "IN_PROGRESS"
  }
  ```

- **Commission Calculation:**
  - **Direct, Reference, or Planner:** 10% of deal value
  - **Divert:** 15% of deal value
  - If `commissionAmount` is provided, it overrides the calculated value

- **Error Responses:**
  - `400 Bad Request`: `"Deal value is required when marking deal as WON. Please provide a value greater than 0."`
  - `400 Bad Request`: `"lostReason is required when marking deal as LOST. Please select a reason from the list."`
  - `400 Bad Request`: `"Invalid lostReason value: ... Allowed values: Slot not opened, Not Interested, Date postponed, Not Available, Ghosted, Budget, Booked Someone else"`

---

## Deal Diversion

### Get Available Pipelines for Diversion

When diverting a deal, you need to show only pipelines where the deal hasn't been diverted yet. Use this endpoint to get the list of available pipelines.

- **Method / URL:** `GET /api/deals/{dealId}/available-pipelines`
- **Description:** Returns pipelines where the specified deal has not been diverted yet. Use this when showing pipeline options in the diversion flow.
- **Response (200 OK):** Array of `PipelineResponse` objects
  ```json
  [
    {
      "id": 5,
      "name": "Photography Pipeline",
      "category": "PHOTOGRAPHY",
      "teamId": 2,
      "isDeleted": false,
      "createdAt": "2025-11-10T10:00:00Z",
      "updatedAt": "2025-11-10T10:00:00Z"
    },
    {
      "id": 8,
      "name": "Makeup Pipeline",
      "category": "MAKEUP",
      "teamId": 3,
      "isDeleted": false,
      "createdAt": "2025-11-10T10:00:00Z",
      "updatedAt": "2025-11-10T10:00:00Z"
    }
  ]
  ```
- **Notes:**
  - Only returns active (non-deleted) pipelines
  - Excludes pipelines where a diverted deal already exists for the specified deal
  - Excludes the referenced pipeline (the original pipeline from which the deal was diverted, if applicable)
  - If the deal doesn't exist, returns `404 Not Found`

### Creating a Diverted Deal

To create a diverted deal:

1. **Get available pipelines** using `GET /api/deals/{originalDealId}/available-pipelines`
2. **Create the new deal** with:
   - `label`: `"DIVERT"` (required)
   - `referencedDealId`: ID of the original deal (required)
   - `pipelineId`: One of the available pipelines from step 1
   - Other deal fields as needed

**Example Request:**
```json
{
  "name": "Diverted Deal - Photography",
  "value": 0,
  "label": "DIVERT",
  "referencedDealId": 123,
  "pipelineId": 5,
  "stageId": 20,
  "personId": 101,
  "status": "IN_PROGRESS"
}
```

**Note:** The `value` field is automatically set to `0` for diverted deals, regardless of what is sent in the request. You can omit the `value` field or send `0`/`null` - the backend will handle it.

**What happens:**
- The backend automatically sets `isDiverted = true` when `label` is `DIVERT`
- The `referencedDealId` links the new deal to the original deal
- The `referencedPipelineId` is automatically set to the **initial/original pipeline** from which the deal was first diverted
  - If diverting an original deal: uses the original deal's current pipeline
  - If diverting an already-diverted deal: uses the original pipeline from the first diversion (traverses up the chain)
- The original deal's ID is stored in the `referenced_deal_id` column
- The original pipeline ID is stored in the `referenced_pipeline_id` column

**Important:** When a diverted deal is diverted again, the `referencedPipelineId` always points to the initial/original pipeline, not the pipeline of the deal being diverted.

**Response includes:**
```json
{
  "id": 215,
  "name": "Diverted Deal - Photography",
  "isDiverted": true,
  "referencedDealId": 123,
  "referencedPipelineId": 8,
  ...
}
```

---

## Field Reference

- `status`: enum defined in backend (`DealStatus`).  
- `commissionAmount`: when omitted the backend uses the associated source's fixed amount or percentage.  
- `eventDate`: persisted as `LocalDate`; send `yyyy-MM-dd`.  
- `phoneNumber` is separate from the linked person's phone—when a person is attached, the backend copies their current phone into `contactNumber`.  
- `finalThankYouSent`, `eventDateAsked`, `contactNumberAsked`, `venueAsked` are booleans; `null` means "not captured yet".
- `label`: enum with values `DIRECT`, `DIVERT`, `DESTINATION`, `PARTY MAKEUP`, `PRE WEDDING`, `BRIDAL MAKEUP`. Accepts both space-separated and underscore formats (e.g., "PARTY MAKEUP" or "PARTY_MAKEUP", "Bridal Makeup" or "BRIDAL_MAKEUP"). Returns display format with spaces.
- `source`: enum with values `Direct`, `Divert`, `Reference`, `Planner`, `TBS`. Case-insensitive input (e.g., "direct", "Direct", "DIRECT" all accepted). Returns properly capitalized display format.
- `subSource`: enum with values `Instagram`, `Whatsapp`, `Landing Page`, `Email`. **Only valid when `source` is `"Direct"`**. Case-insensitive input. Returns properly capitalized display format. Automatically cleared when `source` is not `"Direct"`.
- `lostReason`: string indicating why a deal was marked as LOST. Only present when `status` is `LOST`. Values: `"Slot not opened"`, `"Not Interested"`, `"Date postponed"`, `"Not Available"`, `"Ghosted"`, `"Budget"`, `"Booked Someone else"`. Automatically cleared when status changes to `IN_PROGRESS` or `WON`.
- `isDiverted`: boolean indicating if this deal was diverted from another deal. Automatically set to `true` when `label` is `DIVERT`.
- `referencedDealId`: ID of the original deal if this is a diverted deal. `null` for non-diverted deals. Required when creating a diverted deal.
- `referencedPipelineId`: ID of the **initial/original pipeline** from which the deal was first diverted. Automatically set when creating a diverted deal. Always points to the original pipeline, even when diverting a diverted deal. Used to prevent diverting back to the original pipeline.

---

## Dropdown Options

### Label Options
The following label values are available for the deal label dropdown:
- `DIRECT`
- `DIVERT`
- `DESTINATION`
- `PARTY MAKEUP` (can also be sent as "PARTY_MAKEUP")
- `PRE WEDDING` (can also be sent as "PRE_WEDDING")
- `BRIDAL MAKEUP` (can also be sent as "BRIDAL_MAKEUP" or "Bridal Makeup")

### Source Options
The following source values are available for the deal source dropdown:
- `Direct` - Direct leads/customers
- `Divert` - Diverted deals
- `Reference` - Referred by someone
- `Planner` - From event planners
- `TBS` - TBS source

### Sub-Source Options (Only for Direct)
When `source` is set to `"Direct"`, you can optionally specify a sub-source:
- `Instagram` - Lead came from Instagram
- `Whatsapp` - Lead came from WhatsApp
- `Landing Page` - Lead came from landing page
- `Email` - Lead came from email

**Important:** 
- `subSource` is **only valid** when `source` is `"Direct"`
- If `source` is `"Divert"`, `"Reference"`, or `"Planner"`, the `subSource` field will be ignored or cleared
- If you set `source` to `"Direct"` without providing `subSource`, the `subSource` will be `null`

### Lost Reason Options
The following lost reason values are available when marking a deal as LOST:
- `"Slot not opened"`
- `"Not Interested"`
- `"Date postponed"`
- `"Not Available"`
- `"Ghosted"`
- `"Budget"`
- `"Booked Someone else"`

**Note:** `lostReason` is **required** when `status` is `LOST`. It is automatically cleared when the status is changed to `IN_PROGRESS` or `WON`.

---

## Error Scenarios

- Missing required fields → `400 Bad Request`.  
- Referencing non-existent related IDs → `404 Not Found`.  
- Invalid `status` or malformed date strings → `400 Bad Request`.
- Invalid `label` value → `400 Bad Request` with message: "Invalid label value: {value}. Allowed values: DIRECT, DIVERT, DESTINATION, PARTY MAKEUP, PRE WEDDING, BRIDAL MAKEUP".
- Invalid `source` value → `400 Bad Request` with message: "Invalid source value: {value}. Allowed values: Direct, Divert, Reference, Planner, TBS".
- Invalid `subSource` value → `400 Bad Request` with message: "Invalid subSource value: {value}. Allowed values: Instagram, Whatsapp, Landing Page, Email".
- `subSource` provided when `source` is not `"Direct"` → `400 Bad Request` with message: "subSource can only be provided when source is 'Direct'".
- Marking deal as LOST without `lostReason` → `400 Bad Request` with message: "lostReason is required when marking deal as LOST. Please select a reason from the list."
- Invalid `lostReason` value → `400 Bad Request` with message: "Invalid lostReason value: {value}. Allowed values: Slot not opened, Not Interested, Date postponed, Not Available, Ghosted, Budget, Booked Someone else".
- Creating a diverted deal without `referencedDealId` → `400 Bad Request` with message: "referencedDealId is required when label is DIVERT".
- Referenced deal not found → `404 Not Found` with message: "Referenced deal not found with id {id}".
- Getting available pipelines for non-existent deal → `404 Not Found` with message: "Referenced deal not found with id {id}".

---

## UI Implementation Guide

### Marking Deal as WON (Deal Value & Commission Popup)

When a user attempts to mark a deal as WON, you must show a popup/modal to capture or edit the deal value and commission:

1. **User clicks "Mark as Won" button**
   - Check if the deal already has a value:
     - **If deal has no value:** Show popup with empty value field (required)
     - **If deal has value:** Show popup with existing value pre-filled (editable)

2. **Popup should display:**
   - **Deal Value** field (required, numeric)
     - Pre-fill with existing value if deal already has one
     - Show as empty if deal doesn't have a value
   - **Commission Amount** field (editable, numeric)
     - Calculate default commission based on deal source:
       - **10%** of deal value if source is `Direct`, `Reference`, or `Planner`
       - **15%** of deal value if source is `Divert`
     - Allow user to edit the commission amount
     - Update commission automatically when deal value changes

3. **Submit the status update**
   ```javascript
   const response = await fetch(`/api/deals/${dealId}/status`, {
     method: 'PATCH',
     headers: {
       'Content-Type': 'application/json',
       'Authorization': `Bearer ${token}`
     },
     body: JSON.stringify({
       status: 'WON',
       value: dealValue,  // Required if deal doesn't have value
       commissionAmount: commissionAmount  // Optional, will be calculated if not provided
     })
   });
   ```

4. **Handle errors**
   - If `value` is missing or 0, the backend returns `400 Bad Request`
   - Display error message: "Deal value is required when marking this deal as won"

**Example React Component:**
```jsx
function MarkAsWonModal({ deal, isOpen, onClose, onSuccess }) {
  const [dealValue, setDealValue] = useState(deal?.value?.toString() || '');
  const [commissionAmount, setCommissionAmount] = useState('');
  const [loading, setLoading] = useState(false);
  
  // Calculate default commission when value changes
  useEffect(() => {
    if (dealValue && !isNaN(parseFloat(dealValue))) {
      const value = parseFloat(dealValue);
      const dealSource = deal?.source; // "Direct", "Divert", "Reference", "Planner"
      
      let commissionRate = 0.10; // Default 10%
      if (dealSource === 'Divert') {
        commissionRate = 0.15; // 15% for Divert
      }
      
      const calculatedCommission = value * commissionRate;
      setCommissionAmount(calculatedCommission.toFixed(2));
    } else {
      setCommissionAmount('');
    }
  }, [dealValue, deal?.source]);
  
  const handleSubmit = async () => {
    if (!dealValue || parseFloat(dealValue) <= 0) {
      alert('Please enter a valid deal value');
      return;
    }
    
    setLoading(true);
    try {
      const response = await fetch(`/api/deals/${deal.id}/status`, {
        method: 'PATCH',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${token}`
        },
        body: JSON.stringify({
          status: 'WON',
          value: parseFloat(dealValue),
          commissionAmount: commissionAmount ? parseFloat(commissionAmount) : undefined
        })
      });
      
      if (!response.ok) {
        const error = await response.json();
        alert(error.message || 'Failed to mark deal as won');
        return;
      }
      
      onSuccess();
      onClose();
    } catch (error) {
      alert('An error occurred');
    } finally {
      setLoading(false);
    }
  };
  
  if (!isOpen) return null;
  
  return (
    <div className="modal">
      <h2>Mark Deal as Won</h2>
      
      <div>
        <label htmlFor="dealValue">Deal Value: *</label>
        <input
          id="dealValue"
          type="number"
          value={dealValue}
          onChange={(e) => setDealValue(e.target.value)}
          min="0"
          step="0.01"
          required
        />
      </div>
      
      <div>
        <label htmlFor="commission">Commission Amount:</label>
        <input
          id="commission"
          type="number"
          value={commissionAmount}
          onChange={(e) => setCommissionAmount(e.target.value)}
          min="0"
          step="0.01"
        />
        <small>
          Default: {deal?.source === 'Divert' ? '15%' : '10%'} of deal value
        </small>
      </div>
      
      <button
        onClick={handleSubmit}
        disabled={!dealValue || parseFloat(dealValue) <= 0 || loading}
      >
        {loading ? 'Marking...' : 'Mark as Won'}
      </button>
      <button onClick={onClose}>Cancel</button>
    </div>
  );
}
```

### Marking Deal as LOST (Lost Reason Popup)

When a user attempts to mark a deal as LOST, you must show a popup/modal with the lost reason options:

1. **User clicks "Mark as Lost" button**
   - Show a modal/popup with the following options:
     - "Slot not opened"
     - "Not Interested"
     - "Date postponed"
     - "Not Available"
     - "Ghosted"
     - "Budget"
     - "Booked Someone else"

2. **User must select a reason**
   - The "Mark as Lost" button in the popup should be **disabled** until a reason is selected
   - Display the list as radio buttons, dropdown, or clickable list items

3. **Submit the status update**
   ```javascript
   const response = await fetch(`/api/deals/${dealId}/status`, {
     method: 'PATCH',
     headers: {
       'Content-Type': 'application/json',
       'Authorization': `Bearer ${token}`
     },
     body: JSON.stringify({
       status: 'LOST',
       lostReason: selectedReason // e.g., "Not Interested"
     })
   });
   ```

4. **Handle errors**
   - If `lostReason` is missing, the backend returns `400 Bad Request`
   - Display error message to user: "Please select a reason for marking this deal as lost"

**Example React Component:**
```jsx
function MarkAsLostModal({ dealId, isOpen, onClose, onSuccess }) {
  const [selectedReason, setSelectedReason] = useState('');
  const [loading, setLoading] = useState(false);
  
  const lostReasons = [
    "Slot not opened",
    "Not Interested",
    "Date postponed",
    "Not Available",
    "Ghosted",
    "Budget",
    "Booked Someone else"
  ];
  
  const handleSubmit = async () => {
    if (!selectedReason) return;
    
    setLoading(true);
    try {
      const response = await fetch(`/api/deals/${dealId}/status`, {
        method: 'PATCH',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${token}`
        },
        body: JSON.stringify({
          status: 'LOST',
          lostReason: selectedReason
        })
      });
      
      if (!response.ok) {
        const error = await response.json();
        alert(error.message || 'Failed to mark deal as lost');
        return;
      }
      
      onSuccess();
      onClose();
    } catch (error) {
      alert('An error occurred');
    } finally {
      setLoading(false);
    }
  };
  
  if (!isOpen) return null;
  
  return (
    <div className="modal">
      <h2>Mark Deal as Lost</h2>
      <p>Please select a reason:</p>
      <div>
        {lostReasons.map(reason => (
          <label key={reason}>
            <input
              type="radio"
              name="lostReason"
              value={reason}
              checked={selectedReason === reason}
              onChange={(e) => setSelectedReason(e.target.value)}
            />
            {reason}
          </label>
        ))}
      </div>
      <button
        onClick={handleSubmit}
        disabled={!selectedReason || loading}
      >
        {loading ? 'Marking...' : 'Mark as Lost'}
      </button>
      <button onClick={onClose}>Cancel</button>
    </div>
  );
}
```

### Deal Diversion Flow

1. **User clicks "Divert Deal" on an existing deal**
   - Get the deal ID (e.g., `dealId = 123`)

2. **Fetch available pipelines**
   ```
   GET /api/deals/123/available-pipelines
   ```
   - Show these pipelines in a dropdown
   - If empty array, show message: "This deal has been diverted to all available pipelines"

3. **User selects pipeline and fills deal form**
   - Set `label: "DIVERT"`
   - Set `referencedDealId: 123` (the original deal ID)
   - Set `pipelineId` to the selected pipeline
   - Fill other required fields

4. **Submit the diverted deal**
   ```
   POST /api/deals
   ```
   - Backend automatically sets `isDiverted: true`
   - The new deal is linked to the original via `referencedDealId`

5. **Display diverted deals**
   - Check `isDiverted` field to show diversion indicator
   - Use `referencedDealId` to link back to original deal
   - Filter or group deals by diversion status if needed

### Notes

- A deal can be diverted to multiple pipelines, but only once per pipeline
- The original deal remains unchanged when diverted
- Diverted deals are independent deals with their own stages, status, etc.
- Use `GET /api/deals/{dealId}/available-pipelines` to prevent showing pipelines where the deal is already diverted
- The endpoint automatically excludes the referenced pipeline (original pipeline from which the deal was diverted) from the available options
- If a deal is already diverted, clicking "Divert Deal" will show available pipelines excluding the original pipeline stored in `referencedPipelineId`

---

Hook these endpoints into the deals UI for creating, listing, filtering by status, changing stages/status, and handling deal diversion. Let me know if you need pagination guidance or additional filters. 

