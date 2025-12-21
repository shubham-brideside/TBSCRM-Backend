# Labels API Integration Guide

## Overview

Labels are now customizable via the database. Users can create, update, and delete labels from the frontend. Labels can be assigned to both **Deals** and **Persons** (many-to-many relationship).

> **Backward Compatibility**: The legacy `label` enum field still works. You can use either the old enum approach or the new `labelIds` approach.

---

## Label API Endpoints

### Base URL: `/api/labels`

### 1. Get All Labels

```http
GET /api/labels
```

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `search` | string | No | Filter labels by name (case-insensitive) |

**Response:**
```json
[
  {
    "id": 1,
    "name": "Direct",
    "color": "#4CAF50",
    "createdAt": "2024-12-17T10:00:00",
    "updatedAt": "2024-12-17T10:00:00"
  },
  {
    "id": 2,
    "name": "Hot Lead",
    "color": "#FF5733",
    "createdAt": "2024-12-17T10:00:00",
    "updatedAt": "2024-12-17T10:00:00"
  }
]
```

### 2. Get Single Label

```http
GET /api/labels/{id}
```

**Response:**
```json
{
  "id": 1,
  "name": "Direct",
  "color": "#4CAF50",
  "createdAt": "2024-12-17T10:00:00",
  "updatedAt": "2024-12-17T10:00:00"
}
```

### 3. Create Label

```http
POST /api/labels
Content-Type: application/json
```

**Request Body:**
```json
{
  "name": "Hot Lead",
  "color": "#FF5733"
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | Yes | Label name (must be unique) |
| `color` | string | No | Hex color code (e.g., `#FF5733`) |

**Response:** `201 Created`
```json
{
  "id": 3,
  "name": "Hot Lead",
  "color": "#FF5733",
  "createdAt": "2024-12-17T10:00:00",
  "updatedAt": "2024-12-17T10:00:00"
}
```

### 4. Update Label

```http
PUT /api/labels/{id}
Content-Type: application/json
```

**Request Body:**
```json
{
  "name": "Very Hot Lead",
  "color": "#FF0000"
}
```

Both fields are optional - only provided fields will be updated.

**Response:** `200 OK`
```json
{
  "id": 3,
  "name": "Very Hot Lead",
  "color": "#FF0000",
  "createdAt": "2024-12-17T10:00:00",
  "updatedAt": "2024-12-17T11:00:00"
}
```

### 5. Delete Label

```http
DELETE /api/labels/{id}
```

**Response:** `204 No Content`

---

## Assigning Labels to Deals

### Create Deal with Labels

```http
POST /api/deals
Content-Type: application/json
```

```json
{
  "name": "Wedding Photography",
  "value": 50000,
  "pipelineId": 1,
  "labelIds": [1, 3, 5]
}
```

### Update Deal Labels

```http
PUT /api/deals/{id}
Content-Type: application/json
```

```json
{
  "labelIds": [2, 4]
}
```

### Deal Response with Labels

```json
{
  "id": 1,
  "name": "Wedding Photography",
  "value": 50000,
  "label": "DIRECT",
  "labels": [
    {
      "id": 1,
      "name": "Direct",
      "color": "#4CAF50"
    },
    {
      "id": 3,
      "name": "Hot Lead",
      "color": "#FF5733"
    }
  ]
}
```

---

## Assigning Labels to Persons

### Create Person with Labels

```http
POST /api/persons
Content-Type: application/json
```

```json
{
  "name": "John Doe",
  "phone": "9876543210",
  "labelIds": [1, 2]
}
```

### Update Person Labels

```http
PUT /api/persons/{id}
Content-Type: application/json
```

```json
{
  "labelIds": [3, 4, 5]
}
```

### Person Response with Labels

```json
{
  "id": 1,
  "name": "John Doe",
  "phone": "9876543210",
  "label": "BRIDAL_MAKEUP",
  "labels": [
    {
      "id": 1,
      "name": "Direct",
      "color": "#4CAF50"
    },
    {
      "id": 2,
      "name": "Bridal Makeup",
      "color": "#F44336"
    }
  ]
}
```

---

## Default Labels (Pre-seeded)

The following labels are created automatically during migration:

| ID | Name | Color |
|----|------|-------|
| 1 | Direct | #4CAF50 |
| 2 | Divert | #FF9800 |
| 3 | Destination | #2196F3 |
| 4 | Party Makeup | #E91E63 |
| 5 | Pre Wedding | #9C27B0 |
| 6 | Bridal Makeup | #F44336 |
| 7 | Engagement | #00BCD4 |
| 8 | Reception | #795548 |
| 9 | Other | #607D8B |

---

## Frontend Implementation Examples

### React: Label Management Component

```jsx
// Fetch all labels
const fetchLabels = async () => {
  const response = await fetch('/api/labels');
  return response.json();
};

// Create new label
const createLabel = async (name, color) => {
  const response = await fetch('/api/labels', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ name, color })
  });
  return response.json();
};

// Delete label
const deleteLabel = async (id) => {
  await fetch(`/api/labels/${id}`, { method: 'DELETE' });
};
```

### React: Multi-Select Label Component

```jsx
import Select from 'react-select';

const LabelMultiSelect = ({ selectedLabelIds, onChange, labels }) => {
  const options = labels.map(label => ({
    value: label.id,
    label: label.name,
    color: label.color
  }));

  const selectedOptions = options.filter(opt => 
    selectedLabelIds.includes(opt.value)
  );

  return (
    <Select
      isMulti
      options={options}
      value={selectedOptions}
      onChange={(selected) => onChange(selected.map(s => s.value))}
      styles={{
        multiValue: (base, { data }) => ({
          ...base,
          backgroundColor: data.color + '20',
          borderLeft: `3px solid ${data.color}`
        })
      }}
    />
  );
};
```

### React: Label Badge Component

```jsx
const LabelBadge = ({ label }) => (
  <span
    style={{
      backgroundColor: label.color + '20',
      color: label.color,
      padding: '2px 8px',
      borderRadius: '12px',
      fontSize: '12px',
      fontWeight: 500,
      border: `1px solid ${label.color}`
    }}
  >
    {label.name}
  </span>
);

// Usage in Deal/Person list
const DealLabels = ({ labels }) => (
  <div style={{ display: 'flex', gap: '4px', flexWrap: 'wrap' }}>
    {labels?.map(label => (
      <LabelBadge key={label.id} label={label} />
    ))}
  </div>
);
```

---

## Error Responses

| Status | Error | Description |
|--------|-------|-------------|
| 400 | Label name is required | `name` field is empty |
| 400 | Label with name 'X' already exists | Duplicate name |
| 404 | Label not found with id: X | Invalid label ID |
| 404 | One or more labels not found | Invalid ID in `labelIds` array |

---

## Migration Notes

1. The legacy `label` enum field (`DIRECT`, `DIVERT`, etc.) still works and is independent of the new `labels` array.
2. You can use both simultaneously or migrate gradually.
3. To fully migrate:
   - Replace enum dropdowns with multi-select using `/api/labels`
   - Use `labelIds` instead of `label` in create/update requests
   - Display `labels` array instead of `label` enum in UI

