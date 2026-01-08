# Deal Multiple Labels API Documentation

## Overview

Deals now support **multiple label selection**. You can assign multiple labels to a deal during creation or update. The API maintains backward compatibility with the single `labelId` field.

---

## API Changes

### Create Deal

**Endpoint:** `POST /api/deals`

**Request Body:**
```json
{
  "name": "Wedding Photography Package",
  "value": 125000,
  "personId": 101,
  "pipelineId": 8,
  "stageId": 37,
  "labelIds": [1, 2, 3],  // ✅ NEW: Array of label IDs
  "labelId": 1             // ⚠️ DEPRECATED: Still works for backward compatibility
}
```

**Fields:**
- `labelIds` (List<Long>, optional): Array of label IDs from the labels table
- `labelId` (Long, optional): Single label ID (deprecated, use `labelIds` instead)

**Notes:**
- If both `labelIds` and `labelId` are provided, `labelIds` takes precedence
- If `labelId` is provided (without `labelIds`), it will be converted to a single-item array for backward compatibility
- Empty array `[]` will clear all labels
- If neither is provided, no labels will be assigned

---

### Update Deal

**Endpoint:** `PATCH /api/deals/{id}`

**Request Body:**
```json
{
  "name": "Updated Deal Name",
  "labelIds": [2, 4, 5]  // ✅ Update to new set of labels
}
```

**Behavior:**
- If `labelIds` is provided (even if empty `[]`), it will replace all existing labels
- If `labelId` is provided (without `labelIds`), it will replace all labels with a single label
- If neither is provided, existing labels remain unchanged

---

## Response Format

### Deal Response

**Endpoint:** `GET /api/deals/{id}` or `GET /api/deals`

**Response Body:**
```json
{
  "id": 1,
  "name": "Wedding Photography Package",
  "value": 125000,
  "labelIds": [1, 2, 3],  // ✅ NEW: Array of label IDs
  "labels": [              // ✅ NEW: Array of full label objects
    {
      "id": 1,
      "name": "Hot Lead",
      "color": "#FF5733",
      "createdAt": "2024-12-17T10:00:00",
      "updatedAt": "2024-12-17T10:00:00"
    },
    {
      "id": 2,
      "name": "Priority",
      "color": "#33FF57",
      "createdAt": "2024-12-17T10:00:00",
      "updatedAt": "2024-12-17T10:00:00"
    },
    {
      "id": 3,
      "name": "Follow Up",
      "color": "#5733FF",
      "createdAt": "2024-12-17T10:00:00",
      "updatedAt": "2024-12-17T10:00:00"
    }
  ],
  "labelId": 1,           // ⚠️ DEPRECATED: First label ID (for backward compatibility)
  "label": {               // ⚠️ DEPRECATED: First label object (for backward compatibility)
    "id": 1,
    "name": "Hot Lead",
    "color": "#FF5733",
    "createdAt": "2024-12-17T10:00:00",
    "updatedAt": "2024-12-17T10:00:00"
  },
  "labelString": null,     // Legacy enum string (if using old enum labels)
  // ... other fields
}
```

**Response Fields:**
- `labelIds` (List<Long>): Array of all label IDs assigned to the deal
- `labels` (List<LabelDtos.Response>): Array of full label objects with all details
- `labelId` (Long, deprecated): First label ID (for backward compatibility)
- `label` (LabelDtos.Response, deprecated): First label object (for backward compatibility)
- `labelString` (String): Legacy enum string (DIRECT, DIVERT, etc.) if using old enum labels

---

## Frontend Implementation Guide

### 1. Update Deal Form

**Before (Single Select):**
```jsx
<Select
  options={labels}
  value={selectedLabel}
  onChange={(selected) => setLabelId(selected?.value)}
/>
```

**After (Multi-Select):**
```jsx
<Select
  isMulti
  options={labels}
  value={selectedLabels}
  onChange={(selected) => {
    const labelIds = selected ? selected.map(s => s.value) : [];
    setLabelIds(labelIds);
  }}
/>
```

### 2. Update State Management

**Before:**
```javascript
const [labelId, setLabelId] = useState(null);
```

**After:**
```javascript
const [labelIds, setLabelIds] = useState([]);
```

### 3. Update API Calls

**Create Deal:**
```javascript
const createDeal = async (dealData) => {
  const response = await fetch('/api/deals', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      ...dealData,
      labelIds: [1, 2, 3]  // ✅ Array of label IDs
    })
  });
  return response.json();
};
```

**Update Deal:**
```javascript
const updateDeal = async (dealId, dealData) => {
  const response = await fetch(`/api/deals/${dealId}`, {
    method: 'PATCH',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      ...dealData,
      labelIds: [2, 4, 5]  // ✅ Array of label IDs
    })
  });
  return response.json();
};
```

### 4. Update Display Components

**Before (Single Label):**
```jsx
{deal.label && (
  <LabelBadge label={deal.label} />
)}
```

**After (Multiple Labels):**
```jsx
{deal.labels && deal.labels.length > 0 && (
  <div className="label-container">
    {deal.labels.map(label => (
      <LabelBadge key={label.id} label={label} />
    ))}
  </div>
)}
```

### 5. Handle Response Data

**Recommended Approach (Use New Fields):**
```javascript
// Use labelIds and labels arrays
const dealLabelIds = deal.labelIds || [];
const dealLabels = deal.labels || [];

// Initialize form with selected labels
const selectedLabels = labels.filter(l => dealLabelIds.includes(l.id));
setLabelIds(dealLabelIds);
```

**Backward Compatibility (Fallback):**
```javascript
// Fallback to deprecated fields if new fields not available
const dealLabelIds = deal.labelIds || (deal.labelId ? [deal.labelId] : []);
const dealLabels = deal.labels || (deal.label ? [deal.label] : []);
```

---

## Complete Example: React Component

```jsx
import React, { useState, useEffect } from 'react';
import Select from 'react-select';

const DealForm = ({ deal, onSubmit }) => {
  const [labels, setLabels] = useState([]);
  const [formData, setFormData] = useState({
    name: deal?.name || '',
    value: deal?.value || 0,
    labelIds: deal?.labelIds || []
  });

  // Fetch available labels
  useEffect(() => {
    fetch('/api/labels')
      .then(res => res.json())
      .then(data => setLabels(data.map(l => ({
        value: l.id,
        label: l.name,
        color: l.color
      }))));
  }, []);

  // Initialize selected labels from deal
  useEffect(() => {
    if (deal?.labelIds && labels.length > 0) {
      const selected = labels.filter(l => deal.labelIds.includes(l.value));
      setFormData(prev => ({ ...prev, labelIds: deal.labelIds }));
    }
  }, [deal, labels]);

  const handleSubmit = (e) => {
    e.preventDefault();
    onSubmit({
      ...formData,
      labelIds: formData.labelIds || []
    });
  };

  const handleLabelChange = (selected) => {
    const labelIds = selected ? selected.map(s => s.value) : [];
    setFormData(prev => ({ ...prev, labelIds }));
  };

  const selectedLabels = labels.filter(l => formData.labelIds.includes(l.value));

  return (
    <form onSubmit={handleSubmit}>
      <input
        type="text"
        value={formData.name}
        onChange={(e) => setFormData({ ...formData, name: e.target.value })}
        placeholder="Deal Name"
      />
      
      <input
        type="number"
        value={formData.value}
        onChange={(e) => setFormData({ ...formData, value: e.target.value })}
        placeholder="Deal Value"
      />

      <Select
        isMulti
        options={labels}
        value={selectedLabels}
        onChange={handleLabelChange}
        isClearable
        placeholder="Select Labels"
      />

      <button type="submit">Save Deal</button>
    </form>
  );
};
```

---

## Migration Checklist

- [ ] Update deal form to use multi-select component
- [ ] Change state from `labelId` to `labelIds` (array)
- [ ] Update API calls to send `labelIds` array
- [ ] Update display components to show multiple labels
- [ ] Handle response data using `labelIds` and `labels` arrays
- [ ] Test backward compatibility with existing deals
- [ ] Update TypeScript interfaces/types if using TypeScript

---

## Backward Compatibility

The API maintains full backward compatibility:

1. **Request:** You can still send `labelId` (single value) - it will be converted to `labelIds` array internally
2. **Response:** The response includes both:
   - New fields: `labelIds` and `labels` (arrays)
   - Deprecated fields: `labelId` and `label` (single values, first label from array)

**Recommendation:** Migrate to using `labelIds` and `labels` arrays as soon as possible. The deprecated fields will be removed in a future version.

---

## Error Handling

**Invalid Label ID:**
```json
{
  "error": "Label not found with id: 999"
}
```

**Deleted Label:**
```json
{
  "error": "Label with id 5 has been deleted"
}
```

**Empty Array:**
- Sending `labelIds: []` will clear all labels from the deal
- This is valid and will not throw an error

---

## Notes

1. **Label Validation:** All label IDs must exist and not be deleted. If any label ID is invalid, the entire request will fail.

2. **Performance:** Labels are eagerly loaded to avoid N+1 queries, so there's no performance penalty for multiple labels.

3. **Legacy Enum Labels:** The old enum-based labels (`labelString`: DIRECT, DIVERT, etc.) are still supported but separate from custom labels. Custom labels take precedence.

4. **Empty Labels:** A deal can have zero labels (empty array). This is valid.

---

## Questions?

If you have any questions or need clarification, please contact the backend team.

