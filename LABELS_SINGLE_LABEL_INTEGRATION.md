# Single Label Integration Guide

## Overview

Labels are now stored as a **single label per deal/person** using a foreign key relationship. The `label` field in both `Deal` and `Person` entities now references the `labels` table instead of using enums.

> **Migration Note**: The legacy enum `label` field is kept for backward compatibility but is read-only. Use `labelId` to set custom labels.

---

## API Changes

### Create/Update Deal with Label

**Before (Many-to-Many):**
```json
{
  "name": "Wedding Photography",
  "value": 50000,
  "labelIds": [1, 3, 5]  // ❌ No longer supported
}
```

**After (Single Label):**
```json
{
  "name": "Wedding Photography",
  "value": 50000,
  "labelId": 3  // ✅ Single label ID
}
```

### Create/Update Person with Label

**Before (Many-to-Many):**
```json
{
  "name": "John Doe",
  "phone": "9876543210",
  "labelIds": [1, 2]  // ❌ No longer supported
}
```

**After (Single Label):**
```json
{
  "name": "John Doe",
  "phone": "9876543210",
  "labelId": 2  // ✅ Single label ID
}
```

---

## Request/Response Format

### Deal Create/Update Request

```json
POST /api/deals
PUT /api/deals/{id}
```

```json
{
  "name": "Wedding Photography",
  "value": 50000,
  "pipelineId": 1,
  "labelId": 3  // Optional: ID from labels table
}
```

### Deal Response

```json
{
  "id": 1,
  "name": "Wedding Photography",
  "value": 50000,
  "label": {
    "id": 3,
    "name": "Hot Lead",
    "color": "#FF5733",
    "createdAt": "2024-12-17T10:00:00",
    "updatedAt": "2024-12-17T10:00:00"
  },
  "labelId": 3
}
```

### Person Create/Update Request

```json
POST /api/persons
PUT /api/persons/{id}
```

```json
{
  "name": "John Doe",
  "phone": "9876543210",
  "labelId": 2  // Optional: ID from labels table
}
```

### Person Response

```json
{
  "id": 1,
  "name": "John Doe",
  "phone": "9876543210",
  "label": {
    "id": 2,
    "name": "Bridal Makeup",
    "color": "#F44336",
    "createdAt": "2024-12-17T10:00:00",
    "updatedAt": "2024-12-17T10:00:00"
  },
  "labelId": 2
}
```

---

## Frontend Implementation Changes

### 1. Update Deal Form

**Before:**
```jsx
// Multi-select for labels
<Select
  isMulti
  options={labels}
  value={selectedLabels}
  onChange={(selected) => setLabelIds(selected.map(s => s.value))}
/>
```

**After:**
```jsx
// Single select for label
<Select
  options={labels}
  value={selectedLabel}
  onChange={(selected) => setLabelId(selected?.value)}
/>
```

### 2. Update Person Form

**Before:**
```jsx
// Multi-select
<Select
  isMulti
  options={labels}
  value={selectedLabels}
  onChange={(selected) => setLabelIds(selected.map(s => s.value))}
/>
```

**After:**
```jsx
// Single select
<Select
  options={labels}
  value={selectedLabel}
  onChange={(selected) => setLabelId(selected?.value)}
/>
```

### 3. Update API Calls

**Before:**
```javascript
const createDeal = async (dealData) => {
  const response = await fetch('/api/deals', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      ...dealData,
      labelIds: [1, 2, 3]  // ❌ Old way
    })
  });
  return response.json();
};
```

**After:**
```javascript
const createDeal = async (dealData) => {
  const response = await fetch('/api/deals', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      ...dealData,
      labelId: 3  // ✅ New way - single label ID
    })
  });
  return response.json();
};
```

### 4. Update Display Components

**Before:**
```jsx
// Display multiple labels
{deal.labels?.map(label => (
  <LabelBadge key={label.id} label={label} />
))}
```

**After:**
```jsx
// Display single label
{deal.label && (
  <LabelBadge label={deal.label} />
)}
```

### 5. Update State Management

**Before:**
```javascript
const [labelIds, setLabelIds] = useState([]);

// In form submission
const handleSubmit = () => {
  createDeal({ ...formData, labelIds });
};
```

**After:**
```javascript
const [labelId, setLabelId] = useState(null);

// In form submission
const handleSubmit = () => {
  createDeal({ ...formData, labelId });
};
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
    labelId: deal?.label?.id || null
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

  const handleSubmit = (e) => {
    e.preventDefault();
    onSubmit({
      ...formData,
      labelId: formData.labelId || null
    });
  };

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
        options={labels}
        value={labels.find(l => l.value === formData.labelId)}
        onChange={(selected) => setFormData({ 
          ...formData, 
          labelId: selected?.value || null 
        })}
        isClearable
        placeholder="Select Label"
      />

      <button type="submit">Save Deal</button>
    </form>
  );
};

export default DealForm;
```

---

## Migration Checklist

- [ ] Update all deal create/update forms to use `labelId` instead of `labelIds`
- [ ] Update all person create/update forms to use `labelId` instead of `labelIds`
- [ ] Change multi-select components to single-select
- [ ] Update API calls to send `labelId` (number) instead of `labelIds` (array)
- [ ] Update display components to show single label instead of multiple
- [ ] Update state management to use `labelId` instead of `labelIds`
- [ ] Test create/update operations with labels
- [ ] Test removing labels (set `labelId: null`)

---

## Database Migration

Run the migration to add `label_id` columns:

```sql
-- This is handled by Flyway migration V20251217_06__add_label_id_to_deals_and_persons.sql
-- Adds label_id foreign key to deals and persons tables
```

---

## Backward Compatibility

- The legacy `label` enum field is still present but **read-only**
- Old API calls using `label` enum will still work but won't affect the new `labelId` field
- To fully migrate, use `labelId` for all new creates/updates

---

## Error Handling

| Error | Cause | Solution |
|-------|-------|----------|
| `Label not found with id: X` | Invalid `labelId` | Verify label exists via `GET /api/labels` |
| `400 Bad Request` | `labelId` is not a number | Ensure `labelId` is a valid integer or `null` |

---

## Summary of Changes

| Aspect | Before | After |
|--------|--------|-------|
| **Field Name** | `labelIds` (array) | `labelId` (single number) |
| **UI Component** | Multi-select | Single select |
| **Storage** | Join tables (`deal_labels`, `person_labels`) | Foreign key (`label_id` column) |
| **Response** | `labels: [...]` (array) | `label: {...}` (object) |
| **Max Labels** | Multiple | One per deal/person |

