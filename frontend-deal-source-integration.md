# Deal Source & Sub-Source - Frontend Integration Guide

This guide provides step-by-step instructions for integrating the deal source and sub-source fields into your frontend application.

## Overview

Deals now have two related fields:
- **`source`**: Main source category (Direct, Divert, Reference, Planner)
- **`subSource`**: Sub-category for Direct deals only (Instagram, Whatsapp, Landing Page, Email)

**Important:** `subSource` is only valid when `source` is `"Direct"`. If `source` is anything else, `subSource` will be ignored or cleared.

## Source Options

| Value | Display | Description |
|-------|---------|-------------|
| `Direct` | Direct | Direct leads/customers |
| `Divert` | Divert | Diverted deals |
| `Reference` | Reference | Referred by someone |
| `Planner` | Planner | From event planners |

## Sub-Source Options (Only for Direct)

| Value | Display | Description |
|-------|---------|-------------|
| `Instagram` | Instagram | Lead came from Instagram |
| `Whatsapp` | Whatsapp | Lead came from WhatsApp |
| `Landing Page` | Landing Page | Lead came from landing page |
| `Email` | Email | Lead came from email |

## API Usage

### Create Deal with Source and Sub-Source

```javascript
// Example: Direct deal with Instagram sub-source
const createDeal = async () => {
  const response = await fetch('/api/deals', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      name: 'Wedding Package',
      value: 125000,
      source: 'Direct',
      subSource: 'Instagram',  // Only valid when source is "Direct"
      // ... other fields
    })
  });
  return response.json();
};

// Example: Reference deal (no sub-source)
const createReferenceDeal = async () => {
  const response = await fetch('/api/deals', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      name: 'Wedding Package',
      value: 125000,
      source: 'Reference',
      // subSource is not needed and will be ignored
      // ... other fields
    })
  });
  return response.json();
};
```

### Update Deal Source and Sub-Source

```javascript
// Update source to Direct with sub-source
const updateDealSource = async (dealId) => {
  const response = await fetch(`/api/deals/${dealId}`, {
    method: 'PATCH',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      source: 'Direct',
      subSource: 'Whatsapp'
    })
  });
  return response.json();
};

// Change source from Direct to Reference (subSource will be cleared automatically)
const changeToReference = async (dealId) => {
  const response = await fetch(`/api/deals/${dealId}`, {
    method: 'PATCH',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      source: 'Reference'
      // subSource will be automatically cleared by backend
    })
  });
  return response.json();
};
```

## Frontend Implementation

### Step 1: Create Source Dropdown Component

```tsx
// SourceDropdown.tsx
import React, { useState, useEffect } from 'react';

const SOURCE_OPTIONS = [
  { value: 'Direct', label: 'Direct' },
  { value: 'Divert', label: 'Divert' },
  { value: 'Reference', label: 'Reference' },
  { value: 'Planner', label: 'Planner' }
];

const SUB_SOURCE_OPTIONS = [
  { value: 'Instagram', label: 'Instagram' },
  { value: 'Whatsapp', label: 'Whatsapp' },
  { value: 'Landing Page', label: 'Landing Page' },
  { value: 'Email', label: 'Email' }
];

interface SourceDropdownProps {
  value?: string;
  subSourceValue?: string;
  onChange: (source: string, subSource: string | null) => void;
  disabled?: boolean;
}

export const SourceDropdown: React.FC<SourceDropdownProps> = ({
  value,
  subSourceValue,
  onChange,
  disabled = false
}) => {
  const [source, setSource] = useState(value || '');
  const [subSource, setSubSource] = useState(subSourceValue || '');

  useEffect(() => {
    setSource(value || '');
    setSubSource(subSourceValue || '');
  }, [value, subSourceValue]);

  const handleSourceChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const newSource = e.target.value;
    setSource(newSource);
    
    // Clear subSource if source is not "Direct"
    if (newSource !== 'Direct') {
      setSubSource('');
      onChange(newSource, null);
    } else {
      // Keep subSource if it exists, otherwise pass empty string
      onChange(newSource, subSource || null);
    }
  };

  const handleSubSourceChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const newSubSource = e.target.value;
    setSubSource(newSubSource);
    onChange(source, newSubSource || null);
  };

  const showSubSource = source === 'Direct';

  return (
    <div className="source-dropdown">
      <div>
        <label htmlFor="source">Source:</label>
        <select
          id="source"
          value={source}
          onChange={handleSourceChange}
          disabled={disabled}
        >
          <option value="">Select Source</option>
          {SOURCE_OPTIONS.map(option => (
            <option key={option.value} value={option.value}>
              {option.label}
            </option>
          ))}
        </select>
      </div>

      {showSubSource && (
        <div>
          <label htmlFor="subSource">Sub-Source:</label>
          <select
            id="subSource"
            value={subSource}
            onChange={handleSubSourceChange}
            disabled={disabled}
          >
            <option value="">Select Sub-Source (Optional)</option>
            {SUB_SOURCE_OPTIONS.map(option => (
              <option key={option.value} value={option.value}>
                {option.label}
              </option>
            ))}
          </select>
        </div>
      )}
    </div>
  );
};
```

### Step 2: Integrate into Deal Form

```tsx
// DealForm.tsx
import React, { useState } from 'react';
import { SourceDropdown } from './SourceDropdown';

export const DealForm: React.FC = () => {
  const [formData, setFormData] = useState({
    name: '',
    value: 0,
    source: '',
    subSource: '',
    // ... other fields
  });

  const handleSourceChange = (source: string, subSource: string | null) => {
    setFormData(prev => ({
      ...prev,
      source,
      subSource: subSource || ''
    }));
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    const payload = {
      ...formData,
      // Only include subSource if source is "Direct"
      subSource: formData.source === 'Direct' ? formData.subSource : undefined
    };

    const response = await fetch('/api/deals', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
      },
      body: JSON.stringify(payload)
    });

    if (!response.ok) {
      const error = await response.json();
      alert(error.message);
      return;
    }

    // Handle success
    const deal = await response.json();
    console.log('Deal created:', deal);
  };

  return (
    <form onSubmit={handleSubmit}>
      {/* Other form fields */}
      
      <SourceDropdown
        value={formData.source}
        subSourceValue={formData.subSource}
        onChange={handleSourceChange}
      />
      
      {/* Other form fields */}
      
      <button type="submit">Create Deal</button>
    </form>
  );
};
```

### Step 3: Display Source and Sub-Source in Deal List

```tsx
// DealRow.tsx
interface Deal {
  id: number;
  name: string;
  source?: string;
  subSource?: string;
  // ... other fields
}

export const DealRow: React.FC<{ deal: Deal }> = ({ deal }) => {
  const getSourceDisplay = () => {
    if (!deal.source) return 'N/A';
    
    if (deal.source === 'Direct' && deal.subSource) {
      return `${deal.source} (${deal.subSource})`;
    }
    
    return deal.source;
  };

  return (
    <tr>
      <td>{deal.name}</td>
      <td>{getSourceDisplay()}</td>
      {/* Other columns */}
    </tr>
  );
};
```

## Validation Rules

### Frontend Validation

```typescript
const validateDealSource = (source: string, subSource: string | null): string | null => {
  // Source is optional, but if provided, must be valid
  if (source && !['Direct', 'Divert', 'Reference', 'Planner'].includes(source)) {
    return 'Invalid source value';
  }

  // SubSource validation
  if (subSource) {
    if (source !== 'Direct') {
      return 'Sub-source can only be set when source is "Direct"';
    }
    
    if (!['Instagram', 'Whatsapp', 'Landing Page', 'Email'].includes(subSource)) {
      return 'Invalid sub-source value';
    }
  }

  return null; // Valid
};
```

## Error Handling

The backend will return the following errors:

1. **Invalid source value:**
   ```json
   {
     "success": false,
     "message": "Invalid source value: InvalidValue. Allowed values: Direct, Divert, Reference, Planner",
     "data": null
   }
   ```

2. **Invalid subSource value:**
   ```json
   {
     "success": false,
     "message": "Invalid subSource value: InvalidValue. Allowed values: Instagram, Whatsapp, Landing Page, Email",
     "data": null
   }
   ```

3. **SubSource provided when source is not Direct:**
   ```json
   {
     "success": false,
     "message": "subSource can only be provided when source is 'Direct'",
     "data": null
   }
   ```

## Example: Complete Deal Creation Flow

```tsx
// Complete example with validation
const CreateDealForm: React.FC = () => {
  const [source, setSource] = useState('');
  const [subSource, setSubSource] = useState('');
  const [errors, setErrors] = useState<Record<string, string>>({});

  const handleSourceChange = (newSource: string, newSubSource: string | null) => {
    setSource(newSource);
    setSubSource(newSubSource || '');
    
    // Clear errors
    setErrors(prev => {
      const newErrors = { ...prev };
      delete newErrors.source;
      delete newErrors.subSource;
      return newErrors;
    });
  };

  const validate = () => {
    const newErrors: Record<string, string> = {};
    
    if (source && !['Direct', 'Divert', 'Reference', 'Planner'].includes(source)) {
      newErrors.source = 'Invalid source value';
    }
    
    if (subSource) {
      if (source !== 'Direct') {
        newErrors.subSource = 'Sub-source can only be set when source is "Direct"';
      } else if (!['Instagram', 'Whatsapp', 'Landing Page', 'Email'].includes(subSource)) {
        newErrors.subSource = 'Invalid sub-source value';
      }
    }
    
    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validate()) {
      return;
    }

    const payload = {
      name: 'Deal Name',
      value: 10000,
      source: source || undefined,
      subSource: (source === 'Direct' && subSource) ? subSource : undefined,
      // ... other fields
    };

    try {
      const response = await fetch('/api/deals', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${token}`
        },
        body: JSON.stringify(payload)
      });

      if (!response.ok) {
        const error = await response.json();
        alert(error.message);
        return;
      }

      const deal = await response.json();
      console.log('Deal created:', deal);
      // Handle success (e.g., redirect, show success message)
    } catch (error) {
      console.error('Error creating deal:', error);
      alert('Failed to create deal');
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <SourceDropdown
        value={source}
        subSourceValue={subSource}
        onChange={handleSourceChange}
      />
      
      {errors.source && <div className="error">{errors.source}</div>}
      {errors.subSource && <div className="error">{errors.subSource}</div>}
      
      <button type="submit">Create Deal</button>
    </form>
  );
};
```

## Quick Reference

### Source Values
- `"Direct"` - Direct leads
- `"Divert"` - Diverted deals
- `"Reference"` - Referred deals
- `"Planner"` - From planners

### Sub-Source Values (Direct only)
- `"Instagram"` - Instagram leads
- `"Whatsapp"` - WhatsApp leads
- `"Landing Page"` - Landing page leads
- `"Email"` - Email leads

### API Request Examples

```javascript
// Direct deal with Instagram
{ source: "Direct", subSource: "Instagram" }

// Reference deal (no sub-source)
{ source: "Reference" }

// Direct deal without sub-source (subSource will be null)
{ source: "Direct" }
```

## Notes

- Both `source` and `subSource` are **optional** fields
- `subSource` is **only valid** when `source` is `"Direct"`
- If `source` is changed to something other than `"Direct"`, `subSource` will be automatically cleared
- Case-insensitive: `"direct"`, `"Direct"`, `"DIRECT"` all work
- The API returns display format: `"Direct"`, `"Instagram"`, etc.

