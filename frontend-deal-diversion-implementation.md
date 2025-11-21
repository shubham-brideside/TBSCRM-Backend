# Deal Diversion - Frontend Implementation Guide

This guide provides step-by-step instructions for implementing the deal diversion feature in your frontend application.

---

## Overview

Deal diversion allows users to create a new deal in a different pipeline based on an existing deal. When a deal is diverted:
- A new deal is created with `label: "DIVERT"`
- The new deal is linked to the original deal via `referencedDealId`
- The **initial/original pipeline** is stored in `referencedPipelineId` (always points to the first pipeline, even when diverting a diverted deal)
- The system prevents diverting back to the original pipeline

**Important:** When a diverted deal is diverted again, the `referencedPipelineId` always points to the initial/original pipeline, not the pipeline of the deal being diverted.

---

## Prerequisites

- API Base URL configured (e.g., `https://your-backend-url.com/api`)
- Authentication token available for API requests
- Deal list/management UI already implemented

---

## Step 1: Add "Divert Deal" Button

Add a "Divert Deal" button or action to your deal card/list item.

### Example (React/TypeScript):

```tsx
// DealCard.tsx
interface DealCardProps {
  deal: Deal;
  onDivert: (dealId: number) => void;
}

const DealCard: React.FC<DealCardProps> = ({ deal, onDivert }) => {
  return (
    <div className="deal-card">
      <h3>{deal.name}</h3>
      <p>Status: {deal.status}</p>
      <p>Pipeline ID: {deal.pipelineId}</p>
      
      {/* Show diversion indicator if already diverted */}
      {deal.isDiverted && (
        <div className="diverted-badge">
          Diverted from Pipeline {deal.referencedPipelineId}
        </div>
      )}
      
      <button onClick={() => onDivert(deal.id)}>
        Divert Deal
      </button>
    </div>
  );
};
```

---

## Step 2: Create Diversion Modal/Form

Create a modal or form component for the diversion flow.

### Example (React/TypeScript):

```tsx
// DivertDealModal.tsx
import { useState, useEffect } from 'react';

interface DivertDealModalProps {
  dealId: number;
  isOpen: boolean;
  onClose: () => void;
  onSuccess: () => void;
}

interface Pipeline {
  id: number;
  name: string;
  category: string;
  teamId?: number;
}

const DivertDealModal: React.FC<DivertDealModalProps> = ({
  dealId,
  isOpen,
  onClose,
  onSuccess
}) => {
  const [availablePipelines, setAvailablePipelines] = useState<Pipeline[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [selectedPipelineId, setSelectedPipelineId] = useState<number | null>(null);
  const [formData, setFormData] = useState({
    name: '',
    value: '',
    stageId: null as number | null,
    personId: null as number | null,
    organizationId: null as number | null,
  });

  // Fetch available pipelines when modal opens
  useEffect(() => {
    if (isOpen && dealId) {
      fetchAvailablePipelines();
    }
  }, [isOpen, dealId]);

  const fetchAvailablePipelines = async () => {
    setLoading(true);
    setError(null);
    
    try {
      const response = await fetch(
        `${API_BASE_URL}/deals/${dealId}/available-pipelines`,
        {
          headers: {
            'Authorization': `Bearer ${getAuthToken()}`,
            'Content-Type': 'application/json',
          },
        }
      );

      if (!response.ok) {
        if (response.status === 404) {
          throw new Error('Deal not found');
        }
        throw new Error('Failed to fetch available pipelines');
      }

      const pipelines = await response.json();
      setAvailablePipelines(pipelines);

      // Show message if no pipelines available
      if (pipelines.length === 0) {
        setError('This deal has been diverted to all available pipelines');
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'An error occurred');
    } finally {
      setLoading(false);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!selectedPipelineId) {
      setError('Please select a pipeline');
      return;
    }

    setLoading(true);
    setError(null);

    try {
      const response = await fetch(`${API_BASE_URL}/deals`, {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${getAuthToken()}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          name: formData.name,
          // Note: value is automatically set to 0 for diverted deals by the backend
          // You can omit it or send 0/null - the backend will ignore any value and set it to 0
          value: 0,
          label: 'DIVERT',
          referencedDealId: dealId,
          pipelineId: selectedPipelineId,
          stageId: formData.stageId,
          personId: formData.personId,
          organizationId: formData.organizationId,
          status: 'IN_PROGRESS',
        }),
      });

      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(errorData.message || 'Failed to create diverted deal');
      }

      const newDeal = await response.json();
      onSuccess();
      onClose();
      
      // Optionally show success message
      showNotification('Deal diverted successfully!', 'success');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'An error occurred');
    } finally {
      setLoading(false);
    }
  };

  if (!isOpen) return null;

  return (
    <div className="modal-overlay" onClick={onClose}>
      <div className="modal-content" onClick={(e) => e.stopPropagation()}>
        <div className="modal-header">
          <h2>Divert Deal</h2>
          <button onClick={onClose} className="close-button">×</button>
        </div>

        <form onSubmit={handleSubmit}>
          {loading && availablePipelines.length === 0 && (
            <div className="loading">Loading available pipelines...</div>
          )}

          {error && (
            <div className="error-message">{error}</div>
          )}

          {availablePipelines.length > 0 && (
            <>
              <div className="form-group">
                <label htmlFor="pipeline">Select Pipeline *</label>
                <select
                  id="pipeline"
                  value={selectedPipelineId || ''}
                  onChange={(e) => setSelectedPipelineId(Number(e.target.value))}
                  required
                >
                  <option value="">-- Select Pipeline --</option>
                  {availablePipelines.map((pipeline) => (
                    <option key={pipeline.id} value={pipeline.id}>
                      {pipeline.name} ({pipeline.category})
                    </option>
                  ))}
                </select>
              </div>

              <div className="form-group">
                <label htmlFor="name">Deal Name *</label>
                <input
                  id="name"
                  type="text"
                  value={formData.name}
                  onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                  required
                  placeholder="Enter deal name"
                />
              </div>

              {/* Note: Value field is hidden for diverted deals as it's automatically set to 0 by the backend */}
              {/* Uncomment below if you want to show it (though it will be ignored) */}
              {/* 
              <div className="form-group">
                <label htmlFor="value">Deal Value</label>
                <input
                  id="value"
                  type="number"
                  step="0.01"
                  value={formData.value}
                  onChange={(e) => setFormData({ ...formData, value: e.target.value })}
                  placeholder="0.00"
                  disabled
                />
                <small>Value is automatically set to 0 for diverted deals</small>
              </div>
              */}

              {/* Add other form fields as needed (stage, person, organization, etc.) */}

              <div className="modal-actions">
                <button type="button" onClick={onClose} disabled={loading}>
                  Cancel
                </button>
                <button type="submit" disabled={loading || !selectedPipelineId}>
                  {loading ? 'Creating...' : 'Create Diverted Deal'}
                </button>
              </div>
            </>
          )}

          {!loading && availablePipelines.length === 0 && !error && (
            <div className="info-message">
              This deal has been diverted to all available pipelines.
            </div>
          )}
        </form>
      </div>
    </div>
  );
};

export default DivertDealModal;
```

---

## Step 3: Update Deal List to Show Diversion Status

Update your deal list/card component to display diversion information.

### Example:

```tsx
// DealList.tsx
interface Deal {
  id: number;
  name: string;
  status: string;
  pipelineId: number;
  isDiverted: boolean;
  referencedDealId: number | null;
  referencedPipelineId: number | null;
}

const DealList: React.FC = () => {
  const [deals, setDeals] = useState<Deal[]>([]);
  const [selectedDealId, setSelectedDealId] = useState<number | null>(null);

  // Fetch deals on component mount
  useEffect(() => {
    fetchDeals();
  }, []);

  const fetchDeals = async () => {
    try {
      const response = await fetch(`${API_BASE_URL}/deals`, {
        headers: {
          'Authorization': `Bearer ${getAuthToken()}`,
        },
      });
      const dealsData = await response.json();
      setDeals(dealsData);
    } catch (error) {
      console.error('Failed to fetch deals:', error);
    }
  };

  return (
    <div className="deal-list">
      {deals.map((deal) => (
        <div key={deal.id} className="deal-item">
          <h3>{deal.name}</h3>
          <p>Status: {deal.status}</p>
          
          {/* Show diversion link if diverted */}
          {deal.isDiverted && deal.referencedDealId && (
            <div className="diversion-info">
              <span className="badge">Diverted</span>
              <a 
                href={`/deals/${deal.referencedDealId}`}
                onClick={(e) => {
                  e.preventDefault();
                  navigateToDeal(deal.referencedDealId!);
                }}
              >
                View Original Deal
              </a>
            </div>
          )}
          
          {/* Show if this deal has been diverted to other pipelines */}
          {deal.referencedPipelineId && (
            <div className="original-pipeline-info">
              Originally from Pipeline {deal.referencedPipelineId}
            </div>
          )}
          
          <button onClick={() => setSelectedDealId(deal.id)}>
            Divert Deal
          </button>
        </div>
      ))}
      
      {selectedDealId && (
        <DivertDealModal
          dealId={selectedDealId}
          isOpen={true}
          onClose={() => setSelectedDealId(null)}
          onSuccess={() => {
            fetchDeals(); // Refresh deals list
            setSelectedDealId(null);
          }}
        />
      )}
    </div>
  );
};
```

---

## Step 4: Handle API Responses

### Success Response Example:

```json
{
  "id": 215,
  "name": "Diverted Deal - Photography",
  "value": 50000,
  "label": "DIVERT",
  "isDiverted": true,
  "referencedDealId": 123,
  "referencedPipelineId": 8,
  "pipelineId": 5,
  "status": "IN_PROGRESS",
  ...
}
```

### Error Handling:

```tsx
const handleApiError = (error: any) => {
  if (error.status === 400) {
    // Validation error
    const message = error.message || 'Invalid input';
    showNotification(message, 'error');
  } else if (error.status === 404) {
    // Deal not found
    showNotification('Deal not found', 'error');
  } else {
    // Generic error
    showNotification('An unexpected error occurred', 'error');
  }
};
```

---

## Step 5: Styling (CSS Example)

```css
/* Modal Styles */
.modal-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.modal-content {
  background: white;
  border-radius: 8px;
  padding: 24px;
  max-width: 500px;
  width: 90%;
  max-height: 90vh;
  overflow-y: auto;
}

.modal-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 20px;
}

.close-button {
  background: none;
  border: none;
  font-size: 24px;
  cursor: pointer;
  color: #666;
}

.form-group {
  margin-bottom: 16px;
}

.form-group label {
  display: block;
  margin-bottom: 4px;
  font-weight: 500;
}

.form-group input,
.form-group select {
  width: 100%;
  padding: 8px;
  border: 1px solid #ddd;
  border-radius: 4px;
}

.modal-actions {
  display: flex;
  justify-content: flex-end;
  gap: 12px;
  margin-top: 24px;
}

.modal-actions button {
  padding: 10px 20px;
  border: none;
  border-radius: 4px;
  cursor: pointer;
}

.modal-actions button[type="submit"] {
  background: #007bff;
  color: white;
}

.modal-actions button[type="submit"]:disabled {
  background: #ccc;
  cursor: not-allowed;
}

/* Diversion Badge */
.diverted-badge {
  display: inline-block;
  padding: 4px 8px;
  background: #ffc107;
  color: #000;
  border-radius: 4px;
  font-size: 12px;
  font-weight: 500;
  margin: 8px 0;
}

.diversion-info {
  margin: 8px 0;
  font-size: 14px;
}

.diversion-info a {
  color: #007bff;
  text-decoration: none;
  margin-left: 8px;
}

.diversion-info a:hover {
  text-decoration: underline;
}

.error-message {
  padding: 12px;
  background: #fee;
  color: #c33;
  border-radius: 4px;
  margin-bottom: 16px;
}

.info-message {
  padding: 12px;
  background: #e3f2fd;
  color: #1976d2;
  border-radius: 4px;
  margin-bottom: 16px;
}
```

---

## Step 6: Testing Checklist

- [ ] "Divert Deal" button appears on deal cards
- [ ] Clicking "Divert Deal" opens the modal
- [ ] Available pipelines are fetched and displayed
- [ ] Original pipeline is excluded from the list
- [ ] Pipelines where deal is already diverted are excluded
- [ ] Form validation works (required fields)
- [ ] Successfully creating a diverted deal
- [ ] Error handling for API failures
- [ ] Diversion badge appears on diverted deals
- [ ] Link to original deal works (if implemented)
- [ ] Deal list refreshes after successful diversion
- [ ] Empty state message shows when no pipelines available

---

## Common Issues and Solutions

### Issue: "This deal has been diverted to all available pipelines"
**Solution:** This is expected behavior. The deal has already been diverted to all available pipelines. Consider disabling the "Divert Deal" button in this case.

### Issue: Original pipeline still appears in the list
**Solution:** Ensure you're using the correct endpoint (`/api/deals/{dealId}/available-pipelines`) and that the backend has the latest code with the exclusion logic.

### Issue: 404 error when fetching available pipelines
**Solution:** Verify the deal ID is correct and the deal exists in the database.

### Issue: Validation error "referencedDealId is required"
**Solution:** Ensure you're sending `referencedDealId` in the request body when `label` is `"DIVERT"`.

---

## API Endpoints Summary

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/deals/{dealId}/available-pipelines` | GET | Get pipelines available for diversion |
| `/api/deals` | POST | Create a new deal (including diverted deals) |
| `/api/deals` | GET | List all deals |
| `/api/deals/{id}` | GET | Get a single deal |

---

## Example API Calls

### Fetch Available Pipelines:
```javascript
const response = await fetch(
  `${API_BASE_URL}/deals/123/available-pipelines`,
  {
    headers: {
      'Authorization': `Bearer ${token}`,
    },
  }
);
const pipelines = await response.json();
```

### Create Diverted Deal:
```javascript
const response = await fetch(`${API_BASE_URL}/deals`, {
  method: 'POST',
  headers: {
    'Authorization': `Bearer ${token}`,
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    name: 'Diverted Deal Name',
    label: 'DIVERT',
    referencedDealId: 123,
    pipelineId: 5,
    value: 50000,
    status: 'IN_PROGRESS',
  }),
});
const newDeal = await response.json();
```

---

## Best Practices

1. **Loading States**: Always show loading indicators when fetching data
2. **Error Handling**: Provide clear error messages to users
3. **Validation**: Validate form inputs before submission
4. **User Feedback**: Show success/error notifications after actions
5. **Refresh Data**: Refresh deal list after successful diversion
6. **Disable Actions**: Disable "Divert Deal" button if no pipelines available
7. **Accessibility**: Ensure modal is keyboard accessible and has proper ARIA labels

---

## Additional Features (Optional)

### Show Diverted Deals Count:
```tsx
const divertedDealsCount = deals.filter(deal => deal.isDiverted).length;
```

### Filter Diverted Deals:
```tsx
const [filter, setFilter] = useState<'all' | 'diverted' | 'original'>('all');

const filteredDeals = deals.filter(deal => {
  if (filter === 'diverted') return deal.isDiverted;
  if (filter === 'original') return !deal.isDiverted;
  return true;
});
```

### Show Diverted Deals Tree:
```tsx
const getDivertedDeals = (dealId: number) => {
  return deals.filter(deal => deal.referencedDealId === dealId);
};
```

---

This guide should help you implement the deal diversion feature in your frontend. Adjust the code examples to match your framework and coding style.

