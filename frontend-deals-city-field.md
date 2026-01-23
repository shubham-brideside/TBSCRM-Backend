## Deals `city` Field Integration (Frontend)

This document explains how the new `city` field is exposed in the Deals API and how the frontend should send and consume it.

---

## Create Deal (`POST /api/deals`)

**Request body** (`DealDtos.CreateRequest`):

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
  "city": "Mumbai",
  "finalThankYouSent": false,
  "eventDateAsked": true,
  "contactNumberAsked": true,
  "venueAsked": false,
  "eventDate": "2025-12-19",
  "eventDates": ["2025-12-19"],
  "label": "DIRECT",
  "labelId": null,
  "labelIds": [1, 2],
  "source": "Direct",
  "subSource": "Instagram",
  "referencedDealId": null,
  "createdBy": "USER",
  "createdByUserId": 42
}
```

**Notes**

- **city** (optional, string): free-text city for the deal (e.g. `"Mumbai"`).
- If omitted, the deal will be created with `city = null`.

### TypeScript interface (frontend)

```ts
interface CreateDealRequest {
  name: string;
  value?: number;
  personId?: number;
  pipelineId?: number;
  stageId?: number;
  sourceId?: number;
  organizationId?: number;
  categoryId?: number | string;
  eventType?: string;
  status?: 'IN_PROGRESS' | 'WON' | 'LOST';
  commissionAmount?: number;
  venue?: string;
  phoneNumber?: string;
  city?: string;
  finalThankYouSent?: boolean;
  eventDateAsked?: boolean;
  contactNumberAsked?: boolean;
  venueAsked?: boolean;
  eventDate?: string;        // yyyy-MM-dd (legacy)
  eventDates?: string[];     // preferred
  label?: string;
  labelId?: number;
  labelIds?: number[];
  source?: string;
  subSource?: string;
  referencedDealId?: number | null;
  createdBy?: 'USER' | 'BOT';
  createdByUserId?: number;
}
```

---

## Update Deal (`PATCH /api/deals/{id}`)

**Request body** (`DealDtos.UpdateRequest`):

```json
{
  "name": "Updated Deal Name",
  "value": 150000,
  "personId": 101,
  "pipelineId": 8,
  "stageId": 40,
  "sourceId": 5,
  "organizationId": 12,
  "categoryId": 3,
  "eventType": "Reception",
  "status": "IN_PROGRESS",
  "commissionAmount": 9000,
  "venue": "The Oberoi",
  "phoneNumber": "+91 98765 43210",
  "city": "Delhi",
  "finalThankYouSent": true,
  "eventDateAsked": true,
  "contactNumberAsked": true,
  "venueAsked": true,
  "eventDate": "2025-12-20",
  "eventDates": ["2025-12-20"],
  "label": "DIRECT",
  "labelId": null,
  "labelIds": [1],
  "source": "Direct",
  "subSource": "Whatsapp",
  "clientBudget": 80000
}
```

**Notes**

- Only provided fields are updated (partial update).
- **city**: if present, `Deal.city` will be set to this value; if omitted, the existing `city` is left unchanged.

### TypeScript interface (frontend)

```ts
interface UpdateDealRequest {
  name?: string;
  value?: number;
  personId?: number;
  pipelineId?: number;
  stageId?: number;
  sourceId?: number;
  organizationId?: number;
  categoryId?: number | string;
  category?: string;
  eventType?: string;
  status?: 'IN_PROGRESS' | 'WON' | 'LOST';
  commissionAmount?: number;
  venue?: string;
  phoneNumber?: string;
  city?: string;
  finalThankYouSent?: boolean;
  eventDateAsked?: boolean;
  contactNumberAsked?: boolean;
  venueAsked?: boolean;
  eventDate?: string;
  eventDates?: string[];
  label?: string;
  labelId?: number;
  labelIds?: number[];
  source?: string;
  subSource?: string;
  clientBudget?: number;
}
```

---

## Deal Response Shape (`DealResponse`)

The `city` field is also returned in all deal responses (`GET /api/deals`, `GET /api/deals/{id}`, person/organization deal lists, etc.).

### Java DTO (`DealResponse`)

```9:52:src/main/java/com/brideside/crm/dto/DealResponse.java
public class DealResponse {
    public Long id;
    public String name;
    public BigDecimal value;
    public Long personId;
    public String personName;
    public Long pipelineId;
    public Long stageId;
    public Long sourceId;
    public Long organizationId;
    public String organizationName;
    public Long categoryId;
    public String eventType;
    public DealStatus status;
    public BigDecimal commissionAmount;
    public LocalDateTime createdAt;
    public LocalDateTime updatedAt;
    public String venue;
    public String phoneNumber;
    public String city;
    public Boolean finalThankYouSent;
    public Boolean eventDateAsked;
    public Boolean contactNumberAsked;
    public Boolean venueAsked;
    public String eventDate;
    public List<String> eventDates;
    // ... remaining fields unchanged ...
}
```

### TypeScript interface (frontend)

```ts
interface DealResponse {
  id: number;
  name: string;
  value: number;
  personId: number | null;
  personName: string | null;
  pipelineId: number | null;
  stageId: number | null;
  sourceId: number | null;
  organizationId: number | null;
  organizationName: string | null;
  categoryId: number | null;
  eventType: string | null;
  status: 'IN_PROGRESS' | 'WON' | 'LOST';
  commissionAmount: number | null;
  createdAt: string;
  updatedAt: string | null;
  venue: string | null;
  phoneNumber: string | null;
  city: string | null;
  finalThankYouSent: boolean | null;
  eventDateAsked: boolean | null;
  contactNumberAsked: boolean | null;
  venueAsked: boolean | null;
  eventDate: string | null;
  eventDates: string[] | null;
  // ... existing label/source/diversion fields ...
}
```


