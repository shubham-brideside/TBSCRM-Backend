# Meeting Summary Debugging Guide

## Issue
`meetingAssignedCount` is returning 0 even though the SQL query `SELECT COUNT(*) FROM activities WHERE category = 'MEETING_SCHEDULER'` returns the correct count.

## Debugging Steps Added

### 1. **Comprehensive Parameter Logging**
When `/api/activities/summary/meeting` is called, the logs will show:
- All request parameters (personId, dateFrom, dateTo, assignedUser, organizationIds, assignedUserIds, category, done, serviceCategoryCodes, organizationCategoryCodes)
- Whether filters are detected (`hasFilters` flag)

### 2. **Direct SQL Query Execution**
When no filters are provided, the code executes:
- `SELECT COUNT(*) FROM activities WHERE category = 'MEETING_SCHEDULER'` - Direct SQL count
- `SELECT category, COUNT(*) FROM activities GROUP BY category` - Category distribution from DB

### 3. **JPA Query Comparison**
The code also executes JPA queries and compares:
- JPA `count()` result vs SQL count
- JPA `findAll()` result vs SQL count
- JPA category distribution vs SQL category distribution

### 4. **Filtered Activities Debugging**
When filters are provided, `loadFilteredActivities` logs:
- All input parameters
- Number of activities loaded from database (before category/date filtering)
- Category distribution BEFORE filtering
- Number of activities AFTER category filtering
- Warning if all activities are filtered out

### 5. **Final Count Logging**
Before returning the response, logs show:
- `activities.size()` - Total activities loaded
- `assigned` - Value set to `meetingAssignedCount`
- `completed` - Value set to `meetingDoneCount`
- Final response values

## How to Debug

### Step 1: Check the Logs
When you call `/api/activities/summary/meeting`, look for these log entries:

```
=== MEETING SUMMARY DEBUG START ===
Meeting summary called with parameters:
  personId: ...
  dateFrom: ...
  ...
Meeting summary: hasFilters = true/false
```

### Step 2: Check SQL vs JPA Results
If `hasFilters = false`, look for:
```
Meeting summary (no filters): Direct SQL query result: SELECT COUNT(*) FROM activities WHERE category = 'MEETING_SCHEDULER' = X
Meeting summary (no filters): Direct SQL category distribution: {...}
Meeting summary (no filters): JPA count() result: X
Meeting summary (no filters): JPA category distribution: {...}
Meeting summary: JPA findAll() loaded X activities
```

**Compare:**
- If SQL count > 0 but JPA count = 0 → JPA query issue (enum mismatch, case sensitivity, etc.)
- If SQL count = 0 → No data in database
- If SQL count = JPA count but `activities.size() = 0` → Issue with `findAll()` execution

### Step 3: Check Filtered Path
If `hasFilters = true`, look for:
```
=== loadFilteredActivities DEBUG START ===
loadFilteredActivities called with: ...
loadFilteredActivities: loaded X activities from database (before category/date filtering)
loadFilteredActivities: category distribution BEFORE filter: {...}
loadFilteredActivities: filtering by categories: [MEETING_SCHEDULER]
loadFilteredActivities: after category filter: X activities (from Y)
```

**Check:**
- If "loaded X activities" = 0 → Role-based scoping might be filtering everything out
- If "category distribution BEFORE filter" doesn't include MEETING_SCHEDULER → Activities don't have that category
- If "after category filter" = 0 but "before" > 0 → Category filter is too strict

### Step 4: Check Final Count
Look for:
```
=== MEETING SUMMARY CALCULATION ===
activities.size() = X
assigned (meetingAssignedCount) = X
...
=== MEETING SUMMARY DEBUG END ===
Final response: meetingAssignedCount=X, meetingDoneCount=Y, meetingOverdueCount=Z
```

## Common Issues and Solutions

### Issue 1: SQL count > 0 but JPA count = 0
**Possible causes:**
- Enum value mismatch (database has different case or value)
- JPA not recognizing the enum value
- Column type mismatch

**Solution:**
- Check the "Direct SQL category distribution" to see what values are actually in the database
- Compare with "JPA category distribution" to see if JPA is reading them correctly

### Issue 2: Role-based scoping filtering everything out
**Possible causes:**
- User doesn't have access to organizations/users that own the meetings
- `buildSpecification` is applying too strict filters

**Solution:**
- Check if user is ADMIN (should bypass scoping)
- Check the scope resolution in logs
- Consider bypassing role-based scoping for meeting summary when no explicit filters are provided

### Issue 3: Category filter too strict
**Possible causes:**
- Activities have `category = null`
- Category enum value doesn't match what's in database

**Solution:**
- Check "category distribution BEFORE filter" to see what categories exist
- Verify that activities actually have `category = 'MEETING_SCHEDULER'` in the database

## Next Steps

1. **Run the API** and collect all log entries starting with `=== MEETING SUMMARY` or `=== loadFilteredActivities`
2. **Share the logs** so we can identify where the issue is occurring
3. **Check the database directly** with: `SELECT category, COUNT(*) FROM activities GROUP BY category;`
4. **Verify enum values** match what's stored in the database

