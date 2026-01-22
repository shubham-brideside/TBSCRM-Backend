# Testing OLA Maps API

## Direct API Test

Test the OLA Maps API directly to see what the actual response is:

### Using curl (Windows PowerShell):

```powershell
# Basic test
curl "https://maps.olakrutrim.com/places/autocomplete?input=del&key=9QDZh4N31jeLF9LDo5dZqAHvQshXSkEUjqe09mN1"

# With verbose output to see headers
curl -v "https://maps.olakrutrim.com/places/autocomplete?input=del&key=9QDZh4N31jeLF9LDo5dZqAHvQshXSkEUjqe09mN1"
```

### Using Postman:

1. Method: `GET`
2. URL: `https://maps.olakrutrim.com/places/autocomplete`
3. Query Parameters:
   - `input`: `del`
   - `key`: `9QDZh4N31jeLF9LDo5dZqAHvQshXSkEUjqe09mN1`

## Common Issues to Check:

1. **Endpoint Format**: The endpoint might be:
   - `/api/places/autocomplete` (with /api prefix)
   - `/v1/places/autocomplete` (with version)
   - `/places/autocomplete` (current)

2. **Authentication Method**: OLA Maps might require:
   - OAuth 2.0 token instead of API key
   - API key in header instead of query parameter
   - Different parameter name (e.g., `apiKey` instead of `key`)

3. **Response Format**: The API might return a different JSON structure than expected

## Check Server Logs

When you make a request from the frontend, check your Spring Boot console for:
- The exact URL being called
- The HTTP status code returned
- The response body from OLA Maps API
- Any exception stack traces

## Next Steps

Once you identify the actual error from the logs or direct API test, we can:
1. Fix the endpoint URL if it's wrong
2. Update authentication method if needed
3. Adjust the response parsing if the format is different


