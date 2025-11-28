# Swagger Testing Guide - Activity Screenshot Upload

## Accessing Swagger UI

1. Start your application
2. Navigate to: **http://localhost:8080/swagger-ui.html** (or your deployed URL)
3. You should see the Swagger UI interface

## Testing the Screenshot Upload Endpoint

### Endpoint Details
- **URL:** `POST /api/activities/{activityId}/upload-screenshot`
- **Content-Type:** `multipart/form-data`
- **Authentication:** Required (Bearer Token)

### Steps to Test in Swagger UI

1. **Authenticate First:**
   - Find the **Auth** section at the top of Swagger UI
   - Click **Authorize** button
   - Enter your JWT token in the format: `Bearer YOUR_JWT_TOKEN`
   - Click **Authorize** and then **Close**

2. **Navigate to Activities Section:**
   - Scroll down to find the **Activities** section
   - Expand it to see all available endpoints

3. **Find the Upload Endpoint:**
   - Look for: **POST /api/activities/{activityId}/upload-screenshot**
   - Click on it to expand

4. **Fill in the Parameters:**
   - **activityId** (path parameter): Enter a valid activity ID (e.g., `1`)
   - **file** (form data): Click **Choose File** and select an image file (PNG, JPEG, etc.)

5. **Execute the Request:**
   - Click the **Try it out** button
   - Fill in the `activityId` field
   - Click **Choose File** and select an image file
   - Click **Execute**

6. **View the Response:**
   - You should see a response like:
   ```json
   {
     "success": true,
     "message": "Screenshot uploaded successfully",
     "data": "https://bridesideimages.blob.core.windows.net/call-screenshots/1234567890-abc12345-screenshot.png"
   }
   ```

### Expected Responses

#### Success (200 OK)
```json
{
  "success": true,
  "message": "Screenshot uploaded successfully",
  "data": "https://bridesideimages.blob.core.windows.net/call-screenshots/1701234567890-a1b2c3d4-screenshot.png"
}
```

#### Error - Invalid File Type (400 Bad Request)
```json
{
  "success": false,
  "message": "Only image files are allowed. Received content type: application/pdf",
  "data": null
}
```

#### Error - File Too Large (400 Bad Request)
```json
{
  "success": false,
  "message": "File size exceeds maximum allowed size of 10MB",
  "data": null
}
```

#### Error - Azure Not Configured (503 Service Unavailable)
```json
{
  "success": false,
  "message": "Azure Blob Storage is not configured. Please set AZURE_STORAGE_BLOB_CONNECTION_STRING environment variable.",
  "data": null
}
```

#### Error - Activity Not Found (404 Not Found)
```json
{
  "success": false,
  "message": "Activity not found: 999",
  "data": null
}
```

## Testing with cURL (Alternative)

If you prefer using cURL instead of Swagger UI:

```bash
curl -X POST \
  "http://localhost:8080/api/activities/1/upload-screenshot" \
  -H "Authorization: Bearer YOUR_JWT_TOKEN" \
  -H "Content-Type: multipart/form-data" \
  -F "file=@/path/to/your/screenshot.png"
```

## Testing with Postman

1. **Method:** POST
2. **URL:** `http://localhost:8080/api/activities/{activityId}/upload-screenshot`
   - Replace `{activityId}` with actual ID (e.g., `1`)
3. **Headers:**
   - `Authorization: Bearer YOUR_JWT_TOKEN`
4. **Body:**
   - Select **form-data**
   - Key: `file` (type: File)
   - Value: Select your image file
5. **Send** the request

## Verification

After a successful upload:

1. The activity's `attachmentUrl` field is automatically updated
2. You can verify by calling `GET /api/activities/{activityId}` and checking the `attachmentUrl` field
3. The returned URL should be accessible (if container is set to public access)

## Troubleshooting

### Issue: "Azure Blob Storage is not configured"
**Solution:** Make sure you've set the `AZURE_STORAGE_BLOB_CONNECTION_STRING` environment variable and restarted the application.

### Issue: "Activity not found"
**Solution:** Make sure the activity ID exists. You can check by calling `GET /api/activities` first.

### Issue: "Only image files are allowed"
**Solution:** Make sure you're uploading an image file (PNG, JPEG, GIF, etc.), not a PDF or other file type.

### Issue: "File size exceeds maximum allowed size"
**Solution:** The maximum file size is 10MB. Compress or resize your image if needed.

### Issue: Cannot see the file upload option in Swagger
**Solution:** Make sure you're using a modern browser. Some older browsers may not support the file input in Swagger UI properly.

