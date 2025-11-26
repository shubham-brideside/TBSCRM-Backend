# Azure Blob Storage Setup for Activity Screenshots

This document explains how to configure Azure Blob Storage for storing activity screenshot images.

## Azure Information Required

To use Azure Blob Storage, you need the following information from your Azure Storage Account:

### 1. Connection String
The connection string contains all the information needed to connect to your Azure Storage Account.

**How to get it:**
1. Go to Azure Portal → Your Storage Account
2. Navigate to **Access keys** (under Security + networking)
3. Copy the **Connection string** from either key1 or key2

**Format:**
```
DefaultEndpointsProtocol=https;AccountName=yourstorageaccount;AccountKey=yourkey==;EndpointSuffix=core.windows.net
```

### 2. Container Name (Optional)
The container name where screenshots will be stored. Default is `call-screenshots`.

**How to create a container:**
1. Go to Azure Portal → Your Storage Account
2. Navigate to **Containers** (under Data storage)
3. Click **+ Container**
4. Name: `call-screenshots`
5. Public access level: **Blob** (for public access) or **Private** (for private access with SAS tokens)

## Environment Variables

Set the following environment variables in Azure App Service:

### Required:
```
AZURE_STORAGE_BLOB_CONNECTION_STRING=DefaultEndpointsProtocol=https;AccountName=bridesideimages;AccountKey=YOUR_ACCOUNT_KEY;EndpointSuffix=core.windows.net
```

**⚠️ IMPORTANT:** Replace `YOUR_ACCOUNT_KEY` with your actual account key from the connection string you received from Azure Portal.

### Optional:
```
AZURE_STORAGE_BLOB_CONTAINER_NAME=call-screenshots
AZURE_STORAGE_BLOB_BASE_URL=https://bridesideimages.blob.core.windows.net
```

## Quick Setup Steps for Azure App Service

1. **Go to Azure Portal** → Your App Service → **Configuration** → **Application settings**

2. **Add/Update the following environment variable:**
   - **Name:** `AZURE_STORAGE_BLOB_CONNECTION_STRING`
   - **Value:** Your full connection string (the one you received from Azure Portal)
   - **Example:** `DefaultEndpointsProtocol=https;AccountName=bridesideimages;AccountKey=YOUR_ACCOUNT_KEY_HERE;EndpointSuffix=core.windows.net`

3. **Optional - Set container name:**
   - **Name:** `AZURE_STORAGE_BLOB_CONTAINER_NAME`
   - **Value:** `call-screenshots` (or your preferred container name)

4. **Click "Save"** and **restart your App Service**

5. **Note:** The container will be created automatically if it doesn't exist. You don't need to create it manually in Azure Portal.

## API Endpoint

### Upload Screenshot
- **Method:** `POST`
- **URL:** `/api/activities/{activityId}/upload-screenshot`
- **Content-Type:** `multipart/form-data`
- **Body:**
  - `file` (required): The image file (PNG, JPEG, etc.)

**Example Request (cURL):**
```bash
curl -X POST \
  https://your-backend-url/api/activities/123/upload-screenshot \
  -H "Authorization: Bearer YOUR_JWT_TOKEN" \
  -F "file=@/path/to/screenshot.png"
```

**Example Request (JavaScript/FormData):**
```javascript
const formData = new FormData();
formData.append('file', fileInput.files[0]);

const response = await fetch(`/api/activities/${activityId}/upload-screenshot`, {
  method: 'POST',
  headers: {
    'Authorization': `Bearer ${token}`
  },
  body: formData
});

const result = await response.json();
if (result.success) {
  const blobUrl = result.data; // Use this URL to update the activity
  console.log('Screenshot uploaded:', blobUrl);
}
```

**Response:**
```json
{
  "success": true,
  "message": "Screenshot uploaded successfully",
  "data": "https://bridesideimages.blob.core.windows.net/call-screenshots/1234567890-abc12345-screenshot.png"
}
```

## Features

- **Automatic container creation:** The container will be created automatically if it doesn't exist
- **Unique file names:** Files are named with timestamp and UUID to prevent conflicts
- **File validation:** Only image files are accepted (PNG, JPEG, etc.)
- **Size limit:** Maximum file size is 10MB
- **Automatic activity update:** The activity's `attachmentUrl` field is automatically updated with the uploaded file URL

## File Naming Convention

Uploaded files are stored with the following naming pattern:
```
{timestamp}-{uuid}-{originalFileName}
```

Example:
```
1701234567890-a1b2c3d4-screenshot.png
```

Files are stored in the `call-screenshots` container.

## Error Handling

The API will return appropriate error responses:
- `400 Bad Request`: Invalid file type or file too large
- `404 Not Found`: Activity not found
- `503 Service Unavailable`: Azure Blob Storage not configured
- `500 Internal Server Error`: Upload failed

## Security Notes

1. **Container Access:** If you set the container to **Private**, you'll need to generate SAS tokens for accessing the images, or configure the container to allow public read access for blobs.

2. **Connection String Security:** Never commit the connection string to version control. Always use environment variables.

3. **File Validation:** The API validates that only image files are uploaded and enforces a 10MB size limit.

## Testing

After setting up the environment variables and restarting the application:

1. Upload a screenshot using the API endpoint
2. Check the activity's `attachmentUrl` field - it should contain the blob URL
3. Verify the image is accessible at the returned URL

