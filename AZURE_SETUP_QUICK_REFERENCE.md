# Azure Blob Storage - Quick Setup Reference

## Your Storage Account Details
- **Storage Account Name:** `bridesideimages`
- **Container Name:** `call-screenshots` (will be auto-created)

## Steps to Configure in Azure App Service

### 1. Set Environment Variable

1. Go to **Azure Portal** → Your App Service
2. Navigate to **Configuration** → **Application settings**
3. Click **+ New application setting**
4. Add:
   - **Name:** `AZURE_STORAGE_BLOB_CONNECTION_STRING`
   - **Value:** `DefaultEndpointsProtocol=https;AccountName=bridesideimages;AccountKey=YOUR_ACCOUNT_KEY_HERE;EndpointSuffix=core.windows.net`
5. Click **OK**
6. Click **Save** at the top
7. **Restart your App Service**

### 2. Verify Setup

After restarting, check the application logs. You should see:
```
Azure Blob Storage service initialized. Container: call-screenshots
```

If the container doesn't exist, you'll see:
```
Creating Azure Blob Storage container: call-screenshots
```

### 3. Test the API

Once configured, you can test the upload endpoint:
```bash
POST /api/activities/{activityId}/upload-screenshot
Content-Type: multipart/form-data
Body: file=<image file>
```

## Container Access Settings

**Important:** To make uploaded images publicly accessible:

1. Go to Azure Portal → Storage Account `bridesideimages`
2. Navigate to **Containers** → `call-screenshots`
3. Click **Change access level**
4. Set **Public access level** to **Blob** (anonymous read access for blobs only)
5. Click **OK**

This allows anyone with the URL to view the images. If you need private access, you'll need to implement SAS token generation.

## Security Reminder

⚠️ **Never commit the connection string to version control!** It should only exist in:
- Azure App Service environment variables
- Local development environment variables (for testing)

