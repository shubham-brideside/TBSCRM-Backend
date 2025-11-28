# Local Development Setup - Azure Blob Storage

## Setting Azure Blob Storage Connection String for Local Development

### Option 1: Set Environment Variable in Terminal (macOS/Linux)

Before running the application, export the environment variable:

```bash
export AZURE_STORAGE_BLOB_CONNECTION_STRING="DefaultEndpointsProtocol=https;AccountName=bridesideimages;AccountKey=YOUR_ACCOUNT_KEY_HERE;EndpointSuffix=core.windows.net"

# Then run the application
mvn spring-boot:run
```

### Option 2: Set Environment Variable in IDE (IntelliJ IDEA)

1. Go to **Run** → **Edit Configurations...**
2. Select your Spring Boot run configuration
3. Under **Environment variables**, click the **...** button
4. Click **+** to add a new variable
5. Add:
   - **Name:** `AZURE_STORAGE_BLOB_CONNECTION_STRING`
   - **Value:** `DefaultEndpointsProtocol=https;AccountName=bridesideimages;AccountKey=YOUR_ACCOUNT_KEY_HERE;EndpointSuffix=core.windows.net`
6. Click **OK** → **Apply** → **OK**
7. Restart the application

### Option 3: Create application-local.yml (For Local Testing Only)

Create a file `src/main/resources/application-local.yml`:

```yaml
azure:
  storage:
    blob:
      connection-string: DefaultEndpointsProtocol=https;AccountName=bridesideimages;AccountKey=YOUR_ACCOUNT_KEY_HERE;EndpointSuffix=core.windows.net
      container-name: call-screenshots
```

Then run with profile:
```bash
mvn spring-boot:run -Dspring-boot.run.profiles=local
```

**⚠️ WARNING:** Do NOT commit `application-local.yml` to version control if it contains sensitive information. Add it to `.gitignore`.

### Option 4: Add to .env file (if using Spring Boot with .env support)

Create a `.env` file in the project root:
```
AZURE_STORAGE_BLOB_CONNECTION_STRING=DefaultEndpointsProtocol=https;AccountName=bridesideimages;AccountKey=YOUR_ACCOUNT_KEY_HERE;EndpointSuffix=core.windows.net
```

## Verify Setup

After setting the environment variable and restarting:

1. Check application logs - you should see:
   ```
   Azure Blob Storage service initialized. Container: call-screenshots
   ```

2. Test the upload endpoint in Swagger UI or Postman

3. If the container doesn't exist, you'll see:
   ```
   Creating Azure Blob Storage container: call-screenshots
   ```

## Troubleshooting

- **Still getting "not configured" error?** Make sure you restarted the application after setting the environment variable
- **Connection string format:** Make sure there are no extra spaces or line breaks in the connection string
- **Check logs:** Look for any Azure-related errors in the application startup logs

