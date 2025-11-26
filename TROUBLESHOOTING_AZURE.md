# Troubleshooting Azure Blob Storage Connection String

## Error: "Invalid connection string"

This error means the environment variable is being read, but the connection string format is invalid.

### Common Issues:

1. **Extra quotes in the environment variable**
   - ❌ Wrong: `"DefaultEndpointsProtocol=https;..."`
   - ✅ Correct: `DefaultEndpointsProtocol=https;...`

2. **Special characters not properly handled**
   - The connection string contains `+`, `/`, `=` characters which should be preserved as-is

3. **Environment variable not properly set in IDE**

### How to Fix in IntelliJ IDEA:

1. **Go to Run → Edit Configurations...**

2. **Select your Spring Boot run configuration**

3. **Under "Environment variables", click the "..." button**

4. **Make sure the variable is set EXACTLY like this:**
   - **Name:** `AZURE_STORAGE_BLOB_CONNECTION_STRING`
   - **Value:** `DefaultEndpointsProtocol=https;AccountName=bridesideimages;AccountKey=YOUR_ACCOUNT_KEY_HERE;EndpointSuffix=core.windows.net`
   - **⚠️ IMPORTANT:** Do NOT add quotes around the value!

5. **Click OK → Apply → OK**

6. **Restart the application**

### Verify the Environment Variable is Set:

After restarting, check the application logs. You should see:
```
Initializing Azure Blob Storage with connection string (first 50 chars): DefaultEndpointsProtocol=https;AccountName=brideside...
```

If you see something different or an error, the environment variable might not be set correctly.

### Alternative: Set in Terminal (macOS/Linux)

```bash
export AZURE_STORAGE_BLOB_CONNECTION_STRING="DefaultEndpointsProtocol=https;AccountName=bridesideimages;AccountKey=YOUR_ACCOUNT_KEY_HERE;EndpointSuffix=core.windows.net"

# Then run
mvn spring-boot:run
```

**Note:** In terminal, you CAN use quotes around the value when exporting, but make sure there are no extra spaces.

### Check What's Being Read:

The updated code will now log the first 50 characters of the connection string it receives. Check your application logs to see what value is actually being read. If it doesn't start with `DefaultEndpointsProtocol=`, then the environment variable is not set correctly.

### Still Not Working?

1. **Check for hidden characters:** Copy the connection string directly from Azure Portal
2. **Verify the connection string format:** It should start with `DefaultEndpointsProtocol=`
3. **Check application logs:** Look for the log message showing the first 50 characters
4. **Try setting it in application.yml temporarily** (for testing only, don't commit):
   ```yaml
   azure:
     storage:
       blob:
         connection-string: DefaultEndpointsProtocol=https;AccountName=bridesideimages;AccountKey=YOUR_ACCOUNT_KEY_HERE;EndpointSuffix=core.windows.net
   ```

