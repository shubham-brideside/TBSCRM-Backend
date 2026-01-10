# Frontend Integration Guide - Activity Screenshot Upload

This guide explains how to integrate the Azure Blob Storage screenshot upload feature in your frontend application.

## API Endpoint

### Upload Screenshot
- **Method:** `POST`
- **URL:** `/api/activities/{activityId}/upload-screenshot`
- **Content-Type:** `multipart/form-data`
- **Authentication:** Required (Bearer Token)

## Request Format

### Path Parameters
- `activityId` (required): The ID of the activity to attach the screenshot to

### Form Data
- `file` (optional): The image file to upload
  - **Accepted formats:** PNG, JPEG, GIF, WebP, etc. (any image format)
  - **Maximum size:** 10MB
  - **Content-Type:** Must be an image (e.g., `image/png`, `image/jpeg`)
  - **Note:** The image attachment is optional. If no file is provided, the endpoint will return success without uploading anything.

## Response Format

### Success Response (200 OK)
```json
{
  "success": true,
  "message": "Screenshot uploaded successfully",
  "data": "https://bridesideimages.blob.core.windows.net/call-screenshots/1701234567890-abc12345-screenshot.png"
}
```

### Error Responses

#### 400 Bad Request - Invalid File Type
```json
{
  "success": false,
  "message": "Only image files are allowed. Received content type: application/pdf",
  "data": null
}
```

#### 400 Bad Request - File Too Large
```json
{
  "success": false,
  "message": "File size exceeds maximum allowed size of 10MB",
  "data": null
}
```

#### 200 OK - No File Provided (Optional)
```json
{
  "success": true,
  "message": "No image provided - attachment is optional",
  "data": null
}
```

#### 404 Not Found - Activity Not Found
```json
{
  "success": false,
  "message": "Activity not found: 999",
  "data": null
}
```

#### 503 Service Unavailable - Azure Not Configured
```json
{
  "success": false,
  "message": "Azure Blob Storage is not configured. Please set AZURE_STORAGE_BLOB_CONNECTION_STRING environment variable.",
  "data": null
}
```

#### 500 Internal Server Error
```json
{
  "success": false,
  "message": "Failed to upload screenshot: [error details]",
  "data": null
}
```

## Frontend Implementation Examples

### React/TypeScript Example

```typescript
import React, { useState } from 'react';

interface UploadScreenshotProps {
  activityId: number;
  onUploadSuccess?: (imageUrl: string) => void;
  onUploadError?: (error: string) => void;
}

const UploadScreenshot: React.FC<UploadScreenshotProps> = ({
  activityId,
  onUploadSuccess,
  onUploadError
}) => {
  const [uploading, setUploading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleFileChange = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (!file) return;

    // Validate file type
    if (!file.type.startsWith('image/')) {
      setError('Please select an image file');
      return;
    }

    // Validate file size (10MB = 10 * 1024 * 1024 bytes)
    const maxSize = 10 * 1024 * 1024;
    if (file.size > maxSize) {
      setError('File size must be less than 10MB');
      return;
    }

    await uploadScreenshot(file);
  };

  const uploadScreenshot = async (file: File) => {
    setUploading(true);
    setError(null);

    try {
      const formData = new FormData();
      formData.append('file', file);

      const token = localStorage.getItem('token'); // or your token storage method
      
      const response = await fetch(
        `${process.env.REACT_APP_API_URL || 'http://localhost:8080'}/api/activities/${activityId}/upload-screenshot`,
        {
          method: 'POST',
          headers: {
            'Authorization': `Bearer ${token}`
          },
          body: formData
        }
      );

      const result = await response.json();

      if (result.success) {
        const imageUrl = result.data;
        onUploadSuccess?.(imageUrl);
        // Optionally show success message
        alert('Screenshot uploaded successfully!');
      } else {
        const errorMessage = result.message || 'Failed to upload screenshot';
        setError(errorMessage);
        onUploadError?.(errorMessage);
      }
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to upload screenshot';
      setError(errorMessage);
      onUploadError?.(errorMessage);
    } finally {
      setUploading(false);
    }
  };

  return (
    <div>
      <input
        type="file"
        accept="image/*"
        onChange={handleFileChange}
        disabled={uploading}
        style={{ display: 'none' }}
        id="screenshot-upload"
      />
      <label htmlFor="screenshot-upload">
        <button disabled={uploading}>
          {uploading ? 'Uploading...' : 'Upload Screenshot'}
        </button>
      </label>
      {error && <div style={{ color: 'red' }}>{error}</div>}
    </div>
  );
};

export default UploadScreenshot;
```

### React Hook Example (Custom Hook)

```typescript
import { useState } from 'react';

interface UseScreenshotUploadResult {
  uploadScreenshot: (file: File) => Promise<string | null>;
  uploading: boolean;
  error: string | null;
}

export const useScreenshotUpload = (activityId: number): UseScreenshotUploadResult => {
  const [uploading, setUploading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const uploadScreenshot = async (file: File): Promise<string | null> => {
    setUploading(true);
    setError(null);

    try {
      // Validate file
      if (!file.type.startsWith('image/')) {
        throw new Error('Only image files are allowed');
      }

      const maxSize = 10 * 1024 * 1024; // 10MB
      if (file.size > maxSize) {
        throw new Error('File size must be less than 10MB');
      }

      const formData = new FormData();
      formData.append('file', file);

      const token = localStorage.getItem('token');
      const apiUrl = process.env.REACT_APP_API_URL || 'http://localhost:8080';

      const response = await fetch(
        `${apiUrl}/api/activities/${activityId}/upload-screenshot`,
        {
          method: 'POST',
          headers: {
            'Authorization': `Bearer ${token}`
          },
          body: formData
        }
      );

      const result = await response.json();

      if (!response.ok || !result.success) {
        throw new Error(result.message || 'Failed to upload screenshot');
      }

      return result.data; // Return the image URL
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to upload screenshot';
      setError(errorMessage);
      return null;
    } finally {
      setUploading(false);
    }
  };

  return { uploadScreenshot, uploading, error };
};
```

### Usage of the Hook

```typescript
import React, { useRef } from 'react';
import { useScreenshotUpload } from './hooks/useScreenshotUpload';

const ActivityDetail: React.FC<{ activityId: number }> = ({ activityId }) => {
  const fileInputRef = useRef<HTMLInputElement>(null);
  const { uploadScreenshot, uploading, error } = useScreenshotUpload(activityId);

  const handleFileSelect = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (!file) return;

    const imageUrl = await uploadScreenshot(file);
    if (imageUrl) {
      // Handle success - maybe update the activity state or show the image
      console.log('Screenshot uploaded:', imageUrl);
    }
  };

  return (
    <div>
      <input
        ref={fileInputRef}
        type="file"
        accept="image/*"
        onChange={handleFileSelect}
        disabled={uploading}
        style={{ display: 'none' }}
      />
      <button onClick={() => fileInputRef.current?.click()} disabled={uploading}>
        {uploading ? 'Uploading...' : 'Upload Screenshot'}
      </button>
      {error && <div className="error">{error}</div>}
    </div>
  );
};
```

### Vanilla JavaScript Example

```javascript
async function uploadScreenshot(activityId, file) {
  // Validate file
  if (!file.type.startsWith('image/')) {
    throw new Error('Only image files are allowed');
  }

  const maxSize = 10 * 1024 * 1024; // 10MB
  if (file.size > maxSize) {
    throw new Error('File size must be less than 10MB');
  }

  const formData = new FormData();
  formData.append('file', file);

  const token = localStorage.getItem('token');
  const apiUrl = 'http://localhost:8080'; // or your API URL

  try {
    const response = await fetch(
      `${apiUrl}/api/activities/${activityId}/upload-screenshot`,
      {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${token}`
        },
        body: formData
      }
    );

    const result = await response.json();

    if (result.success) {
      return result.data; // Return the image URL
    } else {
      throw new Error(result.message || 'Failed to upload screenshot');
    }
  } catch (error) {
    console.error('Upload error:', error);
    throw error;
  }
}

// Usage
const fileInput = document.getElementById('screenshot-input');
fileInput.addEventListener('change', async (event) => {
  const file = event.target.files[0];
  if (file) {
    try {
      const imageUrl = await uploadScreenshot(activityId, file);
      console.log('Screenshot uploaded:', imageUrl);
      // Update UI with the image URL
    } catch (error) {
      console.error('Upload failed:', error.message);
      alert(error.message);
    }
  }
});
```

## Displaying Uploaded Screenshots

### Display Image from URL

```typescript
interface ActivityScreenshotProps {
  imageUrl: string | null;
  activityId: number;
}

const ActivityScreenshot: React.FC<ActivityScreenshotProps> = ({ imageUrl, activityId }) => {
  if (!imageUrl) {
    return <div>No screenshot available</div>;
  }

  return (
    <div>
      <img 
        src={imageUrl} 
        alt={`Screenshot for activity ${activityId}`}
        style={{ maxWidth: '100%', height: 'auto' }}
        onError={(e) => {
          // Handle image load error
          console.error('Failed to load image:', imageUrl);
          e.currentTarget.style.display = 'none';
        }}
      />
    </div>
  );
};
```

### Display with Fallback

```typescript
const ActivityScreenshot: React.FC<ActivityScreenshotProps> = ({ imageUrl, activityId }) => {
  const [imageError, setImageError] = useState(false);

  if (!imageUrl || imageError) {
    return (
      <div className="screenshot-placeholder">
        <span>No screenshot available</span>
      </div>
    );
  }

  return (
    <div className="screenshot-container">
      <img 
        src={imageUrl} 
        alt={`Screenshot for activity ${activityId}`}
        onError={() => setImageError(true)}
        style={{ maxWidth: '100%', height: 'auto', borderRadius: '4px' }}
      />
    </div>
  );
};
```

## Complete Activity Component Example

```typescript
import React, { useState, useRef } from 'react';
import { useScreenshotUpload } from './hooks/useScreenshotUpload';

interface Activity {
  id: number;
  subject: string;
  attachmentUrl: string | null;
  // ... other fields
}

interface ActivityCardProps {
  activity: Activity;
  onUpdate?: (activity: Activity) => void;
}

const ActivityCard: React.FC<ActivityCardProps> = ({ activity, onUpdate }) => {
  const fileInputRef = useRef<HTMLInputElement>(null);
  const { uploadScreenshot, uploading, error } = useScreenshotUpload(activity.id);
  const [imageUrl, setImageUrl] = useState<string | null>(activity.attachmentUrl);

  const handleFileSelect = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (!file) return;

    const uploadedUrl = await uploadScreenshot(file);
    if (uploadedUrl) {
      setImageUrl(uploadedUrl);
      // Optionally update the activity in your state/backend
      if (onUpdate) {
        onUpdate({ ...activity, attachmentUrl: uploadedUrl });
      }
    }
  };

  return (
    <div className="activity-card">
      <h3>{activity.subject}</h3>
      
      {/* Screenshot Display */}
      {imageUrl && (
        <div className="screenshot-preview">
          <img 
            src={imageUrl} 
            alt={`Screenshot for ${activity.subject}`}
            style={{ maxWidth: '100%', height: 'auto' }}
          />
        </div>
      )}

      {/* Upload Button */}
      <div className="upload-section">
        <input
          ref={fileInputRef}
          type="file"
          accept="image/*"
          onChange={handleFileSelect}
          disabled={uploading}
          style={{ display: 'none' }}
        />
        <button 
          onClick={() => fileInputRef.current?.click()} 
          disabled={uploading}
          className="upload-button"
        >
          {uploading ? 'Uploading...' : imageUrl ? 'Replace Screenshot' : 'Upload Screenshot'}
        </button>
        {error && <div className="error-message">{error}</div>}
      </div>
    </div>
  );
};
```

## Drag and Drop Upload

```typescript
import React, { useState, useRef, DragEvent } from 'react';

const DragDropUpload: React.FC<{ activityId: number }> = ({ activityId }) => {
  const [isDragging, setIsDragging] = useState(false);
  const { uploadScreenshot, uploading, error } = useScreenshotUpload(activityId);

  const handleDragOver = (e: DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    setIsDragging(true);
  };

  const handleDragLeave = (e: DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    setIsDragging(false);
  };

  const handleDrop = async (e: DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    setIsDragging(false);

    const file = e.dataTransfer.files[0];
    if (file && file.type.startsWith('image/')) {
      await uploadScreenshot(file);
    }
  };

  return (
    <div
      onDragOver={handleDragOver}
      onDragLeave={handleDragLeave}
      onDrop={handleDrop}
      style={{
        border: isDragging ? '2px dashed blue' : '2px dashed gray',
        padding: '20px',
        textAlign: 'center',
        borderRadius: '4px'
      }}
    >
      {uploading ? (
        <div>Uploading...</div>
      ) : (
        <div>
          <p>Drag and drop an image here, or click to select</p>
          <input
            type="file"
            accept="image/*"
            onChange={(e) => {
              const file = e.target.files?.[0];
              if (file) uploadScreenshot(file);
            }}
          />
        </div>
      )}
      {error && <div style={{ color: 'red' }}>{error}</div>}
    </div>
  );
};
```

## Image Preview Before Upload

```typescript
const ImageUploadWithPreview: React.FC<{ activityId: number }> = ({ activityId }) => {
  const [preview, setPreview] = useState<string | null>(null);
  const [selectedFile, setSelectedFile] = useState<File | null>(null);
  const { uploadScreenshot, uploading, error } = useScreenshotUpload(activityId);

  const handleFileSelect = (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (!file) return;

    setSelectedFile(file);

    // Create preview
    const reader = new FileReader();
    reader.onloadend = () => {
      setPreview(reader.result as string);
    };
    reader.readAsDataURL(file);
  };

  const handleUpload = async () => {
    if (selectedFile) {
      await uploadScreenshot(selectedFile);
      // Clear preview after successful upload
      setPreview(null);
      setSelectedFile(null);
    }
  };

  return (
    <div>
      <input
        type="file"
        accept="image/*"
        onChange={handleFileSelect}
        disabled={uploading}
      />
      
      {preview && (
        <div>
          <img src={preview} alt="Preview" style={{ maxWidth: '200px' }} />
          <button onClick={handleUpload} disabled={uploading}>
            {uploading ? 'Uploading...' : 'Upload'}
          </button>
          <button onClick={() => {
            setPreview(null);
            setSelectedFile(null);
          }}>
            Cancel
          </button>
        </div>
      )}
      
      {error && <div style={{ color: 'red' }}>{error}</div>}
    </div>
  );
};
```

## Error Handling Best Practices

1. **Validate file before upload:**
   - Check file type (must be image)
   - Check file size (max 10MB)
   - Show user-friendly error messages

2. **Handle network errors:**
   - Show retry option
   - Display appropriate error messages

3. **Handle authentication errors:**
   - Redirect to login if token is invalid
   - Show appropriate message

4. **Show loading states:**
   - Disable upload button during upload
   - Show progress indicator

## Notes

- The uploaded image URL is automatically saved to the activity's `attachmentUrl` field
- Images are stored in Azure Blob Storage container: `call-screenshots`
- File names are automatically generated with timestamp and UUID to prevent conflicts
- The image URL returned can be used directly in `<img src={url} />` tags
- Make sure your Azure Blob Storage container has public read access if you want images to be viewable without authentication

## Testing

1. Test with valid image files (PNG, JPEG, etc.)
2. Test with invalid file types (PDF, DOC, etc.) - should show error
3. Test with files larger than 10MB - should show error
4. Test with no file selected - should show error
5. Test with invalid activity ID - should show 404 error
6. Test network errors - should handle gracefully

