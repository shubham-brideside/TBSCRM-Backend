# Brideside CRM Backend

A Spring Boot-based CRM backend application with role-based access control (RBAC) and user invitation system.

## Features

- **Role-Based Access Control (RBAC)**: Admin, User, and Manager roles
- **JWT Authentication**: Secure token-based authentication
- **User Invitation System**: Admin can invite users via email
- **Password Setup Flow**: Invited users set their password via email link
- **RESTful API**: Clean API endpoints for all operations
- **Swagger Documentation**: Interactive API documentation
- **Frontend Testing Pages**: Sample HTML pages for testing the flow

## Tech Stack

- **Java 17**
- **Spring Boot 3.2.0**
- **Spring Security** with JWT
- **Spring Data JPA**
- **MySQL Database**
- **Swagger/OpenAPI 3**
- **Maven**

## Prerequisites

- Java 17 or higher
- Maven 3.6+
- MySQL 8.0+
- Email SMTP server (for sending invitation emails)

## Configuration

### Database Configuration

The database connection is configured in `application.yml`. Default credentials are provided:

```yaml
spring:
  datasource:
    url: jdbc:mysql://thebrideside.mysql.database.azure.com:3306/thebrideside?useSSL=true...
    username: thebrideside
    password: TheBride@260799
```

You can override these using environment variables:
- `DB_HOST`
- `DB_DATABASE`
- `DB_USER`
- `DB_PASSWORD`

### Email Configuration

Configure email settings in `application.yml` or via environment variables:

```yaml
spring:
  mail:
    host: smtp.gmail.com
    port: 587
    username: ${MAIL_USERNAME}
    password: ${MAIL_PASSWORD}
```

Environment variables:
- `MAIL_HOST` (default: smtp.gmail.com)
- `MAIL_PORT` (default: 587)
- `MAIL_USERNAME`
- `MAIL_PASSWORD`

### JWT Configuration

Set a strong secret key for JWT tokens in production:

```yaml
jwt:
  secret: ${JWT_SECRET:your-secret-key-change-this-in-production-use-a-long-random-string}
  expiration: 86400000 # 24 hours
```

### Frontend URL

Set the frontend URL for invitation links:

```yaml
app:
  frontend-url: ${FRONTEND_URL:http://localhost:3000}
```

## Getting Started

### 1. Clone the Repository

```bash
cd BridesideCRM_Backend
```

### 2. Configure Application Properties

Update `application.yml` with your database and email credentials.

### 3. Build the Project

```bash
mvn clean install
```

### 4. Run the Application

```bash
mvn spring-boot:run
```

The application will start on `http://localhost:8080`

### 5. Access the Application

- **API Documentation (Swagger)**: http://localhost:8080/swagger-ui.html
- **Frontend Pages**: http://localhost:8080/frontend/

## API Endpoints

### Public Endpoints

#### Create Admin (First Time Only)
```
POST /api/admin/create-admin
```

Request Body:
```json
{
  "email": "admin@example.com",
  "password": "password123",
  "firstName": "Admin",
  "lastName": "User"
}
```

#### Login
```
POST /api/auth/login
```

Request Body:
```json
{
  "email": "admin@example.com",
  "password": "password123"
}
```

Response:
```json
{
  "success": true,
  "message": "Login successful",
  "data": {
    "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "type": "Bearer",
    "userId": 1,
    "email": "admin@example.com",
    "firstName": "Admin",
    "lastName": "User",
    "role": "ADMIN"
  }
}
```

#### Set Password (Using Invitation Token)
```
POST /api/users/set-password
```

Request Body:
```json
{
  "token": "invitation-token-from-email",
  "password": "newpassword123",
  "confirmPassword": "newpassword123"
}
```

### Protected Endpoints (Require JWT Token)

#### Create User (Admin Only)
```
POST /api/users
Authorization: Bearer {token}
```

Request Body:
```json
{
  "email": "user@example.com",
  "firstName": "John",
  "lastName": "Doe",
  "role": "USER"
}
```

#### Get All Users (Admin Only)
```
GET /api/users
Authorization: Bearer {token}
```

#### Get User by ID (Admin Only)
```
GET /api/users/{id}
Authorization: Bearer {token}
```

## User Flow

### 1. Initial Setup
1. Access the application for the first time
2. Use `/api/admin/create-admin` to create the first admin user
3. Login with the admin credentials

### 2. Create Users (Admin)
1. Admin logs in and receives JWT token
2. Admin navigates to Users page
3. Admin creates a new user with email and role
4. System sends invitation email to the user

### 3. User Invitation Acceptance
1. User receives invitation email with link
2. User clicks on the link (e.g., `http://localhost:3000/accept-invitation?token=...`)
3. User is redirected to password setup page
4. User enters password and confirms
5. User account is activated
6. User can now login with their credentials

## Frontend Testing Pages

The application includes sample HTML pages for testing:

1. **Create Admin**: `/frontend/create-admin.html`
2. **Login**: `/frontend/login.html`
3. **Users Management**: `/frontend/users.html` (Admin only)
4. **Accept Invitation**: `/frontend/accept-invitation.html?token=...`

## Database Schema

### Users Table
- `id` (Primary Key)
- `email` (Unique)
- `password` (Encrypted)
- `first_name`
- `last_name`
- `role_id` (Foreign Key)
- `active` (Boolean)
- `password_set` (Boolean)
- `created_at`
- `updated_at`
- `last_login_at`

### Roles Table
- `id` (Primary Key)
- `name` (Enum: ADMIN, USER, MANAGER)
- `description`

### Invitation Tokens Table
- `id` (Primary Key)
- `token` (Unique UUID)
- `user_id` (Foreign Key)
- `expires_at`
- `used` (Boolean)
- `created_at`

## Security Features

- **JWT Token Authentication**: Secure token-based authentication
- **Password Encryption**: BCrypt password hashing
- **Role-Based Authorization**: Endpoints protected by role
- **CORS Configuration**: Configurable CORS settings
- **Exception Handling**: Comprehensive error handling with proper HTTP status codes

## Production Considerations

1. **Change JWT Secret**: Set a strong, random JWT secret key
2. **Database Security**: Use connection pooling and secure database credentials
3. **Email Configuration**: Use a production SMTP server
4. **HTTPS**: Enable HTTPS in production
5. **Logging**: Configure proper logging levels
6. **Environment Variables**: Use environment variables for sensitive data
7. **Rate Limiting**: Consider adding rate limiting for API endpoints
8. **Token Refresh**: Consider implementing token refresh mechanism

## Error Handling

The application includes comprehensive error handling:

- `400 Bad Request`: Invalid input data
- `401 Unauthorized`: Invalid credentials or token
- `403 Forbidden`: Insufficient permissions
- `404 Not Found`: Resource not found
- `500 Internal Server Error`: Server errors

All errors are returned in a consistent format:

```json
{
  "success": false,
  "message": "Error message",
  "data": null
}
```

## Testing with Swagger

1. Start the application
2. Navigate to http://localhost:8080/swagger-ui.html
3. Use the "Authorize" button to add your JWT token
4. Test all endpoints interactively

## Support

For issues or questions, please contact the development team.

## License

This project is proprietary software for Brideside CRM.

