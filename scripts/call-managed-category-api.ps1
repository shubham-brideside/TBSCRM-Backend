# Login as category manager, then PATCH /api/users/me/managed-category
# Usage (PowerShell):
#   $env:CM_EMAIL = "you@example.com"
#   $env:CM_PASSWORD = "your-password"
#   $env:MANAGED_CATEGORY_ID = "4"   # categories.id from your DB
#   $env:API_BASE = "http://localhost:8080"   # optional
#   .\scripts\call-managed-category-api.ps1

$ErrorActionPreference = "Stop"
$base = if ($env:API_BASE) { $env:API_BASE.TrimEnd("/") } else { "http://localhost:8080" }
$email = $env:CM_EMAIL
$password = $env:CM_PASSWORD
$catId = $env:MANAGED_CATEGORY_ID

if (-not $email -or -not $password) {
    Write-Host "Set environment variables CM_EMAIL and CM_PASSWORD, then run this script again." -ForegroundColor Yellow
    exit 1
}
if (-not $catId) {
    Write-Host "Set MANAGED_CATEGORY_ID (e.g. categories.id from SELECT id,name FROM categories)." -ForegroundColor Yellow
    exit 1
}

$loginBody = @{ email = $email; password = $password } | ConvertTo-Json
$login = Invoke-RestMethod -Uri "$base/api/auth/login" -Method Post -ContentType "application/json; charset=utf-8" -Body $loginBody
$token = $login.data.token
if (-not $token) {
    Write-Host "Login response missing data.token: $($login | ConvertTo-Json -Depth 5)" -ForegroundColor Red
    exit 1
}

$patchBody = @{ managedCategoryId = [long]$catId } | ConvertTo-Json
$headers = @{
    Authorization = "Bearer $token"
    "Content-Type"  = "application/json; charset=utf-8"
}
$patch = Invoke-RestMethod -Uri "$base/api/users/me/managed-category" -Method Patch -Headers $headers -Body $patchBody
Write-Host "OK: $($patch.message)" -ForegroundColor Green
Write-Host ($patch.data | ConvertTo-Json -Depth 5)
