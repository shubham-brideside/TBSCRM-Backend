-- Add index on organizations.category to optimize JOIN queries with activities table
-- This significantly improves performance when filtering activities by organization category
CREATE INDEX IF NOT EXISTS idx_organizations_category ON organizations(category);

