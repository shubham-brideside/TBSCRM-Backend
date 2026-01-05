-- ============================================================================
-- Generate 500 Persons (Contacts) with realistic but fictional data
-- ============================================================================
-- This script creates 500 persons with:
--   - Unique emails and phone numbers
--   - Proper linking to Organization (1 for Photography, 7 for Makeup)
--   - Proper linking to Category (1 for Photography, 2 for Makeup)
--   - Proper linking to Owner (13 for Photography, 48 for Makeup)
--   - Realistic Indian names, venues, and contact information
--   - Unique Instagram IDs (different from names)
--   - Random lead dates within the last 365 days
--   - person_source: INSTAGRAM, WHATSAPP, TBS_WEBSITE (old PersonSource enum values)
--   - source: DIRECT, DIVERT, REFERENCE, PLANNER (DealSource enum - used by API for filtering)
--
-- IMPORTANT: Before running this script, ensure:
--   1. Organizations with IDs 1 and 7 exist
--   2. Categories with IDs 1 and 2 exist
--   3. Users with IDs 13 and 48 exist
--   4. You have write permissions on the persons table
--
-- To run: Execute this script in your MySQL client or database tool
-- ============================================================================

-- First names pool
SET @first_names = 'Priya,Ananya,Riya,Kavya,Aditi,Meera,Shreya,Sneha,Divya,Pooja,Arjun,Rohan,Karan,Vikram,Rahul,Aryan,Aditya,Siddharth,Neeraj,Varun';

-- Last names pool
SET @last_names = 'Sharma,Patel,Singh,Kumar,Gupta,Mehta,Joshi,Reddy,Rao,Verma,Malhotra,Kapoor,Chopra,Shah,Agarwal';

-- Venue names pool
SET @venues = 'Taj Palace Hotel,The Oberoi,ITC Maurya,The Leela Palace,Hyatt Regency,Marriott Hotel,Radisson Blu,The Grand,The Ashok,Conrad Hotel,The Imperial,The Claridges,Shangri-La,The Ritz-Carlton,Four Seasons,The Westin,Novotel,The Lalit,The Park,ITC Maratha';

-- Instagram username prefixes
SET @instagram_prefixes = 'weddingdreams,bridalbliss,foreverlove,perfectday,celebrateus,ourjourney,lovestory,blissfulwedding,happilyever,weddingvibes,bridalbeauty,weddingmagic,celebratelove,weddinggoals,perfectmoment';

-- Generate 500 persons
INSERT INTO persons (
    name,
    instagram_id,
    phone,
    email,
    venue,
    person_source,
    source,
    lead_date,
    organization_id,
    category_id,
    owner_id,
    is_deleted,
    created_at,
    updated_at
)
SELECT
    -- Generate name
    CONCAT(
        SUBSTRING_INDEX(SUBSTRING_INDEX(@first_names, ',', 1 + ((row_num - 1) % 20)), ',', -1),
        ' ',
        SUBSTRING_INDEX(SUBSTRING_INDEX(@last_names, ',', 1 + ((row_num * 7) % 15)), ',', -1),
        CASE WHEN row_num % 50 = 0 THEN CONCAT(' ', CHAR(65 + (row_num % 26))) ELSE '' END
    ) AS name,
    
    -- Generate unique Instagram ID (different from name)
    CONCAT(
        SUBSTRING_INDEX(SUBSTRING_INDEX(@instagram_prefixes, ',', 1 + ((row_num * 3) % 15)), ',', -1),
        '_',
        LPAD(row_num, 4, '0'),
        CASE WHEN row_num % 3 = 0 THEN CONCAT('_', CHAR(97 + (row_num % 26))) ELSE '' END
    ) AS instagram_id,
    
    -- Generate unique phone number (Indian format)
    CONCAT(
        '+91 ',
        CASE 
            WHEN row_num % 3 = 0 THEN '9'
            WHEN row_num % 3 = 1 THEN '8'
            ELSE '7'
        END,
        LPAD(800000000 + row_num, 9, '0')
    ) AS phone,
    
    -- Generate unique email
    CONCAT(
        LOWER(REPLACE(
            CONCAT(
                SUBSTRING_INDEX(SUBSTRING_INDEX(@first_names, ',', 1 + ((row_num * 5) % 20)), ',', -1),
                SUBSTRING_INDEX(SUBSTRING_INDEX(@last_names, ',', 1 + ((row_num * 11) % 15)), ',', -1),
                row_num
            ),
            ' ', ''
        )),
        '@',
        CASE 
            WHEN row_num % 4 = 0 THEN 'gmail.com'
            WHEN row_num % 4 = 1 THEN 'yahoo.com'
            WHEN row_num % 4 = 2 THEN 'outlook.com'
            ELSE 'hotmail.com'
        END
    ) AS email,
    
    -- Generate venue
    SUBSTRING_INDEX(SUBSTRING_INDEX(@venues, ',', 1 + ((row_num * 13) % 20)), ',', -1) AS venue,
    
    -- Generate person_source (using old PersonSource enum values)
    CASE 
        WHEN row_num % 3 = 0 THEN 'INSTAGRAM'
        WHEN row_num % 3 = 1 THEN 'WHATSAPP'
        ELSE 'TBS_WEBSITE'
    END AS person_source,
    
    -- Generate source (DealSource enum values - this is what the API filters by)
    CASE 
        WHEN row_num % 4 = 0 THEN 'DIRECT'
        WHEN row_num % 4 = 1 THEN 'DIVERT'
        WHEN row_num % 4 = 2 THEN 'REFERENCE'
        ELSE 'PLANNER'
    END AS source,
    
    -- Generate lead_date (random date within last 365 days)
    DATE_SUB(CURDATE(), INTERVAL ((row_num * 17) % 365) DAY) AS lead_date,
    
    -- Set organization_id: 1 for photography, 7 for makeup (alternating)
    CASE 
        WHEN row_num % 2 = 0 THEN 1  -- Photography
        ELSE 7  -- Makeup
    END AS organization_id,
    
    -- Set category_id: 1 for photography, 2 for makeup
    CASE 
        WHEN row_num % 2 = 0 THEN 1  -- Photography
        ELSE 2  -- Makeup
    END AS category_id,
    
    -- Set owner_id: 13 if photography, 48 if makeup
    CASE 
        WHEN row_num % 2 = 0 THEN 13  -- Photography owner
        ELSE 48  -- Makeup owner
    END AS owner_id,
    
    -- Set is_deleted: FALSE (for BIT(1) column)
    FALSE AS is_deleted,
    
    -- Generate created_at (random timestamp within last 365 days)
    DATE_SUB(NOW(), INTERVAL ((row_num * 19) % 365) DAY) + INTERVAL ((row_num * 23) % 86400) SECOND AS created_at,
    
    -- Generate updated_at (same as created_at or slightly later)
    DATE_SUB(NOW(), INTERVAL ((row_num * 19) % 365) DAY) + INTERVAL ((row_num * 23) % 86400) SECOND AS updated_at

FROM (
    SELECT 
        (@row_number := @row_number + 1) AS row_num
    FROM 
        (SELECT @row_number := 0) r
        CROSS JOIN
        (SELECT 0 UNION SELECT 1 UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 UNION SELECT 5 UNION SELECT 6 UNION SELECT 7 UNION SELECT 8 UNION SELECT 9) t1
        CROSS JOIN
        (SELECT 0 UNION SELECT 1 UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 UNION SELECT 5 UNION SELECT 6 UNION SELECT 7 UNION SELECT 8 UNION SELECT 9) t2
        CROSS JOIN
        (SELECT 0 UNION SELECT 1 UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 UNION SELECT 5) t3
    LIMIT 500
) numbers;

-- Verify the count
SELECT COUNT(*) AS total_persons_created FROM persons WHERE is_deleted = 0;

-- Show sample of created persons
SELECT 
    id,
    name,
    instagram_id,
    phone,
    email,
    venue,
    person_source,
    lead_date,
    organization_id,
    category_id,
    owner_id,
    is_deleted,
    created_at
FROM persons 
WHERE is_deleted = 0 
ORDER BY id DESC 
LIMIT 10;

-- Verify no duplicate emails
SELECT email, COUNT(*) as count 
FROM persons 
WHERE is_deleted = 0 
GROUP BY email 
HAVING COUNT(*) > 1;

-- Verify no duplicate phone numbers
SELECT phone, COUNT(*) as count 
FROM persons 
WHERE is_deleted = 0 
GROUP BY phone 
HAVING COUNT(*) > 1;
