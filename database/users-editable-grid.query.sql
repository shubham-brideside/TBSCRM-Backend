-- IntelliJ / DataGrip: include primary key `id` in SELECT or the row editor breaks.
-- Column for category-manager vertical FK: user_managed_category_id

SELECT id,
       user_managed_category_id,
       role_id,
       email,
       first_name,
       last_name
FROM users;
