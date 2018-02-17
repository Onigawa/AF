
CREATE TABLE event
{
id INT PRIMARY KEY NOT NULL
start_date DATETIME
end_date DATETIME
name VARCHAR(100)
location VARCHAR(100)
participants_id INT
}

CREATE TABLE person
{
person_id INT PRIMARY KEY NOT NULL
firstname VARCHAR(100)
lastname VARCHAR(100)
mail VARCHAR(100)
phone VARCHAR(100)
profile_type VARCHAR(100)
sector VARCHAR(100)
job VARCHAR(100)
calendar_export VARCHAR(255)
school VARCHAR(100)
promotion INT
}
