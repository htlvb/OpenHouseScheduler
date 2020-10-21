docker run -d -v "$PSScriptRoot\schema.sql:/docker-entrypoint-initdb.d/schema.sql" -e MYSQL_ROOT_PASSWORD=root123 -e MYSQL_DATABASE=open-house-scheduler -p 3306:3306 mysql
