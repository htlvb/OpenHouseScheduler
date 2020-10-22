USE openhousescheduler

CREATE TABLE schedule
(
    Time TIME NOT NULL,
    Name VARCHAR(255) NOT NULL,
    MailAddress VARCHAR(255) NOT NULL,
    TimeStamp TIMESTAMP NOT NULL,
    PRIMARY KEY (Time)
)