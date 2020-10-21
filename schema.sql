USE open-house-scheduler

CREATE TABLE schedule
(
    SlotNumber INT NOT NULL,
    Name VARCHAR(255) NOT NULL,
    MailAddress VARCHAR(255) NOT NULL,
    TimeStamp TIMESTAMP NOT NULL,
    PRIMARY KEY (SlotNumber)
)