USE openhousescheduler

CREATE TABLE schedule
(
    ID INTEGER NOT NULL AUTO_INCREMENT,
    Time DATETIME NOT NULL,
    Quantity INTEGER NOT NULL,
    Name VARCHAR(255) NOT NULL,
    MailAddress VARCHAR(255) NOT NULL,
    PhoneNumber VARCHAR(255) NOT NULL,
    TimeStamp DATETIME NOT NULL,
    PRIMARY KEY (ID)
)