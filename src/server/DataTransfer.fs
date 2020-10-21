module DataTransfer

open System

type ReservationType =
    | Free of reservationLink: string
    | Taken

type ScheduleEntry = {
    StartTime: TimeSpan
    ReservationType: ReservationType
}

type Schedule = {
    Date: DateTimeOffset
    Entries: ScheduleEntry list
}

type Subscriber = {
    Name: string
    MailAddress: string
}
