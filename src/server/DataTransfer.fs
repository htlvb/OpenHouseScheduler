module DataTransfer

open System

type ReservationLink = ReservationLink of string

type ReservationType =
    | Free of ReservationLink
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
