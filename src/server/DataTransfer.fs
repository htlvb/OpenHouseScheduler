module DataTransfer

open System

type ReservationLink = ReservationLink of string

type ReservationType =
    | Free of maxQuantity: int * ReservationLink
    | Taken

module ReservationType =
    let free reservationsLeft (date: DateTimeOffset) slotNumber =
        Free (reservationsLeft, ReservationLink (sprintf "api/schedule/%d/%d/%d/%d" date.Year date.Month date.Day slotNumber))

type ScheduleEntry = {
    StartTime: DateTimeOffset
    ReservationType: ReservationType
}

type Schedule = {
    Title: string
    Dates: DateTimeOffset list
    ReservationStartTime: DateTimeOffset
    InfoText: string
    Entries: ScheduleEntry list
}

type Subscriber = {
    Quantity: int
    Name: string
    MailAddress: string
}
