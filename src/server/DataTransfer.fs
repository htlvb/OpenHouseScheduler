module DataTransfer

open System

type ReservationLink = ReservationLink of string

type ReservationType =
    | Free of maxQuantity: int * ReservationLink
    | Taken

module ReservationType =
    let free reservationsLeft slotNumber =
        Free (reservationsLeft, ReservationLink (sprintf "api/schedule/%d" slotNumber))

type ScheduleEntry = {
    StartTime: TimeSpan
    ReservationType: ReservationType
}

type Schedule = {
    Date: DateTimeOffset
    ReservationStartTime: DateTimeOffset
    InfoText: string
    Entries: ScheduleEntry list
}

type Subscriber = {
    Quantity: int
    Name: string
    MailAddress: string
}
