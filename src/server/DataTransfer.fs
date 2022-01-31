module DataTransfer

open System

type ReservationLink = ReservationLink of string

type ReservationType =
    | Free of maxQuantity: int * ReservationLink
    | Taken

module ReservationType =
    let free reservationsLeft (date: DateTimeOffset) slotNumber =
        Free (reservationsLeft, ReservationLink (sprintf "api/schedule/%d/%d/%d/%d" date.Year date.Month date.Day slotNumber))
    let isFree = function
        | Free _ -> true
        | Taken -> false

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
module Schedule =
    let tryGetFirstFreeDate schedule =
        schedule.Dates
        |> List.tryFind (fun date ->
            schedule.Entries
            |> List.exists (fun entry -> entry.StartTime.Date = date.Date && ReservationType.isFree entry.ReservationType)
        )
        |> Option.map (fun v -> v.Date)

type Subscriber = {
    Quantity: int
    Name: string
    MailAddress: string
    PhoneNumber: string
}
