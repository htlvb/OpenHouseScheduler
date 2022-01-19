module Main

open Browser.Dom
open DataTransfer
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.UseDeferred
open Feliz.UseElmish
open Thoth.Fetch
open Thoth.Json

importAll "./styles/main.scss"

type UserInput<'a> = ('a * bool) option

type LoadedModel = {
    Schedule: Schedule
    SelectedDate: System.DateTime
    SelectedScheduleEntry: ScheduleEntry option
    Quantity: UserInput<int>
    Name: UserInput<string>
    MailAddress: UserInput<string>
    BookingState: Deferred<unit>
}

type Model = Deferred<LoadedModel>

type Msg =
    | LoadSchedule
    | LoadScheduleResult of Result<Schedule, exn>
    | SelectDate of System.DateTime
    | SelectScheduleEntry of ScheduleEntry option
    | SetQuantity of int
    | SetName of string
    | SetMailAddress of string
    | Book
    | BookingResult of Result<ScheduleEntry * ReservationType, exn>

let init =
    let state = Deferred.HasNotStartedYet
    state, Cmd.ofMsg LoadSchedule

let loadSchedule = async {
    let! (schedule : Schedule) = Fetch.``get``("api/schedule", caseStrategy = CamelCase) |> Async.AwaitPromise
    return schedule
}

let book (ReservationLink reservationLink) (data: Subscriber) = async {
    let! (newReservationType: ReservationType) = Fetch.``post``(reservationLink, data = data, caseStrategy = CamelCase) |> Async.AwaitPromise
    return newReservationType
}

let isValidMailAddress v =
    // see http://emailregex.com/
    System.Text.RegularExpressions.Regex.IsMatch(v, @"^\w+([-+.']\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*$")

let update msg model =
    match msg with
    | LoadSchedule ->
        Deferred.InProgress,
        Cmd.OfAsync.either (fun () -> loadSchedule) () (Ok >> LoadScheduleResult) (Error >> LoadScheduleResult)
    | LoadScheduleResult (Ok schedule) ->
        Deferred.Resolved {
            Schedule = schedule
            SelectedDate = schedule.Dates.Head.Date
            SelectedScheduleEntry = None
            Quantity = None
            Name = None
            MailAddress = None
            BookingState = Deferred.HasNotStartedYet
        }, Cmd.none
    | LoadScheduleResult (Error e) ->
        Deferred.Failed e, Cmd.none
    | SelectDate date ->
        model
        |> Deferred.map (fun loadedModel ->
            { loadedModel with
                SelectedDate = date
                SelectedScheduleEntry = None
            }
        ), Cmd.none
    | SelectScheduleEntry scheduleEntry ->
        model
        |> Deferred.map (fun loadedModel ->
            { loadedModel with
                SelectedScheduleEntry = scheduleEntry
                Quantity =
                    match scheduleEntry, loadedModel.Quantity with
                    | Some { ReservationType = Free (maxQuantity, _reservationLink) }, Some (quantity, true) when maxQuantity >= quantity -> loadedModel.Quantity
                    | None, _ -> loadedModel.Quantity
                    | _ -> None
            }
        ), Cmd.none
    | SetQuantity quantity ->
        model
        |> Deferred.map (fun loadedModel ->
            { loadedModel with Quantity = Some (quantity, true) }
        ), Cmd.none
    | SetName name ->
        model
        |> Deferred.map (fun loadedModel ->
            { loadedModel with Name = Some (name, not <| System.String.IsNullOrWhiteSpace name) }
        ), Cmd.none
    | SetMailAddress mailAddress ->
        model
        |> Deferred.map (fun loadedModel ->
            { loadedModel with MailAddress = Some (mailAddress, isValidMailAddress mailAddress) }
        ), Cmd.none
    | Book ->
        match model with
        | Deferred.Resolved ({ SelectedScheduleEntry = Some ({ ReservationType = Free (maxQuantity, reservationLink) } as selectedScheduleEntry); Quantity = Some (quantity, true); Name = Some (name, true); MailAddress = Some (mailAddress, true) } as loadedModel) when maxQuantity >= quantity ->
            Deferred.Resolved { loadedModel with BookingState = Deferred.InProgress },
            Cmd.OfAsync.either (fun () -> book reservationLink { Quantity = quantity; Name = name; MailAddress = mailAddress }) () (fun reservationType -> Ok (selectedScheduleEntry, reservationType) |> BookingResult) (Error >> BookingResult)
        | _ -> model, Cmd.none
    | BookingResult (Ok (selectedScheduleEntry, newReservationType)) ->
        match model with
        | Deferred.Resolved loadedModel ->
            let scheduleEntries =
                loadedModel.Schedule.Entries
                |> List.map (fun entry ->
                    if entry = selectedScheduleEntry then { entry with ReservationType = newReservationType }
                    else entry
                )
            Deferred.Resolved {
                loadedModel with
                    Schedule = { loadedModel.Schedule with Entries = scheduleEntries }
                    SelectedScheduleEntry = None
                    Quantity = None
                    Name = None
                    MailAddress = None
                    BookingState = Deferred.Resolved ()
            },
            Cmd.none
        | _ -> model, Cmd.none
    | BookingResult (Error e) ->
        match model with
        | Deferred.Resolved loadedModel ->
            Deferred.Resolved { loadedModel with BookingState = Deferred.Failed e },
            Cmd.none
        | _ -> model, Cmd.none

let formatDate (date: System.DateTimeOffset) =
    if date.TimeOfDay = System.TimeSpan.Zero then date.ToString("dd.MM.yyyy")
    else date.ToString("dd.MM.yyyy HH:mm:ss")

let schedule = React.functionComponent(fun () ->
    let (state, dispatch) = React.useElmish(init, update, [||])

    let header data =
        Bulma.hero [
            color.isPrimary
            prop.children [
                Bulma.heroBody [
                    Bulma.container [
                        Bulma.level [
                            Bulma.levelLeft [
                                Bulma.levelItem [
                                    Html.img [
                                        prop.src "img/logo.svg"
                                        prop.style [
                                            style.width 120
                                        ]
                                    ]
                                ]
                                Bulma.levelItem [
                                    Bulma.title.h1 [
                                        match data with
                                        | Some (title, [date: System.DateTimeOffset]) ->
                                            Html.text (sprintf "%s am %s" title (date.ToString("dd.MM.yyyy")))
                                        | Some (title, dates: System.DateTimeOffset list) ->
                                            let startDate = List.min dates
                                            let endDate = List.max dates
                                            Html.text (sprintf "%s von %s bis %s" title (startDate.ToString("dd.MM.yyyy")) (endDate.ToString("dd.MM.yyyy")))
                                        | None ->
                                            ()
                                    ]
                                ]
                            ]
                        ]
                        Bulma.title.h2 [
                            Html.text ("Reservierung")
                        ]
                    ]
                ]
            ]
        ]

    match state with
    | Deferred.HasNotStartedYet -> [ header None ]
    | Deferred.InProgress -> [ header None; View.loadIconBig ]
    | Deferred.Failed _ -> [ header None; View.errorNotificationWithRetry "Fehler beim Laden des Zeitplans" (fun () -> dispatch LoadSchedule) ]
    | Deferred.Resolved loadedModel ->
        let isReservationEnabled = loadedModel.Schedule.ReservationStartTime <= System.DateTimeOffset.UtcNow
        [
            header (Some (loadedModel.Schedule.Title, loadedModel.Schedule.Dates))
            Bulma.container [
                Bulma.section [
                    prop.innerHtml loadedModel.Schedule.InfoText
                ]
                if not isReservationEnabled then
                    Bulma.section [
                        View.errorNotificationWithRetry (sprintf "Die Reservierung ist ab %s möglich" (formatDate loadedModel.Schedule.ReservationStartTime)) (fun () -> dispatch LoadSchedule)
                    ]
                Bulma.section [
                    Html.form [
                        prop.onSubmit (fun e -> e.preventDefault(); dispatch Book)
                        prop.children [
                            yield Bulma.label [ Html.text "Zeitpunkt / freie Plätze" ]
                            let groupedEntries =
                                loadedModel.Schedule.Entries
                                |> List.groupBy (fun e -> e.StartTime.Date)
                                |> List.map (fun (key, entries) ->
                                    let entriesByHour =
                                        entries
                                        |> List.groupBy (fun e -> e.StartTime.TimeOfDay.Hours)
                                    (key, entriesByHour)
                                )
                            yield Bulma.buttons [
                                for (date, _) in groupedEntries ->
                                    Bulma.button.a [
                                        prop.text (date.ToString("dd.MM.yyyy"))
                                        button.isLarge
                                        prop.onClick (fun _ -> dispatch (SelectDate date))
                                        if loadedModel.SelectedDate = date then
                                            color.isSuccess
                                    ]
                            ]
                            yield Html.hr []
                            for (_, entries) in groupedEntries |> List.find (fun (date, _) -> date = loadedModel.SelectedDate) |> snd ->
                                Bulma.buttons [
                                    for entry in entries ->
                                        let isDisabled = not isReservationEnabled || entry.StartTime < System.DateTimeOffset.Now || entry.ReservationType = Taken
                                        let text = sprintf "%02d:%02d" entry.StartTime.TimeOfDay.Hours entry.StartTime.TimeOfDay.Minutes
                                        Bulma.button.a [
                                            prop.disabled isDisabled
                                            match entry.ReservationType with
                                            | Free (maxQuantity, _link) when loadedModel.SelectedScheduleEntry = Some entry ->
                                                yield! [
                                                    prop.text (sprintf "%s | %d P." text maxQuantity)
                                                    prop.onClick (fun _ -> dispatch (SelectScheduleEntry None))
                                                    color.isSuccess
                                                ]
                                            | Free (maxQuantity, _link) ->
                                                yield! [
                                                    prop.text (sprintf "%s | %d P." text maxQuantity)
                                                    prop.onClick (fun _ -> dispatch (SelectScheduleEntry (Some entry)))
                                                ]
                                            | Taken ->
                                                yield! [
                                                    prop.text (sprintf "%s | 0 P." text)
                                                    color.isDanger
                                                ]
                                        ]
                                ]
                    
                            yield Bulma.field.div [
                                Bulma.label [ Html.text "Anzahl Personen" ]
                                Bulma.buttons [
                                    let maxQuantity =
                                        match loadedModel.SelectedScheduleEntry with
                                        | Some { ReservationType = Free (maxQuantity, _reservationLink) } -> maxQuantity
                                        | _ ->
                                            loadedModel.Schedule.Entries
                                            |> List.choose (fun entry ->
                                                match entry.ReservationType with
                                                | Free (maxQuantity, _reservationLink) -> Some maxQuantity
                                                | _ -> None
                                            )
                                            |> List.max
                                    for quantity in [1..maxQuantity] ->
                                        Bulma.button.a [
                                            prop.onClick (fun _ -> dispatch (SetQuantity quantity))
                                            match loadedModel.Quantity with
                                            | Some (c, true) when c = quantity -> color.isSuccess
                                            | _ -> ()
                                            prop.textf "%d" quantity
                                        ]
                                ]
                            ]
                            yield Bulma.field.div [
                                Bulma.label [ Html.text "Name" ]
                                Bulma.control.div [
                                    control.hasIconsLeft
                                    control.hasIconsRight
                                    prop.children [
                                        Bulma.input.text [
                                            prop.placeholder "Bitte geben Sie Ihren Namen an"
                                            prop.value (loadedModel.Name |> Option.map fst |> Option.defaultValue "")
                                            prop.disabled (not isReservationEnabled)
                                            match loadedModel.Name with
                                            | Some (_, true) -> color.isSuccess
                                            | Some _ -> color.isDanger
                                            | None -> ()
                                            prop.onChange (fun (e: Browser.Types.Event) -> dispatch (SetName e.target?value))
                                        ]
                                        Bulma.icon [
                                            icon.isSmall
                                            icon.isLeft
                                            prop.children [
                                                Fa.i [ Fa.Solid.User ] []
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                            yield Bulma.field.div [
                                Bulma.label [ Html.text "E-Mail-Adresse" ]
                                Bulma.control.div [
                                    control.hasIconsLeft
                                    control.hasIconsRight
                                    prop.children [
                                        Bulma.input.text [
                                            prop.placeholder "Bitte geben Sie Ihre E-Mail-Adresse an"
                                            prop.value (loadedModel.MailAddress |> Option.map fst |> Option.defaultValue "")
                                            prop.disabled (not isReservationEnabled)
                                            match loadedModel.MailAddress with
                                            | Some (_, true) -> color.isSuccess
                                            | Some _ -> color.isDanger
                                            | None -> ()
                                            prop.onChange (fun (e: Browser.Types.Event) -> dispatch (SetMailAddress e.target?value))
                                        ]
                                        Bulma.icon [
                                            icon.isSmall
                                            icon.isLeft
                                            prop.children [
                                                Fa.i [ Fa.Solid.Envelope ] []
                                            ]
                                        ]
                                    ]
                                ]
                            ]

                            yield Bulma.level [
                                Bulma.levelLeft [
                                    Bulma.levelItem [
                                        Bulma.button.button [
                                            prop.type' "submit"
                                            prop.text "Reservieren"
                                            color.isSuccess
                                            match isReservationEnabled, loadedModel.SelectedScheduleEntry, loadedModel.Quantity, loadedModel.Name, loadedModel.MailAddress with
                                            | true, Some _, Some (_, true), Some (_, true), Some (_, true) -> ()
                                            | _ -> prop.disabled true
                                            if Deferred.inProgress loadedModel.BookingState then button.isLoading
                                        ]
                                    ]
                                    match loadedModel.BookingState with
                                    | Deferred.Resolved () ->
                                        Bulma.levelItem [
                                            color.hasTextSuccess
                                            prop.children [
                                                Html.text "Ihre Reservierung wurde erfolgreich gespeichert. Sie erhalten in Kürze eine Bestätigung per Mail."
                                            ]
                                        ]
                                    | Deferred.Failed _ ->
                                        Bulma.levelItem [
                                            color.hasTextDanger
                                            prop.children [
                                                Html.text "Fehler beim Reservieren. Bitte versuchen Sie es erneut bzw. laden sie die Seite neu."
                                            ]
                                        ]
                                    | _ -> ()
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
)

ReactDOM.render(schedule, document.getElementById "feliz-app")
