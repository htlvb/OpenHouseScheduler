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
    ReservationLink: string option
    Name: UserInput<string>
    MailAddress: UserInput<string>
    BookingState: Deferred<unit>
}

type Model = Deferred<LoadedModel>

type Msg =
    | LoadSchedule
    | LoadScheduleResult of Result<Schedule, exn>
    | SetReservationLink of string option
    | SetName of string
    | SetMailAddress of string
    | Book
    | BookingResult of Result<unit, exn>

let init =
    let state = Deferred.HasNotStartedYet
    state, Cmd.ofMsg LoadSchedule

let loadSchedule = async {
    let! (schedule : Schedule) = Fetch.``get``("api/schedule", caseStrategy = CamelCase) |> Async.AwaitPromise
    return schedule
}

let book link (data: Subscriber) = async {
    let! (result : unit) = Fetch.``post``(link, data = data, caseStrategy = CamelCase) |> Async.AwaitPromise
    return result
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
            ReservationLink = None
            Name = None
            MailAddress = None
            BookingState = Deferred.HasNotStartedYet
        }, Cmd.none
    | LoadScheduleResult (Error e) ->
        Deferred.Failed e, Cmd.none
    | SetReservationLink link ->
        model
        |> Deferred.map (fun loadedModel ->
            { loadedModel with ReservationLink = link }
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
        | Deferred.Resolved ({ ReservationLink = Some reservationLink; Name = Some (name, true); MailAddress = Some (mailAddress, true) } as loadedModel) ->
            Deferred.Resolved { loadedModel with BookingState = Deferred.InProgress },
            Cmd.OfAsync.either (fun () -> book reservationLink { Name = name; MailAddress = mailAddress }) () (Ok >> BookingResult) (Error >> BookingResult)
        | _ -> model, Cmd.none
    | BookingResult (Ok ()) ->
        match model with
        | Deferred.Resolved loadedModel ->
            Deferred.Resolved { loadedModel with Name = None; MailAddress = None; BookingState = Deferred.Resolved () },
            Cmd.none
        | _ -> model, Cmd.none
    | BookingResult (Error e) ->
        match model with
        | Deferred.Resolved loadedModel ->
            Deferred.Resolved { loadedModel with BookingState = Deferred.Failed e },
            Cmd.none
        | _ -> model, Cmd.none

let schedule = React.functionComponent(fun () ->
    let (state, dispatch) = React.useElmish(init, update, [||])

    let header (date: System.DateTimeOffset option) =
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
                                            style.padding 10
                                            style.backgroundColor "white"
                                        ]
                                    ]
                                ]
                                Bulma.levelItem [
                                    Bulma.title.h1 [
                                        match date with
                                        | Some date ->
                                            Html.text (sprintf "Tag der offenen Tür %s" (date.ToString("dd.MM.yyyy")))
                                        | None ->
                                            Html.text (sprintf "Tag der offenen Tür")
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
        [
            header (Some loadedModel.Schedule.Date)
            Bulma.container [
                Bulma.section [
                    yield Bulma.label [ Html.text "Zeitpunkt" ]
                    let entriesByHour =
                        loadedModel.Schedule.Entries
                        |> List.groupBy (fun e -> e.StartTime.Hours)
                    for (_, entries) in entriesByHour ->
                        Bulma.buttons [
                            for entry in entries ->
                                Bulma.button.button [
                                    prop.text (sprintf "%02d:%02d" entry.StartTime.Hours entry.StartTime.Minutes)
                                    match entry.ReservationType with
                                    | Free link when loadedModel.ReservationLink = Some link ->
                                        yield! [
                                            prop.onClick (fun _ -> dispatch (SetReservationLink None))
                                            color.isSuccess
                                        ]
                                    | Free link ->
                                        yield! [
                                            prop.onClick (fun _ -> dispatch (SetReservationLink (Some link)))
                                        ]
                                    | Taken ->
                                        yield! [
                                            prop.disabled true
                                            color.isDanger
                                        ]
                                ]
                        ]
                    yield Html.form [
                        prop.onSubmit (fun e -> e.preventDefault(); dispatch Book)
                        prop.children [
                            Bulma.field.div [
                                Bulma.label [ Html.text "Name" ]
                                Bulma.control.div [
                                    control.hasIconsLeft
                                    control.hasIconsRight
                                    prop.children [
                                        Bulma.input.text [
                                            prop.placeholder "Bitte geben Sie Ihren Namen an"
                                            prop.value (loadedModel.Name |> Option.map fst |> Option.defaultValue "")
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
                            Bulma.field.div [
                                Bulma.label [ Html.text "E-Mail-Adresse" ]
                                Bulma.control.div [
                                    control.hasIconsLeft
                                    control.hasIconsRight
                                    prop.children [
                                        Bulma.input.text [
                                            prop.placeholder "Bitte geben Sie Ihre E-Mail-Adresse an"
                                            prop.value (loadedModel.MailAddress |> Option.map fst |> Option.defaultValue "")
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

                            Bulma.level [
                                Bulma.levelLeft [
                                    Bulma.levelItem [
                                        Bulma.button.button [
                                            prop.type' "submit"
                                            prop.text "Reservieren"
                                            color.isSuccess
                                            match loadedModel.ReservationLink, loadedModel.Name, loadedModel.MailAddress with
                                            | Some _, Some (_, true), Some (_, true) -> ()
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
