module App.Program

open DataTransfer
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Giraffe.Serialization
open Markdig
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open System
open System.Globalization
open System.Net.Mail
open Thoth.Json.Giraffe
open Thoth.Json.Net

// ---------------------------------
// Web app
// ---------------------------------

type MailConfig = {
    SmtpAddress: string
    MailboxUserName: string
    MailboxPassword: string
    Sender: Mail.User
    BccRecipient: Mail.User option
    Subject: string
    ContentTemplate: string
}

type AppConfig = {
    DbConnectionString: Db.ConnectionString
    ReservationStartTime: DateTimeOffset
    ReservationsPerSlot: int
    Date: DateTimeOffset
    InfoText: string
    StartTime: TimeSpan
    SlotDuration: TimeSpan
    NumberOfSlots: int
    MailConfig: MailConfig
}
module AppConfig =
    let private optEnvVar =
        Environment.GetEnvironmentVariable >> Option.ofObj
    let private envVar name =
        optEnvVar name |> Option.defaultWith (fun () -> failwithf "Environment variable \"%s\" not set" name)
    let private envVarAsInt name =
        let v = envVar name
        match Int32.TryParse(v) with
        | (true, v) -> v
        | _ -> failwithf "Environment variable \"%s\" can't be parsed as integer" name
    let private envVarAsDateTime name (format: string) =
        let v = envVar name
        match DateTimeOffset.TryParseExact(v, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | (true, v) -> v
        | _ -> failwithf "Environment variable \"%s\" can't be parsed as date time (format must be \"%s\")" name format
    let private envVarAsTimeSpan name (format: string) =
        let v = envVar name
        match TimeSpan.TryParseExact(v, format, CultureInfo.InvariantCulture) with
        | (true, v) -> v
        | _ -> failwithf "Environment variable \"%s\" can't be parsed as time span (format must be \"%s\")" name format

    let private parseMarkdown = Markdown.ToHtml

    let fromEnvironment () =
        {
            DbConnectionString = envVar "DB_CONNECTION_STRING" |> Db.ConnectionString
            ReservationStartTime = envVarAsDateTime "RESERVATION_START_TIME" "dd.MM.yyyy HH:mm:ss"
            ReservationsPerSlot = envVarAsInt "SCHEDULE_RESERVATIONS_PER_SLOT"
            Date = envVarAsDateTime "SCHEDULE_DATE" "dd.MM.yyyy"
            InfoText = envVar "INFO_TEXT" |> parseMarkdown
            StartTime = envVarAsTimeSpan "SCHEDULE_START_TIME" "hh\\:mm"
            SlotDuration = envVarAsTimeSpan "SCHEDULE_SLOT_DURATION" "hh\\:mm"
            NumberOfSlots = envVarAsInt "SCHEDULE_NUMBER_OF_SLOTS"
            MailConfig = {
                SmtpAddress = envVar "MAILBOX_SMTP_ADDRESS"
                MailboxUserName = envVar "MAILBOX_USERNAME"
                MailboxPassword = envVar "MAILBOX_PASSWORD"
                Sender = {
                    Name = envVar "MAIL_SENDER_NAME"
                    MailAddress = envVar "MAIL_SENDER_MAIL_ADDRESS"
                }
                BccRecipient =
                    match optEnvVar "MAIL_BCC_RECIPIENT_NAME", optEnvVar "MAIL_BCC_RECIPIENT_MAIL_ADDRESS" with
                    | Some name, Some mailAddress -> Some { Name = name; MailAddress = mailAddress }
                    | _ -> None
                Subject = envVar "MAIL_SUBJECT"
                ContentTemplate = envVar "MAIL_CONTENT_TEMPLATE"
            }
        }

let getSlotStartTime (startTime: TimeSpan) (slotDuration: TimeSpan) slotNumber =
    startTime + slotDuration * (float <| slotNumber - 1)

let handleGetSchedule appConfig : HttpHandler =
    fun next ctx -> task {
        let! schedule = Db.getSchedule (appConfig.DbConnectionString)
        let scheduleEntries =
            [ 1..appConfig.NumberOfSlots ]
            |> List.map (fun slotNumber ->
                let startTime = getSlotStartTime appConfig.StartTime appConfig.SlotDuration slotNumber
                let quantityTaken =
                    schedule
                    |> List.filter (fun entry -> entry.Time = startTime)
                    |> List.sumBy (fun entry -> entry.Quantity)
                let reservationsLeft = appConfig.ReservationsPerSlot - quantityTaken
                if reservationsLeft <= 0 then
                    {
                        StartTime = startTime
                        ReservationType = Taken
                    }
                else
                    {
                        StartTime = getSlotStartTime appConfig.StartTime appConfig.SlotDuration slotNumber
                        ReservationType = ReservationType.free reservationsLeft slotNumber
                    }
            )
        let schedule = {
            Date = appConfig.Date
            ReservationStartTime = appConfig.ReservationStartTime
            InfoText = appConfig.InfoText
            Entries = scheduleEntries
        }
        return! Successful.OK schedule next ctx
    }

type Subscriber = {
    Quantity: int
    Name: string
    MailAddress: string
}
module Subscriber =
    let validate (subscriber: DataTransfer.Subscriber) =
        let quantityIsValid = subscriber.Quantity > 0
        let subscriberNameIsValid = (not <| String.IsNullOrWhiteSpace subscriber.Name) && subscriber.Name.Length <= 100
        let mailAddressIsValid =
            let canParse =
                try
                    MailAddress subscriber.MailAddress |> ignore
                    true
                with _ -> false
            canParse && subscriber.MailAddress.Length <= 100
        if quantityIsValid && subscriberNameIsValid && mailAddressIsValid
        then Ok { Quantity = subscriber.Quantity; Name = subscriber.Name; MailAddress = subscriber.MailAddress }
        else Error ()

let handlePostSchedule appConfig slotNumber : HttpHandler =
    fun next ctx -> task {
        let! subscriber = ctx.BindJsonAsync<DataTransfer.Subscriber>()
        match Subscriber.validate subscriber, slotNumber with
        | Ok subscriber, slotNumber when slotNumber > 0 && slotNumber <= appConfig.NumberOfSlots -> 
            let slotStartTime = getSlotStartTime appConfig.StartTime appConfig.SlotDuration slotNumber
            if appConfig.ReservationStartTime >= DateTimeOffset.Now || appConfig.Date.Add(slotStartTime) < DateTimeOffset.Now then
                return! RequestErrors.BAD_REQUEST () next ctx
            else
                let! reservationsLeft = Db.book appConfig.DbConnectionString appConfig.ReservationsPerSlot {
                    Db.Schedule.Time = slotStartTime
                    Db.Schedule.Quantity = subscriber.Quantity
                    Db.Schedule.Name = subscriber.Name
                    Db.Schedule.MailAddress = subscriber.MailAddress
                    Db.Schedule.TimeStamp = DateTime.Now
                }
                let newReservationType =
                    if reservationsLeft <= 0 then Taken
                    else ReservationType.free reservationsLeft slotNumber
                let settings = {
                    Mail.Settings.SmtpAddress = appConfig.MailConfig.SmtpAddress
                    Mail.Settings.MailboxUserName =  appConfig.MailConfig.MailboxUserName
                    Mail.Settings.MailboxPassword =  appConfig.MailConfig.MailboxPassword
                    Mail.Settings.Sender =  appConfig.MailConfig.Sender
                    Mail.Settings.Recipient = {
                        Mail.User.Name = subscriber.Name
                        Mail.User.MailAddress = subscriber.MailAddress
                    }
                    Mail.Settings.BccRecipient =  appConfig.MailConfig.BccRecipient
                    Mail.Settings.Subject =  appConfig.MailConfig.Subject
                    Mail.Settings.Content =
                        let startTime = getSlotStartTime appConfig.StartTime appConfig.SlotDuration slotNumber
                        let templateVars = [
                            "FullName", subscriber.Name
                            "Date", appConfig.Date.ToString("dd.MM.yyyy")
                            "Time", startTime.ToString("hh\\:mm")
                        ]
                        (appConfig.MailConfig.ContentTemplate, templateVars)
                        ||> List.fold (fun text (varName, value) -> text.Replace(sprintf "{{{%s}}}" varName, value))
                }
                do! Mail.sendBookingConfirmation settings
                return! Successful.OK newReservationType next ctx
        | _ -> return! RequestErrors.BAD_REQUEST () next ctx
    }

let appConfig = AppConfig.fromEnvironment ()

let webApp =
    choose [
        subRoute "/api"
            (choose [
                GET >=> choose [
                    route "/schedule" >=> handleGetSchedule appConfig
                ]
                POST >=> choose [
                    routef "/schedule/%i" (fun slotNumber -> handlePostSchedule appConfig slotNumber)
                ]
            ])
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    match env.IsDevelopment() with
    | true -> app.UseDeveloperExceptionPage() |> ignore
    | false -> app.UseGiraffeErrorHandler errorHandler |> ignore
    app
        .UseHttpsRedirection()
        .UseDefaultFiles()
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddHttpClient() |> ignore
    services.AddGiraffe() |> ignore
    let coders =
        Extra.empty
    services.AddSingleton<IJsonSerializer>(ThothSerializer(caseStrategy = CamelCase, extra = coders)) |> ignore

let configureLogging (ctx: HostBuilderContext) (builder : ILoggingBuilder) =
    builder
        .AddFilter(fun l -> ctx.HostingEnvironment.IsDevelopment() || l.Equals LogLevel.Error)
        .AddConsole()
        .AddDebug()
    |> ignore

[<EntryPoint>]
let main args =
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(fun webHostBuilder -> webHostBuilder.Configure configureApp |> ignore)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0