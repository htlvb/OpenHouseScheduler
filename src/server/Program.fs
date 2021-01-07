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

module Schedule =
    let fromDb (schedule: Db.Schedule) =
        {
            StartTime = schedule.Time
            ReservationType = Taken
        }

let handleGetSchedule appConfig : HttpHandler =
    fun next ctx -> task {
        let! schedule = Db.getSchedule (appConfig.DbConnectionString)
        let scheduleEntries =
            [ 1..appConfig.NumberOfSlots ]
            |> List.map (fun slotNumber ->
                let startTime = getSlotStartTime appConfig.StartTime appConfig.SlotDuration slotNumber
                schedule
                |> List.tryFind(fun entry -> entry.Time = startTime)
                |> Option.map Schedule.fromDb
                |> Option.defaultValue {
                    StartTime = getSlotStartTime appConfig.StartTime appConfig.SlotDuration slotNumber
                    ReservationType = Free (ReservationLink (sprintf "api/schedule/%d" slotNumber))
                }
            )
        let schedule = {
            Date = appConfig.Date
            InfoText = appConfig.InfoText
            Entries = scheduleEntries
        }
        return! Successful.OK schedule next ctx
    }

type Subscriber = {
    Name: string
    MailAddress: string
}
module Subscriber =
    let validate (subscriber: DataTransfer.Subscriber) =
        let mailAddressIsValid =
            try
                MailAddress subscriber.MailAddress |> ignore
                true
            with _ -> false
        if String.IsNullOrWhiteSpace subscriber.Name || subscriber.Name.Length > 100 || not mailAddressIsValid || subscriber.MailAddress.Length > 100
        then Error ()
        else Ok { Name = subscriber.Name; MailAddress = subscriber.MailAddress }

let handlePostSchedule appConfig slotNumber : HttpHandler =
    fun next ctx -> task {
        let! subscriber = ctx.BindJsonAsync<DataTransfer.Subscriber>()
        match Subscriber.validate subscriber, slotNumber with
        | Ok subscriber, slotNumber when slotNumber > 0 && slotNumber <= appConfig.NumberOfSlots -> 
            do! Db.book appConfig.DbConnectionString {
                Db.Schedule.Time = getSlotStartTime appConfig.StartTime appConfig.SlotDuration slotNumber
                Db.Schedule.Name = subscriber.Name
                Db.Schedule.MailAddress = subscriber.MailAddress
                Db.Schedule.TimeStamp = DateTime.Now
            }
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
            return! Successful.OK () next ctx
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