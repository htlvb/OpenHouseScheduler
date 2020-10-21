module App.Program

open DataTransfer
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Giraffe.Serialization
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open System
open System.Globalization
open System.Net.Mail
open System.Text.RegularExpressions
open Thoth.Json.Giraffe
open Thoth.Json.Net

// ---------------------------------
// Web app
// ---------------------------------

type AppConfig = {
    DbConnectionString: Db.ConnectionString
    Date: DateTimeOffset
    StartTime: TimeSpan
    SlotDuration: TimeSpan
    NumberOfSlots: int
}
module AppConfig =
    let private envVar name =
        match Environment.GetEnvironmentVariable name with
        | null -> failwithf "Environment variable \"%s\" not set" name
        | v -> v
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

    let fromEnvironment () =
        {
            DbConnectionString = envVar "DB_CONNECTION_STRING" |> Db.ConnectionString
            Date = envVarAsDateTime "SCHEDULE_DATE" "dd.MM.yyyy"
            StartTime = envVarAsTimeSpan "SCHEDULE_START_TIME" "hh\\:mm"
            SlotDuration = envVarAsTimeSpan "SCHEDULE_SLOT_DURATION" "hh\\:mm"
            NumberOfSlots = envVarAsInt "SCHEDULE_NUMBER_OF_SLOTS"
        }

module Schedule =
    let fromDb (startTime: TimeSpan) (slotDuration: TimeSpan) (schedule: Db.Schedule) =
        {
            StartTime = startTime + slotDuration * (float <| schedule.SlotNumber - 1)
            ReservationType = Taken
        }

let handleGetSchedule appConfig : HttpHandler =
    fun next ctx -> task {
        let! schedule = Db.getSchedule (appConfig.DbConnectionString)
        let scheduleEntries =
            [ 1..appConfig.NumberOfSlots ]
            |> List.map (fun slotNumber ->
                schedule
                |> List.tryFind(fun entry -> entry.SlotNumber = slotNumber)
                |> Option.map (Schedule.fromDb appConfig.StartTime appConfig.SlotDuration)
                |> Option.defaultValue {
                    StartTime = appConfig.StartTime + appConfig.SlotDuration * (float <| slotNumber - 1)
                    ReservationType = Free (sprintf "/api/schedule/%d" slotNumber)
                }
            )
        let schedule = {
            Date = appConfig.Date
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
        match Subscriber.validate subscriber with
        | Ok v -> 
            do! Db.book appConfig.DbConnectionString {
                Db.Schedule.SlotNumber = slotNumber
                Db.Schedule.Name = subscriber.Name
                Db.Schedule.MailAddress = subscriber.MailAddress
            }
            return! Successful.OK () next ctx
        | Error () -> return! RequestErrors.BAD_REQUEST () next ctx
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