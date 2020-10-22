module Db

open Dapper
open MySql.Data.MySqlClient
open System

type ConnectionString = ConnectionString of string

type Schedule = {
    Time: TimeSpan
    Name: string
    MailAddress: string
    TimeStamp: DateTime
}

let private createConnection (ConnectionString connectionString) =
    new MySqlConnection(connectionString)

let getSchedule dbConfig = async {
    use connection = createConnection dbConfig
    let! result = connection.QueryAsync<Schedule>("SELECT * FROM schedule") |> Async.AwaitTask
    return Seq.toList result
}

let book dbConfig (data: Schedule) = async {
    use connection = createConnection dbConfig
    do! connection.ExecuteAsync("INSERT INTO schedule (Time, Name, MailAddress, TimeStamp) VALUES (@Time, @Name, @MailAddress, @TimeStamp)", data) |> Async.AwaitTask |> Async.Ignore
}
