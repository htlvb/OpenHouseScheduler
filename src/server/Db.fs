module Db

open Dapper
open MySql.Data.MySqlClient
open System

type ConnectionString = ConnectionString of string

type Schedule = {
    Time: TimeSpan
    Quantity: int
    Name: string
    MailAddress: string
    TimeStamp: DateTime
}

let private createConnection (ConnectionString connectionString) =
    new MySqlConnection(connectionString)

let getSchedule dbConfig = async {
    use connection = createConnection dbConfig
    let! result = connection.QueryAsync<Schedule>("SELECT Time, Quantity, Name, MailAddress, TimeStamp FROM schedule") |> Async.AwaitTask
    return Seq.toList result
}

let book dbConfig maxQuantity (data: Schedule) = async {
    use connection = createConnection dbConfig
    do! connection.ExecuteAsync("LOCK TABLE schedule WRITE") |> Async.AwaitTask |> Async.Ignore
    let! sumQuantity = async {
        let! v = connection.ExecuteScalarAsync("SELECT COALESCE(SUM(Quantity), 0) FROM schedule WHERE Time = @Time", {| Time = data.Time |}) |> Async.AwaitTask
        return v :?> decimal |> int
    }
    let reservationsLeft = maxQuantity - (sumQuantity + data.Quantity)
    if reservationsLeft < 0 then
        failwithf "Can't save booking because max quantity would be exceeded (%d + %d > %d)" sumQuantity data.Quantity maxQuantity
    do! connection.ExecuteAsync("INSERT INTO schedule (Time, Quantity, Name, MailAddress, TimeStamp) VALUES (@Time, @Quantity, @Name, @MailAddress, @TimeStamp)", data) |> Async.AwaitTask |> Async.Ignore
    do! connection.ExecuteAsync("UNLOCK TABLES") |> Async.AwaitTask |> Async.Ignore
    return reservationsLeft
}
