module Db

open Dapper
open MySql.Data.MySqlClient

type ConnectionString = ConnectionString of string

type Schedule = {
    SlotNumber: int
    Name: string
    MailAddress: string
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
    do! connection.ExecuteAsync("INSERT INTO schedule (SlotNumber, Name, MailAddress) VALUES (@SlotNumber, @Name, @MailAddress)", data) |> Async.AwaitTask |> Async.Ignore
}
