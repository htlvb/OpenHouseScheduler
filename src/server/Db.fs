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

let setup dbConfig = async {
    use connection = createConnection dbConfig
    connection.Execute(
        """CREATE TABLE schedule
        (
            SlotNumber INT NOT NULL,
            Name VARCHAR(255) NOT NULL,
            MailAddress VARCHAR(255) NOT NULL
        )"""
    ) |> ignore
}

let getSchedule dbConfig = async {
    use connection = createConnection dbConfig
    let! result = connection.QueryAsync<Schedule>("SELECT * FROM schedule") |> Async.AwaitTask
    return Seq.toList result
}