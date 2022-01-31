open Dapper
open MySql.Data.MySqlClient
open System

type Schedule = {
    Time: DateTime
    Quantity: int
    Name: string
    MailAddress: string
    PhoneNumber: string
    TimeStamp: DateTime
}

[<EntryPoint>]
let main args =
    let connectionString = Environment.GetEnvironmentVariable "DB_CONNECTION_STRING"
    use connection = new MySqlConnection(connectionString)
    let data =
        [
            for dayOffset in [0..4] do
                let date = DateTime(2022, 02, 04).AddDays(dayOffset)
                for timeOffset in [13*60..5..18*60-5] do
                    yield {
                        Time = date.AddMinutes(timeOffset)
                        Quantity = 8
                        Name = "Albert Einstein"
                        MailAddress = "a.einstein@posteo.at"
                        PhoneNumber = "+43 677 123456789"
                        TimeStamp = DateTime.Now
                    }
        ]
    connection.ExecuteAsync("INSERT INTO schedule (Time, Quantity, Name, MailAddress, PhoneNumber, TimeStamp) VALUES (@Time, @Quantity, @Name, @MailAddress, @PhoneNumber, @TimeStamp)", data) |> Async.AwaitTask |> Async.RunSynchronously
