module Mail

open MailKit.Net.Smtp
open MailKit.Security
open MimeKit
open MimeKit.Text

type User = {
    Name: string
    MailAddress: string
}

type Settings = {
    SmtpAddress: string
    MailboxUserName: string
    MailboxPassword: string
    Sender: User
    Recipient: User
    BccRecipient: User option
    Subject: string
    Content: string
}

let sendBookingConfirmation mailSettings = async {
    let message = MimeMessage()
    message.From.Add(MailboxAddress(mailSettings.Sender.Name, mailSettings.Sender.MailAddress))
    message.To.Add(MailboxAddress(mailSettings.Recipient.Name, mailSettings.Recipient.MailAddress))
    mailSettings.BccRecipient |> Option.iter (fun user -> message.Bcc.Add(MailboxAddress(user.Name, user.MailAddress)))
    message.Subject <- mailSettings.Subject
    message.Body <- TextPart(TextFormat.Plain, Text = mailSettings.Content)

    use smtp = new SmtpClient()
    let! ct = Async.CancellationToken
    do! smtp.ConnectAsync(mailSettings.SmtpAddress, 587, SecureSocketOptions.StartTls, ct) |> Async.AwaitTask
    do! smtp.AuthenticateAsync(mailSettings.MailboxUserName, mailSettings.MailboxPassword, ct) |> Async.AwaitTask
    do! smtp.SendAsync(message, ct) |> Async.AwaitTask
    do! smtp.DisconnectAsync(true, ct) |> Async.AwaitTask
}
