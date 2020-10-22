module Mail

open MailKit.Net.Smtp
open MailKit.Security
open MimeKit
open MimeKit.Text

type MailUser = {
    Name: string
    MailAddress: string
}

type Settings = {
    Sender: MailUser
    BccRecipient: MailUser
    MailboxUserName: string
    MailboxPassword: string
    SmtpAddress: string
}

let sendBookingConfirmation mailSettings recipient subject content = async {
    let message = MimeMessage()
    message.From.Add(MailboxAddress(mailSettings.Sender.Name, mailSettings.Sender.MailAddress))
    message.To.Add(MailboxAddress(recipient.Name, recipient.MailAddress))
    message.Bcc.Add(MailboxAddress(mailSettings.BccRecipient.Name, mailSettings.BccRecipient.MailAddress))
    message.Subject <- subject
    message.Body <- TextPart(TextFormat.Plain, Text = content)

    use smtp = new SmtpClient()
    let! ct = Async.CancellationToken
    do! smtp.ConnectAsync(mailSettings.SmtpAddress, 587, SecureSocketOptions.StartTls, ct) |> Async.AwaitTask
    do! smtp.AuthenticateAsync(mailSettings.MailboxUserName, mailSettings.MailboxPassword, ct) |> Async.AwaitTask
    do! smtp.SendAsync(message, ct) |> Async.AwaitTask
    do! smtp.DisconnectAsync(true, ct) |> Async.AwaitTask
}
