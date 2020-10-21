module Main

open Browser.Dom
open Fable.Core.JsInterop

importAll "./styles/main.scss"

open Feliz

let schedule = React.functionComponent(fun () ->
    let (isLoading, setLoading) = React.useState(false)
    let (content, setContent) = React.useState("")

    let loadData() = async {
        setLoading true
        do! Async.Sleep 1500
        setLoading false
        setContent "Content"
    }

    React.useEffectOnce(loadData >> Async.StartImmediate)

    Html.div [
        if isLoading
        then Html.h1 "Loading"
        else Html.h1 content
    ]
)


ReactDOM.render(schedule, document.getElementById "feliz-app")
