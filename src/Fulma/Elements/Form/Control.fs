namespace Fulma.Elements.Form

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

[<RequireQualifiedAccess>]
module Control =

    module Classes =
        let [<Literal>] Container = "control"
        module HasIcon =
            let [<Literal>] Left = "has-icons-left"
            let [<Literal>] Right = "has-icons-right"
        module State =
            let [<Literal>] IsLoading = "is-loading"

    type [<Fable.Core.CompileAsArray>] Option =
        | HasIconRight
        | HasIconLeft
        | IsLoading
        | CustomClass of string
        | Props of IHTMLProp list

    type [<Fable.Core.CompileAsArray>] internal Options =
        { HasIconLeft : bool
          HasIconRight : bool
          CustomClass : string option
          Props : IHTMLProp list
          IsLoading : bool }
    let inline internal defaultOptions() =
            { HasIconLeft = false
              HasIconRight = false
              CustomClass = None
              Props = []
              IsLoading = false }

    let control options children =
        let parseOptions (result : Options) =
            function
            | HasIconRight -> { result with HasIconRight = true }
            | HasIconLeft -> { result with HasIconLeft = true }
            | CustomClass customClass -> { result with CustomClass = customClass |> Some }
            | Props props -> { result with Props = props }
            | IsLoading -> { result with IsLoading = true }

        let opts = options |> List.fold parseOptions (defaultOptions())

        let classes = Helpers.classes
                        Classes.Container
                        [ opts.CustomClass ]
                        [ Classes.State.IsLoading, opts.IsLoading
                          Classes.HasIcon.Right, opts.HasIconRight
                          Classes.HasIcon.Left, opts.HasIconLeft ]

        div (classes::opts.Props)
            children
