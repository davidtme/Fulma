namespace Fulma.Elements

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

[<RequireQualifiedAccess>]
module Icon =

    module Classes =
        let [<Literal>] Container = "icon"
        module Position =
              let [<Literal>] Left = "is-left"
              let [<Literal>] Right = "is-right"

    type [<Fable.Core.CompileAsArray>] Option =
        // Sizes
        | Size of ISize
        // Position
        | IsLeft
        | IsRight
        // Extra
        | CustomClass of string
        | Props of IHTMLProp list

    type [<Fable.Core.CompileAsArray>] internal Options =
        { Size : string option
          Position : string option
          CustomClass : string option
          Props : IHTMLProp list }
    let inline internal defaultOptions() =
            { Size = None
              Position = None
              CustomClass = None
              Props = [] }

    let icon options children =
        let parseOptions (result : Options) option =
            match option with
            // Sizes
            | Size size -> { result with Size = ofSize size |> Some }
            // Position
            | IsLeft -> { result with Position = Classes.Position.Left |> Some }
            | IsRight -> { result with Position = Classes.Position.Right |> Some }
            // Extra
            | CustomClass customClass -> { result with CustomClass = customClass |> Some }
            | Props props -> { result with Props = props }

        let opts = options |> List.fold parseOptions (defaultOptions())
        let classes = Helpers.classes
                        Classes.Container
                        [ opts.Size; opts.Position; opts.CustomClass ]
                        [ ]
        span (classes::opts.Props)
            children
