namespace Fulma.Elements

open Fulma
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

[<RequireQualifiedAccess>]
module Delete =

    module Classes =
        let [<Literal>] Container = "delete"

    type [<Fable.Core.CompileAsArray>] Option =
        | Size of ISize
        | Props of IHTMLProp list
        | CustomClass of string
        | OnClick of (MouseEvent -> unit)

    type [<Fable.Core.CompileAsArray>] internal Options =
        { Size : string option
          Props : IHTMLProp list
          CustomClass : string option
          OnClick : (MouseEvent -> unit) option }
    let inline internal defaultOptions() =
            { Size = None
              Props = []
              CustomClass = None
              OnClick = None }

    let delete (options : Option list) children =
        let parseOption (result : Options) opt =
            match opt with
            // Sizes
            | Size size -> { result with Size = ofSize size |> Some }
            // Extra
            | Props props -> { result with Props = props }
            | CustomClass customClass -> { result with CustomClass = Some customClass }
            | OnClick cb -> { result with OnClick = cb |> Some }

        let opts = options |> List.fold parseOption (defaultOptions())
        let classes = Helpers.classes
                        Classes.Container
                        [ opts.Size; opts.CustomClass ]
                        [ ]
        a [ yield classes
            yield! opts.Props
            if Option.isSome opts.OnClick then
                yield DOMAttr.OnClick opts.OnClick.Value :> IHTMLProp ]
            children
