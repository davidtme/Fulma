namespace Fulma.Components

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

[<RequireQualifiedAccess>]
module Message =

    module Classes =
        let [<Literal>] Container = "message"
        let [<Literal>] Header = "message-header"
        let [<Literal>] Body = "message-body"

    type [<Fable.Core.CompileAsArray>] Option =
        | Props of IHTMLProp list
        | Color of IColor
        | Size of ISize
        | CustomClass of string

    type internal Options =
        { Props : IHTMLProp list
          Color : string option
          Size : string option
          CustomClass : string option }

        static member Empty =
            { Props = []
              Color = None
              Size = None
              CustomClass = None }

    let message options children =
        let parseOptions (result: Options ) opt =
            match opt with
            | Props props -> { result with Props = props }
            | Option.Color color -> { result with Color = ofColor color |> Some}
            | CustomClass customClass -> { result with CustomClass = Some customClass }
            | Size size -> { result with Size = ofSize size |> Some }

        let opts = options |> List.fold parseOptions Options.Empty
        let classes = Helpers.classes
                        Classes.Container
                        [ opts.Color; opts.CustomClass; opts.Size ]
                        [ ]

        article (classes::opts.Props)
            children

    let header (options: GenericOption list) children =
        let opts = genericParse options
        let classes = Helpers.classes Classes.Header [opts.CustomClass] []
        div (classes::opts.Props) children

    let body (options: GenericOption list) children =
        let opts = genericParse options
        let classes = Helpers.classes Classes.Body [opts.CustomClass] []
        div (classes::opts.Props) children
