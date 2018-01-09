namespace Fulma.Elements

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

[<RequireQualifiedAccess>]
module Table =

    module Classes =
        let [<Literal>] Container = "table"
        module Row =
            module State =
                let [<Literal>] IsSelected = "is-selected"
        module Style =
          let [<Literal>] IsBordered = "is-bordered"
          let [<Literal>] IsStripped = "is-stripped "
          let [<Literal>] IsFullwidth = "is-fullwidth"
        module Spacing =
            let [<Literal>] IsNarrow = "is-narrow"

    type [<Fable.Core.CompileAsArray>] TableOption =
        | IsBordered
        | IsStripped
        | IsFullwidth
        | IsNarrow
        | CustomClass of string
        | Props of IHTMLProp list

    type [<Fable.Core.CompileAsArray>] private TableOptions =
        { IsBordered : bool
          IsStripped : bool
          IsFullwidth : bool
          IsNarrow : bool
          CustomClass : string option
          Props : IHTMLProp list }

    let inline private defaultTableOptions() =
            { IsBordered = false
              IsStripped = false
              IsNarrow = false
              IsFullwidth = false
              CustomClass = None
              Props = [] }

    let table options children =
        let parseOptions (result : TableOptions) =
            function
            | IsBordered -> { result with IsBordered = true }
            | IsStripped -> { result with IsStripped = true }
            | IsFullwidth -> { result with IsFullwidth = true }
            | IsNarrow -> { result with IsNarrow = true }
            | CustomClass customClass -> { result with CustomClass = customClass |> Some }
            | Props props -> { result with Props = props }

        let opts = options |> List.fold parseOptions (defaultTableOptions())
        let classes = Helpers.classes Classes.Container [opts.CustomClass]
                        [ Classes.Style.IsBordered, opts.IsBordered
                          Classes.Style.IsStripped, opts.IsStripped
                          Classes.Style.IsFullwidth, opts.IsFullwidth
                          Classes.Spacing.IsNarrow, opts.IsNarrow ]

        table (classes::opts.Props) children
