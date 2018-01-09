namespace Fulma.Layouts

open Fulma
open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import

[<RequireQualifiedAccess>]
module Section =

    module Classes =
        let [<Literal>] Container = "section"
        module Spacing =
            let [<Literal>] IsMedium = "is-medium"
            let [<Literal>] IsLarge = "is-large"

    type [<Fable.Core.CompileAsArray>] Option =
        | Props of IHTMLProp list
        | CustomClass of string
        | IsMedium
        | IsLarge

    type [<Fable.Core.CompileAsArray>] internal Options =
        { Props : IHTMLProp list
          CustomClass : string option
          Spacing : string option }

    let inline internal defaultOptions() =
            { Props = []
              CustomClass = None
              Spacing = None }

    let section (options: Option list) children =
        let parseOptions (result: Options ) opt =
            match opt with
            | Props props -> { result with Props = props }
            | IsMedium -> { result with Spacing = Classes.Spacing.IsMedium |> Some }
            | IsLarge -> { result with Spacing = Classes.Spacing.IsLarge |> Some }
            | CustomClass customClass -> { result with CustomClass = Some customClass }

        let opts = options |> List.fold parseOptions (defaultOptions())
        let classes = Helpers.classes Classes.Container [opts.CustomClass; opts.Spacing] []

        section (classes::opts.Props) children
