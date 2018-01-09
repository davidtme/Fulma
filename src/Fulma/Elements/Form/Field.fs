namespace Fulma.Elements.Form

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

[<RequireQualifiedAccess>]
module Field =

    module Classes =
        let [<Literal>] Container = "field"
        let [<Literal>] Label = "field-label"
        let [<Literal>] Body = "field-body"
        module HasAddons =
              let [<Literal>] Left = "has-addons"
              let [<Literal>] Centered = "has-addons-centered"
              let [<Literal>] Right = "has-addons-right"
              let [<Literal>] FullWidh = "has-addons-fullwidth"
        module IsGrouped =
              let [<Literal>] Left = "is-grouped"
              let [<Literal>] Centered = "is-grouped-centered"
              let [<Literal>] Right = "is-grouped-right"
              let [<Literal>] Multiline = "is-grouped-multiline"
        module Layout =
            let [<Literal>] IsHorizontal = "is-horizontal"

    type [<Fable.Core.CompileAsArray>] Option =
        | HasAddons
        | HasAddonsCentered
        | HasAddonsRight
        | HasAddonsFullWidth
        | IsGrouped
        | IsGroupedCentered
        | IsGroupedRight
        | IsHorizontal
        | CustomClass of string
        | Props of IHTMLProp list

    type internal Options =
        { HasAddons : string option
          IsGrouped : string option
          Layout : string option
          CustomClass : string option
          Props : IHTMLProp list }
        static member Empty =
            { HasAddons = None
              IsGrouped = None
              Layout = None
              CustomClass = None
              Props = [] }

    type [<Fable.Core.CompileAsArray>] FieldLabelOption =
        | Size of ISize
        | CustomClass of string
        | Props of IHTMLProp list

    type internal FieldLabelOptions =
        { Size : string option
          CustomClass : string option
          Props : IHTMLProp list }
        static member Empty =
            { Size = None
              CustomClass = None
              Props = [] }

    let field options children =
        let parseOptions (result : Options) =
            function
            | HasAddons -> { result with HasAddons = Classes.HasAddons.Left |> Some }
            | HasAddonsCentered -> { result with HasAddons = Classes.HasAddons.Left + " " + Classes.HasAddons.Centered |> Some }
            | HasAddonsRight -> { result with HasAddons = Classes.HasAddons.Left + " " + Classes.HasAddons.Right |> Some }
            | HasAddonsFullWidth -> { result with HasAddons = Classes.HasAddons.Left + " " + Classes.HasAddons.FullWidh |> Some }
            | IsGrouped -> { result with IsGrouped = Classes.IsGrouped.Left |> Some }
            | IsGroupedCentered -> { result with IsGrouped = Classes.IsGrouped.Left + " " + Classes.IsGrouped.Centered |> Some }
            | IsGroupedRight -> { result with IsGrouped = Classes.IsGrouped.Left + " " + Classes.IsGrouped.Right |> Some }
            | IsHorizontal -> { result with Layout = Classes.Layout.IsHorizontal |> Some }
            | Option.CustomClass customClass -> { result with CustomClass = customClass |> Some }
            | Option.Props props -> { result with Props = props }

        let opts = options |> List.fold parseOptions Options.Empty
        let classes = Helpers.classes
                        Classes.Container
                        [ opts.HasAddons; opts.IsGrouped; opts.Layout; opts.CustomClass ]
                        [ ]

        div (classes::opts.Props)
            children

    let label options children =
        let parseOptions (result : FieldLabelOptions) =
            function
            | Size size -> { result with Size = ofSize size |> Some }
            | CustomClass customClass -> { result with CustomClass = customClass |> Some }
            | Props props -> { result with Props = props }

        let opts = options |> List.fold parseOptions FieldLabelOptions.Empty
        let classes = Helpers.classes
                        Classes.Label
                        [ opts.Size; opts.CustomClass ]
                        [ ]
        div (classes::opts.Props)
            children

    let body (options : GenericOption list) children =
        let opts = genericParse options
        let classes = Helpers.classes Classes.Body [opts.CustomClass] []
        div (classes::opts.Props) children
