namespace Fulma.Layouts

open Fulma
open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import

[<RequireQualifiedAccess>]
module Level =

    module Classes =
        let [<Literal>] Container = "level"
        let [<Literal>] Left = "level-left"
        let [<Literal>] Right = "level-right"
        module Item =
            let [<Literal>] Container = "level-item"
            let [<Literal>] HasTextCentered = "has-text-centered"
            let [<Literal>] Heading = "heading"
            let [<Literal>] Title = "title"
        module Mobile =
            let [<Literal>] IsHorizontal = "is-mobile"

    module Level =

        type [<Fable.Core.CompileAsArray>] Option =
            | Props of IHTMLProp list
            | IsMobile
            | CustomClass of string

        type [<Fable.Core.CompileAsArray>] internal Options =
            { Props : IHTMLProp list
              IsMobile : bool
              CustomClass : string option }

        let inline internal defaultOptions() =
                { Props = []
                  IsMobile = false
                  CustomClass = None }

    module Item =

        type [<Fable.Core.CompileAsArray>] Option =
            | Props of IHTMLProp list
            | HasTextCentered
            | CustomClass of string

        type [<Fable.Core.CompileAsArray>] internal Options =
            { Props : IHTMLProp list
              HasTextCentered : bool
              CustomClass : string option }

        let inline internal defaultOptions() =
                { Props = []
                  HasTextCentered = false
                  CustomClass = None }

    let level (options : Level.Option list) children =
        let parseOptions (result: Level.Options ) opt =
            match opt with
            | Level.Option.Props props -> { result with Props = props }
            | Level.Option.IsMobile -> { result with IsMobile = true }
            | Level.CustomClass customClass -> { result with CustomClass = Some customClass }

        let opts = options |> List.fold parseOptions (Level.defaultOptions())
        let classes = Helpers.classes Classes.Container [opts.CustomClass]
                        [ Classes.Mobile.IsHorizontal, opts.IsMobile ]
        nav (classes::opts.Props) children

    let left (options: GenericOption list) children =
        let opts = genericParse options
        let classes = Helpers.classes Classes.Left [opts.CustomClass] []
        div (classes::opts.Props) children

    let right (options: GenericOption list) children =
        let opts = genericParse options
        let classes = Helpers.classes Classes.Right [opts.CustomClass] []
        div (classes::opts.Props) children

    let item (options : Item.Option list) children =
        let parseOptions (result: Item.Options ) opt =
            match opt with
            | Item.Props props -> { result with Props = props }
            | Item.HasTextCentered -> { result with HasTextCentered = true }
            | Item.CustomClass customClass -> { result with CustomClass = Some customClass }

        let opts = options |> List.fold parseOptions (Item.defaultOptions())
        let classes = Helpers.classes Classes.Item.Container [opts.CustomClass]
                        [ Classes.Item.HasTextCentered, opts.HasTextCentered ]
        div (classes::opts.Props) children

    let heading (options: GenericOption list) children =
        let opts = genericParse options
        let classes = Helpers.classes Classes.Item.Heading [opts.CustomClass] []
        p (classes::opts.Props) children

    let title (options: GenericOption list) children =
        let opts = genericParse options
        let classes = Helpers.classes Classes.Item.Title [opts.CustomClass] []
        p (classes::opts.Props) children
