namespace Fulma.Elements

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

[<RequireQualifiedAccess>]
module Image =

    module Classes =
        let [<Literal>] Container = "image"
        module Size =
              let [<Literal>] Is16x16 = "is-16x16"
              let [<Literal>] Is24x24 = "is-24x24"
              let [<Literal>] Is32x32 = "is-32x32"
              let [<Literal>] Is48x48 = "is-48x48"
              let [<Literal>] Is64x64 = "is-64x64"
              let [<Literal>] Is96x96 = "is-96x96"
              let [<Literal>] Is128x128 = "is-128x128"
        module Ratio =
              let [<Literal>] IsSquare = "is-square"
              let [<Literal>] Is1by1 = "is-1by1"
              let [<Literal>] Is4by3 = "is-4by3"
              let [<Literal>] Is3by2 = "is-3by2"
              let [<Literal>] Is16by9 = "is-16by9"
              let [<Literal>] Is2by1 = "is-2by1"

    type [<Fable.Core.CompileAsArray>] Option =
        // Size
        | Is16x16
        | Is24x24
        | Is32x32
        | Is48x48
        | Is64x64
        | Is96x96
        | Is128x128
        // Ratio
        | IsSquare
        | Is1by1
        | Is4by3
        | Is3by2
        | Is16by9
        | Is2by1
        // Extra
        | CustomClass of string
        | Props of IHTMLProp list

    type internal Options =
        { Size : string option
          Ratio : string option
          CustomClass : string option
          Props : IHTMLProp list }
        static member Empty =
            { Size = None
              Ratio = None
              CustomClass = None
              Props = [] }

    let image options children =
        let parseOptions (result : Options) =
            function
            // Size
            | Is16x16 -> { result with Size = Classes.Size.Is16x16 |> Some }
            | Is24x24 -> { result with Size = Classes.Size.Is24x24 |> Some }
            | Is32x32 -> { result with Size = Classes.Size.Is32x32 |> Some }
            | Is48x48 -> { result with Size = Classes.Size.Is48x48 |> Some }
            | Is64x64 -> { result with Size = Classes.Size.Is64x64 |> Some }
            | Is96x96 -> { result with Size = Classes.Size.Is96x96 |> Some }
            | Is128x128 -> { result with Size = Classes.Size.Is128x128 |> Some }
            // Ratio
            | IsSquare -> { result with Ratio = Classes.Ratio.IsSquare |> Some }
            | Is1by1 -> { result with Ratio = Classes.Ratio.Is1by1 |> Some }
            | Is4by3 -> { result with Ratio = Classes.Ratio.Is4by3 |> Some }
            | Is3by2 -> { result with Ratio = Classes.Ratio.Is3by2 |> Some }
            | Is16by9 -> { result with Ratio = Classes.Ratio.Is16by9 |> Some }
            | Is2by1 -> { result with Ratio = Classes.Ratio.Is2by1 |> Some }
            // Extra
            | CustomClass customClass -> { result with CustomClass = customClass |> Some }
            | Props props -> { result with Props = props }

        let opts = options |> List.fold parseOptions Options.Empty
        let classes = Helpers.classes
                        Classes.Container
                        [ opts.Size; opts.Ratio; opts.CustomClass ]
                        [ ]
        figure (classes::opts.Props)
            children
