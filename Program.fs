﻿namespace MonoGame002

module Program =

    open System
    open Microsoft.Xna.Framework

    [<EntryPoint>]
    let main argv =
        use game = new Game3()
        game.Run()
        0 // return an integer exit code
