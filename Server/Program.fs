// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System.Net
open System.Net.Sockets
[<EntryPoint>]
let main = function
    |[|x;y|] ->
        let x,y = int x, int y
        //You better have a lot of swap m8
        let bm = new System.Drawing.Bitmap(x,y)
        let drawone (r:float,g:float,b:float) (x,y) = 
            let c = System.Drawing.Color.FromArgb(r*255. |> int, g*255. |> int, b*255. |> int)
            lock bm (fun () -> bm.SetPixel(x,y,c))
        0
    |_ -> printfn "Server <x> <y>"; exit -1

