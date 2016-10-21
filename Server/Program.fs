// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System.Net
open System.Net.Sockets
open System.Numerics
[<EntryPoint>]
let main = function
    |[|startx;starty;finishx;finishy;res;iters;alloc;port|] ->
        let port     = int port
        let res      = float res
        let alloc    = float alloc
        let iters    = int iters
        let listener = TcpListener(IPEndPoint(IPAddress.Any, port))
        let startx   = float startx
        let starty   = float starty
        let finishx  = float finishx
        let finishy  = float finishy

        printfn "Bound to port %i" port
        let start  = Complex(startx, starty)
        let finish = Complex(finishx, finishy)
        let width  = finish.Real - start.Real
        let height = finish.Imaginary - start.Imaginary
        let x = int(width/alloc)
        let y = int(height/alloc)
        let step = int(alloc/res)

        //You better have a lot of swap m8
        let bm = new System.Drawing.Bitmap(x*step,y*step)

        let drawone i (x,y) = 
            let r,g,b =
                let H = (iters - i) * 270 / iters
                //http://www.rapidtables.com/convert/color/hsv-to-rgb.htm
                let X = 1. - abs(((float H/60.) % 2.) - 1.)
                if   H < 60  then 1.,X ,0.
                elif H < 120 then X ,1.,0.
                elif H < 180 then 0.,1.,X
                elif H < 240 then 0.,X ,1.
                elif H < 300 then X ,0.,1.
                else              1.,0.,X
            let c = System.Drawing.Color.FromArgb(r*255. |> int, g*255. |> int, b*255. |> int)
            lock bm (fun () -> bm.SetPixel(x,y,c))
        
        let xarr = [|0..x-1|]
        let yarr = [|0..y-1|] //a pirate I be
        Array.Parallel.iter (fun i ->
            let startx = float i * alloc
            let finishx = (float i * alloc) + (float step) - 1.
            Array.Parallel.iter (fun j ->
                let starty = float j * alloc
                let finishy = (float j * alloc) + (float step) - 1.
                let s = listener.AcceptTcpClient()
                startx  |> System.BitConverter.GetBytes |> s.Client.Send |> ignore
                starty  |> System.BitConverter.GetBytes |> s.Client.Send |> ignore
                finishx |> System.BitConverter.GetBytes |> s.Client.Send |> ignore
                finishy |> System.BitConverter.GetBytes |> s.Client.Send |> ignore
                res     |> System.BitConverter.GetBytes |> s.Client.Send |> ignore
                iters   |> System.BitConverter.GetBytes |> s.Client.Send |> ignore
                let width1 = (abs startx + abs finishx)/res |> int
                while s.Available = 0 do () done
                let b = Array.zeroCreate<byte> s.Available
                s.Client.Receive(b) |> ignore
                b
                |> Array.chunkBySize 4
                |> Array.Parallel.map (fun i -> System.BitConverter.ToInt32(i,0))
                |> Array.chunkBySize width1
                |> Array.iteri (fun i -> 
                    Array.iteri(fun j v -> 
                        drawone v (i,j))
                    )
            ) yarr
        ) xarr
        0
    |_ -> printfn "Server <resolution> <iterations> <allocation size> <port>"; exit -1

