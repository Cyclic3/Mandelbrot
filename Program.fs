open Microsoft.FSharp.NativeInterop
open System.Numerics
open System.Net
open System.Net.Sockets
#nowarn "9"
[<EntryPoint>]
let main  = 
    function 
    |[|ipaddress;port|] ->
        let real i = Complex(i, 0.)
        let imaginary i = Complex(0., i)

        let client = new TcpClient()
        client.Connect(IPAddress.Parse ipaddress, int port)

        while client.Available = 0 do ()
        let b = Array.zeroCreate<byte> 44
        client.Client.Receive(b) |> ignore

        let startx = real <| System.BitConverter.ToDouble(b,0)
        let starty = imaginary <| System.BitConverter.ToDouble(b,8)
        let finishx = real <| System.BitConverter.ToDouble(b,16)
        let finishy = imaginary <| System.BitConverter.ToDouble(b,24)
        let res = real <| System.BitConverter.ToDouble(b,32)
        let iters = System.BitConverter.ToInt32(b,40)

        printfn "From %A to %A in steps of %A" (startx + starty) (finishx + finishy) res

        let width    = abs(startx.Magnitude) + abs(finishx.Magnitude)
        let height = abs(starty.Magnitude) + abs(finishy.Magnitude)

        let x = width  / res.Magnitude |> int
        let y = height / res.Magnitude |> int

        let size = int64 x * int64 y

        let real i = Complex(i, 0.)
        let imaginary i = Complex(0., i)

        let seed = real 0.
        let isGood (n:Complex) = (n.Real <> infinity && n.Imaginary <> infinity) //nan is not an issue, unless we messed up
        Array.Parallel.init x (fun i -> 
            let start = startx + (real (float i) * res)
            Array.Parallel.init y (fun j ->
                let rec check c v = function |n when n = iters -> n |n -> if isGood v then check c ((v * v)+c) (n+1) else n
                let init = start + (starty + (real (float j) * res))
                check init init 0
                |> System.BitConverter.GetBytes
            )
            |> Array.concat
        )
        |> Array.concat
        |> client.Client.Send
    |_ -> printfn "Mandelbrot <ip address> <port>"; exit -1