open Microsoft.FSharp.NativeInterop
open System.Numerics
open System.Net
open System.Net.Sockets
#nowarn "9"
type InfiniteSequence<'T>(f:'T -> uint64 -> 'T,init:'T) = 
    let mutable i = 0uL
    let mutable acc = init
    interface System.Collections.Generic.IEnumerable<'T> with
        member x.GetEnumerator() = 
            {
                new System.Collections.Generic.IEnumerator<'T> with 
                    member x.Current with get() = acc:'T
                    member x.Current = acc :> obj
                    member x.Dispose() = ()
                    member x.Reset() = i<-0uL;acc<-init
                    member x.MoveNext() = acc <- f acc i; i <- i+1uL; true
            }
        member x.GetEnumerator() = 
            {
                new System.Collections.IEnumerator with 
                    member x.Current = acc :> obj
                    member x.Reset() = i<-0uL;acc<-init
                    member x.MoveNext() = acc <- f acc i; i <- i+1uL; true
            }
    member x.Current = acc
    member x.GetNext() = 
        acc <- f acc i; 
        i <- i+1uL;
        acc
[<EntryPoint>]
let main  = 
    function 
    |[|ipaddress;port|] ->
        let client = new TcpClient()
        client.Connect(IPAddress.Parse ipaddress, int port)
        while client.Available = 0 do ()
        let b = Array.zeroCreate<byte> 44
        client.Client.Receive(b) |> ignore
        let startx = System.BitConverter.ToDouble(b,0)
        let starty = System.BitConverter.ToDouble(b,8)
        let finishx = System.BitConverter.ToDouble(b,16)
        let finishy = System.BitConverter.ToDouble(b,24)
        let res = System.BitConverter.ToDouble(b,32)
        let iters = System.BitConverter.ToInt32(b,40)
        printfn "From (%f, %f) to (%f, %f) in %f steps" startx starty finishx finishy res
        let width    = abs(startx) + abs(finishx)
        let height = abs(starty) + abs(finishy)
        let x = width    / res |> int
        let y = height / res |> int
        let size = int64 x * int64 y

        let real i = Complex(i, 0.)
        let imaginary i = Complex(0., i)

        let seed = real 0.
        let n_bad = ref 0L
        let isGood (n:Complex) = 
            let mag = n.Magnitude
            let ans = (mag = infinity || mag <> mag) |> not
            if not ans then System.Threading.Interlocked.Increment(n_bad) |> ignore
            ans
        let orbitfunc c = 
            InfiniteSequence<_>(
                (fun x _ -> if isGood x then (x*x) + c else real infinity),
                seed
            )
        let mandelbrot res (startx,finishx) (starty,finishy) = 
            let x' = Array.Parallel.init x (fun i -> (float i * res + startx))
            let y' = Array.Parallel.init y (fun i -> (float i * res + starty))
            let cat = ref size
            let t = 
                async{
                    let mouse = ref size
                    while !cat > 0L do
                        if (!mouse - !cat) > size/100L then 
                            mouse := !cat
                            printfn "\t%i%% complete" ((size - !mouse) / (size/100L))
                } |> Async.StartAsTask
            Array.Parallel.map(fun i -> 
                Array.Parallel.map (fun j ->
                    System.Threading.Interlocked.Decrement(cat)|>ignore
                    Complex(i,j)|>orbitfunc
                ) y'
            ) x'
            |> fun i -> t.Wait();i
        let iter_all f v = Array.map(Array.map(fun (i:InfiniteSequence<_>) -> i.GetNext()|>f)) v

        let thing(i:byte) = 
            if i < 128uy then i*2uy
            else 255uy-(i*2uy)
        let get_nice_colour j min max=
            let H = (float max - float j) * 270. / (float max - float min)
            //http://www.rapidtables.com/convert/color/hsv-to-rgb.htm
            let X = 1. - abs(((H/60.) % 2.) - 1.)
            let r',g',b' = 
                if   H < 60.  then 1.,X ,0.
                elif H < 120. then X ,1.,0.
                elif H < 180. then 0.,1.,X
                elif H < 240. then 0.,X ,1.
                elif H < 300. then X ,0.,1.
                else                 1.,0.,X
            printfn "%A" (r',g',b')
            r',g',b'
        let rec progress i stop mb (res:(float*float*float)[][]) =
            if i <= stop then
                printf "%i -> " i
                n_bad := 0L
                let d' = iter_all isGood mb 
                printfn "[%i/%i] " !n_bad size
                let c = get_nice_colour i 0 stop 
                Array.Parallel.iteri (fun x a ->
                    Array.Parallel.iteri (fun y ->
                        function 
                        |true -> res.[x].[y] <- c
                        |_ -> ()
                    ) a
                ) d'
                printfn "%f%% complete" ((float i / float stop)*100.)
                progress (i+1) stop mb res
            else res
        printfn "%ix%i\nGenerating set..." x y
        let mb = mandelbrot res (startx,finishx) (starty,finishy)
        printfn "done"
        progress 0 iters mb (Array.Parallel.init x (fun _ -> Array.zeroCreate y))
        |> Array.Parallel.map (Array.Parallel.map (fun (r,g,b) -> Array.concat [|System.BitConverter.GetBytes r; System.BitConverter.GetBytes g; System.BitConverter.GetBytes g|]))
        |> Array.concat
        |> Array.concat
        |> client.Client.Send
        |> printfn "Sent %i bytes"
        0
    |_ -> printfn "Mandelbrot <ip address> <port>"; exit -1