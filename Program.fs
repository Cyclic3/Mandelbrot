open Microsoft.FSharp.NativeInterop
open System.Numerics
open FSCL.Compiler
open FSCL.Language
open FSCL.Runtime
open FSCL
open OpenCL
open Microsoft.FSharp.Quotations
#nowarn "9"
let real i = Complex(i, 0.)
let imaginary i = Complex(0., i)
[<ReflectedDefinition>]
[<Kernel>]
let iter (n : Complex[], dest : Complex[], b : bool[], wi : WorkItemInfo) = 
    let gid = wi.GlobalID(0)
    if b.[gid] then
        let x = n.[gid]
        let c = dest.[gid]
        let mag = x.Magnitude
        let ans = not(mag = infinity || mag <> mag)
        b.[gid] <- false
        n.[gid] <- (x*x) + c
let compiler = new Compiler()
let do_iter(n : Complex[], dest : Complex[]) =
    OpenCL.OpenCLPlatform.Platforms.Count |> printfn "%A"
    //com_iter.Code |> printfn "%A" 
    let b = Array.init n.Length (fun _ -> true)
    let ws = new WorkSize(n.LongLength, 64L)
    let v = <@ iter(n, dest, b, ws) @>

    v.Run()
    b
let thing(i:byte) = 
    if i < 128uy then i*2uy
    else 255uy-(i*2uy)
let get_nice_colour j min max=
    let H = (float max - float j) * 270. / (float max - float min)
    //http://www.rapidtables.com/convert/color/hsv-to-rgb.htm
    let X = 1. - abs(((H/60.) % 2.) - 1.)
    let r',g',b' = 
        if     H < 60.    then 1.,X ,0.
        elif H < 120. then X ,1.,0.
        elif H < 180. then 0.,1.,X
        elif H < 240. then 0.,X ,1.
        elif H < 300. then X ,0.,1.
        else                 1.,0.,X
    printfn "%A" (r',g',b')
    r',g',b'
[<EntryPoint>]
let main _ = 
    let startx,starty = 
        -2.,-2.
        //printf "startx, starty: "
        //System.Console.ReadLine()
        //|> (fun s -> s.Replace(" ","").Split(','))
        //|> (function
        //        |[|startx;starty|] -> float startx, float starty
        //        |_ -> printfn "Bad input"; exit 0
        //        )
    let finishx,finishy =
        1.,1.
        //printf "finishx, finishy: "
        //System.Console.ReadLine()
        //|> (fun s -> s.Replace(" ","").Split(','))
        //|> (function
        //        |[|finishx;finishy|] -> float finishx, float finishy
        //        |_ -> printfn "Bad input"; exit 0
        //        )
    printfn "From (%f, %f) to (%f, %f)" startx starty finishx finishy
    let res = printf "res: "; System.Console.ReadLine()|>float
    let width    = abs(startx) + abs(finishx)
    let height = abs(starty) + abs(finishy)
    let x = width  / res |> int
    let y = height / res |> int
    let size = int64 x * int64 y

    let seed = real 0.
    let n_bad = ref 0L
    let isGood (n:Complex) = 
        let mag = n.Magnitude
        let ans = (mag = infinity || mag <> mag) |> not
        if not ans then System.Threading.Interlocked.Increment(n_bad) |> ignore
        ans
    printf "Creating bitmap array..."
    let bm = new System.Drawing.Bitmap(x,y)
    printfn "Done"
    let drawone (r:float,g:float,b:float) (x,y) = 
        let c = System.Drawing.Color.FromArgb(r*255. |> int, g*255. |> int, b*255. |> int)
        lock bm (fun () -> bm.SetPixel(x,y,c))
    let rec progress i stop mb (res:(float*float*float)[][]) =
        if i <= stop then
            printf "%i -> " i
            let d' = Array.map do_iter mb
            let c = get_nice_colour i 0 stop
            Array.Parallel.iteri (fun x a ->
                Array.Parallel.iteri (fun y ->
                    function 
                    |true -> res.[x].[y] <- c
                    |_ -> ()
                ) a
            ) d'
            progress (i+1) stop mb res
        else res
    printfn "%ix%i\nGenerating set..." x y
    let mb = 
        Array.Parallel.init x (fun x' -> 
            let x' = float x' * res + float(startx)
            Array.Parallel.init y (fun y' -> 
                let y' = float y' * res + float(starty)
                let i = Complex(x', y')
                i, i
            )
            |> Array.unzip
        )
    printfn "done"
    let m = progress 0 40 mb (Array.init x (fun _ -> Array.init y (fun _ -> 0.,0.,0.)))
    Array.iteri (fun x a ->
        Array.iteri (fun y c -> 
            drawone c (x,y)
        ) a
    ) m
    bm.Save("YAY.bmp",System.Drawing.Imaging.ImageFormat.Bmp)
    printfn "Saved"
    0
