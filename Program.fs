﻿open Microsoft.FSharp.NativeInterop
open System.Numerics
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
let main _ = 
    ///Gtk.Application.Init()
    let startx,finishx = -2., 1.
    let starty,finishy = -1., 1.
    let res = System.Console.ReadLine()|>float
    let width  = abs(startx) + abs(finishx)
    let height = abs(starty) + abs(finishy)
    let x = width  / res |> int
    let y = height / res |> int
    let size = int64 x * int64 y
    ///let window = new Gtk.Window("YAY!")
    ///window.SetSizeRequest(x,y)
    ///window.ShowAll()
    ///window.FocusInEvent.Add(fun _ -> window.ShowAll())
    ///window.AddEvents(Gdk.EventMask.AllEventsMask |> int)
    ///let darea = new Gtk.DrawingArea()
    ///window.Add(darea)
    //async{Gtk.Application.Run()}|>Async.Start
    System.Threading.Thread.Sleep 1000

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
    //let deref<'T when 'T : unmanaged> : nativeint -> 'T = NativePtr.ofNativeInt >> NativePtr.read
    //0.004
    printf "Creating bitmap array..."
    let bm = new System.Drawing.Bitmap(x,y)
    printfn "Done"
    let draw (r:float,g:float,b:float) a = 
        //window.allocate

        ///let context = Gdk.CairoHelper.Create(darea.Window)
        ///context.LineWidth <- 0.
        ///context.SetSourceRGB(r, g, b)
        let c = System.Drawing.Color.FromArgb(r*255. |> int, g*255. |> int, b*255. |> int)
        Array.iteri (fun i l -> 
            Array.iteri (fun j e -> 
                if e then 
                    lock bm (fun () -> bm.SetPixel(i,j,c))
                    ///context.Rectangle(float i,float j,1.,1.)
            ) l
        ) a
        (
            //let c = Gdk.CairoHelper.Create(darea.Window)
            //c.SetSourceRGB(1.,1.,1.)
            //c.Paint()
            //c.Dispose()
        )
        ///context.Fill()
        ///window.ShowAll()
        ///while Gtk.Application.EventsPending() do Gtk.Application.RunIteration()
        //window.Remove(darea)
        ///context.Dispose()
        //darea.Dispose()
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
            else               1.,0.,X
        
        //printfn "%A,%A,%A" r g b
        //let r = r' * 255. |> byte
        //let g = g' * 255. |> byte
        //let b = b' * 255. |> byte
        printfn "%A" (r',g',b')
        r',g',b'
    let rec progress i stop mb = //(d:System.IDisposable)
        if i <= stop then
            printf "%i -> " i
            n_bad := 0L
            let d' = iter_all isGood mb 
            printfn "[%i/%i] " !n_bad size
            let c = get_nice_colour i 0 stop 
            draw c d'
            //printfn "%i" i
            //d.Dispose()
            progress (i+1) stop mb //d'
    printfn "%ix%i\nGenerating set..." x y
    let mb = mandelbrot res (startx,finishx) (starty,finishy)
    printfn "done"
    progress 0 40 mb
    //System.Console.ReadLine()
    bm.Save("YAY.bmp",System.Drawing.Imaging.ImageFormat.Bmp)
    printfn "Saved"
    0