﻿open Microsoft.FSharp.NativeInterop
open global.Quadruple
#nowarn "9"
//
type QuadComplex(r : Quad, i : Quad) = 
    struct
        member x.real = r
        member x.imaginary = i
        member x.Magnitude = Quad.Pow(Quad.Pow(x.real, 2.) + Quad.Pow(x.imaginary, 2.),1./2.)
        member x.Square() = QuadComplex(x.real * x.real,x.imaginary * x.imaginary)
        static member (+) (a:QuadComplex,b:QuadComplex) = QuadComplex(a.real+b.real,a.imaginary+b.imaginary)
    end
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
    Gtk.Application.Init()
    let _int_quad(i:int) = Quad(int64 i,0L)
    let startx,finishx = _int_quad -2, _int_quad 1
    let starty,finishy = _int_quad -1, _int_quad 1
    let res = System.Console.ReadLine()|>Quad.Parse
    let width  = abs(startx) + abs(finishx)
    let height = abs(starty) + abs(finishy)
    let _quad_int(q:Quad) = 
        let s = q.ToString(QuadrupleStringFormat.ScientificExact) 
        printfn "%s" s
        int s
    let x = width  / res |> Quad.Truncate |> _quad_int
    let y = height / res |> Quad.Truncate |> _quad_int
    let window = new Gtk.Window("YAY!")
    window.SetSizeRequest(x,y)
    window.ShowAll()
    window.FocusInEvent.Add(fun _ -> window.ShowAll())
    window.AddEvents(Gdk.EventMask.AllEventsMask |> int)
    let darea = new Gtk.DrawingArea()
    window.Add(darea)
    //async{Gtk.Application.Run()}|>Async.Start
    System.Threading.Thread.Sleep 1000

    let real i = QuadComplex(i, Quad())
    let imaginary i = QuadComplex(Quad(), i)

    let seed = Quad() |> real
    let n_bad = ref 0L
    let isGood (n:QuadComplex) = 
        let ans = (n.real |> Quad.IsInfinity || n.imaginary |> Quad.IsInfinity) |> not
        if not ans then System.Threading.Interlocked.Increment(n_bad) |> ignore
        ans
    let orbitfunc c = 
        InfiniteSequence<_>(
            (fun x _ -> if isGood x then x.Square() + c else real Quad.PositiveInfinity),
            seed
        )
    let mandelbrot res (startx:Quad,finishx:Quad) (starty,finishy) = 
        let x' = Array.Parallel.init x (fun i -> (_int_quad i * res + startx))
        let y' = Array.Parallel.init y (fun i -> (_int_quad i * res + starty))

        Array.Parallel.map(fun i -> 
            Array.Parallel.map (fun j ->
                QuadComplex(i,j)|>orbitfunc
            ) y'
        ) x'
    let iter_all f v = Array.map(Array.map(fun (i:InfiniteSequence<_>) -> i.GetNext()|>f)) v
    //let deref<'T when 'T : unmanaged> : nativeint -> 'T = NativePtr.ofNativeInt >> NativePtr.read
    //0.004
    //let bm = new System.Drawing.Bitmap(x,y)
    let draw (r:float,g:float,b:float) a = 
        //window.allocate

        let context = Gdk.CairoHelper.Create(darea.Window)
        context.LineWidth <- 0.
        context.SetSourceRGB(r, g, b)
        Array.iteri (fun i l -> 
            Array.iteri (fun j e -> 
                if e then 
                    context.Rectangle(float i,float j,1.,1.)
            ) l
        ) a
        (
            let c = Gdk.CairoHelper.Create(darea.Window)
            c.SetSourceRGB(1.,1.,1.)
            c.Paint()
            c.Dispose()
        )
        context.Fill()
        window.ShowAll()
        while Gtk.Application.EventsPending() do Gtk.Application.RunIteration()
        //window.Remove(darea)
        context.Dispose()
        //darea.Dispose()
    let thing(i:byte) = 
        if i < 128uy then i*2uy
        else 255uy-(i*2uy)
    let get_nice_colour j min max=
        let range = max-min |> float
        let H = (float j-float min)*270./range
        //http://www.rapidtables.com/convert/color/hsv-to-rgb.htm
        let X = abs(1. - ((H/60.) % 2.) - 1.)
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
            printf "[%i] " !n_bad
            let c = get_nice_colour i 0 stop 
            draw c d'
            if !n_bad > 0L then System.Console.ReadKey(true)|>ignore
            //printfn "%i" i
            //d.Dispose()
            progress (i+1) stop mb //d'
    

    QuadComplex(_int_quad 3,_int_quad 4).Magnitude |> printfn "%A"
    let mb = mandelbrot res (startx,finishx) (starty,finishy)
    progress 0 2000 mb
    //System.Console.ReadLine()
    //bm.Save("YAY.bmp")
    Gtk.Application.Run()
    0