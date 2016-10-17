#I "bin/Debug"
open Microsoft.FSharp.NativeInterop
#nowarn "9"
open System.Numerics
type Complex with
    static member get_One = new Complex(1.,0.) 
let real i = System.Numerics.Complex(i,0.)
let imaginary i = System.Numerics.Complex(0.,i)
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
                    member x.MoveNext() = acc <- f acc i; true
            }
let seed = real 0.
let isBad (n:Complex) = n.Real = infinity || n.Imaginary = infinity
let orbitfunc c = InfiniteSequence<_>((fun x _ -> x ** real 2.+c),seed)
let check iter c = c |> orbitfunc |> Seq.take iter |> Seq.exists isBad |> not
//let show n a = a |> Seq.take n |> Seq.map (fun (i,j:Complex) -> j.Real) |> Chart.Line

//mandelbrot (imaginary 1.) |> show 20
//Chart.Line [for i = 1 to 10 do yield i,i]
let start,res,finish = -2.,0.005,2.
let n = (abs(start) + abs(finish))/res
let mandelbrot iter = Array.Parallel.map(fun i -> Array.Parallel.map (fun j -> Complex(i,j)|>check iter) [|start..res..finish|]) [|start..res..finish|]

let deref<'T when 'T : unmanaged> : nativeint -> 'T = NativePtr.ofNativeInt >> NativePtr.read
let draw window (r,g,b) a = 
    let renderer' = SDL.SDL_CreateRenderer (window,-1,SDL.SDL_RendererFlags())
    SDL.SDL_SetRenderDrawColor(renderer',255uy, 255uy, 255uy, 255uy)
    SDL.SDL_RenderSetScale(renderer',1.f,1.f)//float32(res)*float32(dim)
    //let surface' = SDL.SDL_GetWindowSurface window
    let mutable rect = SDL.SDL_Rect(h=100,w=50,x=0,y=0)
    //SDL.SDL_RenderClear(renderer')#
    //SDL.SDL_SetRenderDrawColor(renderer',0uy, 0uy, 0uy, 255uy)
    //SDL.SDL_RenderClear(renderer')
    //SDL.SDL_RenderPresent renderer'
    SDL.SDL_SetRenderDrawColor(renderer',r, g, b, 255uy)
    //let surface =  deref<SDL.SDL_Surface> surface'
    Array.Parallel.iteri (fun i l ->
        Array.Parallel.iteri (fun j e ->
            if e then SDL.SDL_RenderDrawPoint(renderer',i,j)|>ignore) l) a

    //SDL.SDL_RenderFillRect(renderer',&rect)

    SDL.SDL_RenderPresent renderer'
    //{new System.IDisposable with member x.Dispose() = SDL.SDL_DestroyWindow(window)}
let rec progress i window  = //(d:System.IDisposable)
    let c = byte (i*10)
    let d' = mandelbrot i |> draw window (c,c,c)
    printfn "%i" i
    //d.Dispose()
    progress (i+1) window //d'

//------------------------------------------------------------------------------------------------------------------------------------------
let window' = SDL.SDL_CreateWindow("Yo!",SDL.SDL_WINDOWPOS_UNDEFINED,SDL.SDL_WINDOWPOS_UNDEFINED,int n,int n,SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN)
if SDL.SDL_Init(SDL.SDL_INIT_VIDEO) < 0 then failwith "=("
//let dim = 1000
progress 12 window' //{new System.IDisposable with member x.Dispose() = ()}
SDL.SDL_Delay(-1|>uint32)
#I "/usr/lib/cli/gtk-sharp-2.0"
#I "/usr/lib/cli/gdk-sharp-2.0"
#I "/usr/lib/cli/atk-sharp-2.0"
#I "/usr/lib/cli/glib-sharp-2.0"
#r "bin/Debug/OxyPlot.dll"
#r "bin/Debug/FSharp.Charting.Gtk.dll"
#load "packages/FSharp.Charting.Gtk.0.90.14/FSharp.Charting.Gtk.fsx"
open FSharp.Charting
open FSharp.Charting.ChartTypes
let get_nice_colour i min max = 
    let n = max-min
    let i = ((i-min)*1024)/max
    printfn "%A" i
    if   i <= 127 then i,0,0//just red
    elif i >= 767 then 0,0,i-639//just blue 768-127
    elif i >  510 then 765-i,255,0
    elif i >= 383 then i-383,510-i,0
    elif i >= 255 then 255,i-127,0
    elif i >  127 then i,i-127,0
    else                failwith "=("
let v = 
    [for i = 0 to 1024 do yield (get_nice_colour i 0 1024|>function |r,g,b -> ["red",i,r;"green",i,g;"blue",i,b])] 
    |> List.concat
    |> List.groupBy (function |i,_,_ -> i)
    |> List.item 1
    |> fun (s,l) -> Chart.Line(List.map (function |_,_,j -> j) l,s) //
