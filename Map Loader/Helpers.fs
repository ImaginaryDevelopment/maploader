module MapLoader.Helpers
open System.IO
open Newtonsoft.Json

module List =
    let trySkip i items =
        if List.length items > i then
            items |> List.skip i |> Some
        else None

let getUnixNow () =
    (System.DateTime.UtcNow.Subtract(System.DateTime(1970, 1, 1)).TotalSeconds |> int64) - 604800L


// index, not page. pages IRL are numbered starting with zero
let tryGetPage index pageSize items =
    if index < 0 then failwithf "Page %i is an invalid page number" index
    match index with
    | 0 ->
        // should this return None if there are 0 items?
        items |> List.truncate pageSize |> Some
    | _ ->
        items |> List.trySkip (pageSize * index)

let launchBrowser (url:string) =
    System.Diagnostics.Process.Start url |> ignore<System.Diagnostics.Process>

type MaybeBuilder() =
    member inline _.Bind(m: 'a option, f: 'a -> 'b option) = Option.bind f m
    member this.Bind(m: 'a when 'a: null, f: 'a -> 'b option) = this.Bind(m |> Option.ofObj, f)
    member inline _.Return(x) = Some x
    member inline this.Zero() = this.Return()
    member inline _.BindReturn(x, f) = Option.map f x
    member inline _.Combine(m, f) = Option.bind f m

let maybe = MaybeBuilder()