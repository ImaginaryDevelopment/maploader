module
    /// purpose: extract core logic into more testable generic code
    MapLoader.App

open MapLoader.Schema
open MapLoader.Helpers

type Path = System.IO.Path
type ConsoleColor = System.ConsoleColor
type ColorPrintFormat<'t> = BlackFox.ColoredPrintf.ColoredPrintf.ColorPrintFormat<'t>
type ConsoleKey = System.ConsoleKey


let getTF2Dir (fs:FsWrapper, fReadLine) =
    let def = @"C:\Program Files (x86)\Steam\steamapps\common\Team Fortress 2"
    if fs.DirectoryExists def then def
    else
        let rec getting () =
            printf "Enter the path for your 'Team Fortress 2' folder >"
            let dir = fReadLine ()
            if
                fs.DirectoryExists dir
                && Path.Combine(dir, @"tf") |> fs.DirectoryExists
                && Path.Combine(dir, @"hl2.exe") |> fs.FileExists
            then
                dir
            else
                getting ()
        getting ()

let getConfig (fss:FsSerializer) tf2 =
  match fss.TryDeserializeFile<Config> "config.json" with
  | Some x -> x
  | None ->
    let cfg:Config = {
      directories= [|
        Path.Combine(tf2, @"tf\maps")
        Path.Combine(tf2, @"tf\download\maps")
      |]
      tf2= Path.Combine(tf2, @"hl2.exe")
      pageSize= 25
      showAuthor= true
      launchOptions= "-novid -windowed -noborder -condebug"
    }
    fss.SerializeToFile<Config> cfg "config.json"
    cfg

let getMaps (fs:FsWrapper) (config:Config) =
    let files =
        config.directories
        |> Seq.collect(fun dir ->
            fs.DirectoryGetFiles dir
            |> Seq.filter(fun x -> x.Contains ".bsp" && not (x.Contains ".bz2"))
        )
        |> List.ofSeq
    let uNames =
        files
        |> List.map Path.GetFileNameWithoutExtension
        |> List.sort
        |> List.distinct
    uNames, files |> List.map fs.GetFileInfo

let getUsername {showAuthor=showAuthor} cache users display (mapName:string) =
  if showAuthor then
    let x: string option =
      maybe {
        let! (mi : mapinfo) = cache |> Map.tryFind mapName
        let! user = users |> Map.tryFind mi.userid
        match display |> Map.tryFind user.display with
        | None -> return sprintf "| %s" user.username
        | Some d -> return sprintf "¦ $%s[%s]" d user.username
      }
    x
  else None
  |> Option.defaultValue ""

type RedrawArgs = {
    Maps: string list
    Config:Config
    Top:int
    Page: int
    Index: int
}

let redraw (cc:CConsole.ColorConsole) (getUsername,displayMaps) {Maps=maps; Config={pageSize=pageSize}; Top=top; Page= page; Index= i} =
        if cc.CursorTop = (top + ((1 + page) * pageSize)) then
            cc.ColorPrintfn "  $darkgray[[Load %i more...\]]" pageSize
        else
            let un = getUsername maps.[cc.CursorTop - top]
            cc.ColorPrintf (ColorPrintFormat(sprintf "  %s %s" (maps.[cc.CursorTop - top]) un))
            cc.CursorTop <- cc.CursorTop + i
            cc.CursorLeft <- 0
        if cc.CursorTop = (top + ((1 + page) * pageSize)) then
            cc.ColorPrintf "$green[> %s]                         " (maps.[cc.CursorTop - top])
            displayMaps top (page+1) maps
        else
            let un = getUsername maps.[cc.CursorTop - top]
            cc.ColorPrintf (ColorPrintFormat(sprintf "$green[> %s] %s" (maps.[cc.CursorTop - top]) un))
        cc.CursorLeft <- 0

let rec displayMaps config cc (pw:ProcWrapper) (getUsername,start) cache top page (maps:string List) =
    let redraw i = redraw cc (getUsername,displayMaps config cc pw (getUsername,start) cache) {Maps=maps; Config= config; Top=top;Page=page;Index=i}
    cc.CursorVisible <- false
    cc.ForegroundColor <- System.ConsoleColor.Gray
    cc.CursorLeft <- 0

    let _m =
        maps
        |> List.skip((page * config.pageSize))
    _m |> List.take(if _m.Length > config.pageSize then config.pageSize else _m.Length)
    |> List.iteri(fun i x ->
        if i = 0 then
            cc.ColorPrintfn (ColorPrintFormat(sprintf "$green[> %s] %s" x (getUsername x)))
        else
            cc.ColorPrintfn (ColorPrintFormat(sprintf "  %s %s" x (getUsername x)))
    )
    if _m.Length >= config.pageSize-1 then
        let _m2 = maps |> List.skip(((1+page) * config.pageSize))
        cc.ColorPrintfn "  $darkgray[[Load %i more...\]]" (if _m2.Length >= config.pageSize then config.pageSize else _m2.Length)
    cc.CursorTop <- top + (page * config.pageSize)
    let mutable active = true
    while true do
        let key = cc.ReadKey true
        match key.Key with
        | ConsoleKey.UpArrow when cc.CursorTop > top ->
            redraw -1
        | ConsoleKey.DownArrow when cc.CursorTop < (top + ((1 + page) * config.pageSize) - (if _m.Length < config.pageSize then config.pageSize - _m.Length + 1 else 0)) ->
            redraw 1
        | ConsoleKey.Enter ->
            cc.Title <- sprintf "Playing %s" (maps.[cc.CursorTop - top])
            cc.ColorPrintf (ColorPrintFormat(sprintf "$magenta[> %s] %s" (maps.[cc.CursorTop - top]) (getUsername maps.[cc.CursorTop - top])))
            cc.CursorLeft <- 0
            pw.Start (sprintf "-hijack +map %s %s" (maps.[cc.CursorTop - top]) config.launchOptions)
        | ConsoleKey.F when cache |> Map.containsKey (maps.[cc.CursorTop - top]) && cache.[maps.[cc.CursorTop - top]].forumid <> 0 ->
            sprintf @"https://tf2maps.net/downloads/%i/" cache.[maps.[cc.CursorTop - top]].forumid
            |> launchBrowser
        | ConsoleKey.A when cache.ContainsKey (maps.[cc.CursorTop - top]) && cache.[maps.[cc.CursorTop - top]].forumid <> 0 ->
            sprintf @"https://tf2maps.net/downloads/authors/%i/" cache.[maps.[cc.CursorTop - top]].userid
            |> launchBrowser
        | ConsoleKey.Escape ->
            cc.CursorTop <- (top + ((1 + page) * config.pageSize) - (if _m.Length < config.pageSize then config.pageSize - _m.Length + 1 else 0)) + 1
            cc.ColorPrintfn "$yellow[Exiting current search]"
            active <- false
            start ()
        | _ -> ()