/// purpose: hold shapes the app will depend on, no almost no logic
namespace MapLoader.Schema

type Config = {directories:string array; tf2:string; pageSize:int; showAuthor:bool; launchOptions:string}
type apiforumuser = {id:int; username:string; custom_title:string; display:int; avatar:string; updated:int64}
type apimapinfo = {map:string; url:string; notes:string; forum_id:int; forum_version:int; forum_icon:string; forum_username:string; forum_user_id:int; forum_tagline:string; forum_user:apiforumuser; discord_user_handle:string; added:int64}

type mapinfo = {userid: int; forumid:int}
type userinfo = {username:string; userid: int; display:int; updated:int64}

type CommandType =
    | Up
    | Down
    | Go
    | F
    | A
    | Esc

type Serializer =
    [<RequiresExplicitTypeArguments>]
    abstract member Deserialize<'t> : string -> 't
    [<RequiresExplicitTypeArguments>]
    abstract member Serialize<'t> : 't -> string

type IFileInfo =
    abstract member CreationTime : System.DateTime with get
    abstract member Name : string with get
// interface/abstract types aren't usually named with an I Prefix in F#
// but since we don't want to clash with IO.Directory, this seemed a nice way to do it
type FsWrapper =
    abstract member DirectoryExists: string -> bool
    abstract member FileExists: string -> bool
    abstract member ReadAllText: string -> string
    abstract member WriteAllText: path:string * text:string -> unit
    abstract member DirectoryGetFiles: path:string -> string array
    abstract member GetFileInfo: path: string -> IFileInfo

type FsSerializer =
    [<RequiresExplicitTypeArguments>]
    abstract member TryDeserializeFile<'t> : string -> 't option
    [<RequiresExplicitTypeArguments>]
    abstract member SerializeToFile<'t> : 't -> string -> unit

type ProcWrapper =
    abstract member Start: arguments:string -> unit

module CConsole =
    open BlackFox.ColoredPrintf
    type ColorConsole =
        abstract member CursorTop : int with get,set
        abstract member CursorVisible : bool with get,set
        abstract member ForegroundColor : System.ConsoleColor with get,set
        abstract member CursorLeft : int with get,set
        abstract member Title : string with get,set

        abstract member ReadKey : bool -> System.ConsoleKeyInfo

        abstract member ColorPrintfn : ColorPrintFormat<'t> -> 't
        abstract member ColorPrintf : ColorPrintFormat<'t> -> 't


    // let defaultCc =
    //     { new ColorConsole with
    //         member _.ColorPrintfn fmt = colorprintfn fmt
    //     }