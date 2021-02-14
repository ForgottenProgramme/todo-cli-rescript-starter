/*
Sample JS implementation of Todo CLI that you can attempt to port:
https://gist.github.com/jasim/99c7b54431c64c0502cfe6f677512a87
*/

/* Returns date with the format: 2021-02-04 */
let getToday: unit => string = %raw(`
function() {
  let date = new Date();
  return new Date(date.getTime() - (date.getTimezoneOffset() * 60000))
    .toISOString()
    .split("T")[0];
}
  `)

type fsConfig = {encoding: string, flag: string}

/* https://nodejs.org/api/fs.html#fs_fs_existssync_path */
@bs.module("fs") external existsSync: string => bool = "existsSync"

/* https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options */
@bs.module("fs")
external readFileSync: (string, fsConfig) => string = "readFileSync"

/* https://nodejs.org/api/fs.html#fs_fs_writefilesync_file_data_options */
@bs.module("fs")
external appendFileSync: (string, string, fsConfig) => unit = "appendFileSync"

@bs.module("fs")
external writeFileSync: (string, string, fsConfig) => unit = "writeFileSync"

/* https://nodejs.org/api/os.html#os_os_eol */
@bs.module("os") external eol: string = "EOL"

@bs.scope("process") @bs.val external argv: array<string> = "argv"


let encoding = "utf8"

/*
NOTE: The code below is provided just to show you how to use the
date and file functions defined above. Remove it to begin your implementation.
*/

type command =
  | Help
  | Ls
  | Add(option<string>)
  | Del(option<int>)
  | Done(option<int>)
  | Report

let parser = (~cmd: string, ~arg: option<string>): command => {
  let cmd = cmd->Js.String.trim->Js.String.toLocaleLowerCase
  let pos = arg->Belt.Option.flatMap(str => str->Belt.Int.fromString)
  switch cmd {
  | "help" => Help
  | "ls" => Ls
  | "add" => Add(arg)
  | "del" => Del(pos)
  | "done" => Done(pos)
  | "report" => Report
  | _ => Help
  }
}

let todo_file =  "todo.txt"
let done_file = "done.txt"


let help_text = `Usage :-
$ ./todo add "todo item"  # Add a new todo
$ ./todo ls               # Show remaining todos
$ ./todo del NUMBER       # Delete a todo
$ ./todo done NUMBER      # Complete a todo
$ ./todo help             # Show usage
$ ./todo report           # Statistics`


let readFile = (filename: string): array<string> => {
  if !existsSync(filename) {
    []
  } else {
    let text = readFileSync(filename, {encoding: encoding, flag: "r"})
    let lines = Js.String.split(eol, text)
    let lines = Js.Array.filter(todo => todo !== "", lines)
    lines
  }
}
let writeFile = (filename: string, lines: array<string>) => {
  if Belt.Array.length(lines) == 1 {
    let text = lines[0] ++ eol
    writeFileSync(filename, text, {encoding: encoding, flag: "w"})
  }
  else{
    let text = Belt.Array.joinWith(lines,eol, x=> x)
    writeFileSync(filename, text, {encoding: encoding, flag: "w"})
  }
}

let appendToFile = (filename: string, text: string) => {
  appendFileSync(filename, text ++ eol , {encoding: encoding, flag: "a"})
}

let updateFile = (filename: string, updaterFn: array<string> => array<string>) => 
{
  let contents = readFile(filename)
  let new_contents = updaterFn(contents)
  writeFile(done_file, new_contents)
}


let cmdHelp = () => {
  Js.log(help_text);
}

let cmdLs = () => {
  let todos = readFile(todo_file)
  if Belt.Array.length(todos) == 0 {
    Js.log("There are no pending todos!")
  }
  else{
  todos
  ->Belt.Array.reverse
  ->Belt.Array.reduceWithIndex("", (acc, todo, index) => 
  acc ++ `[${(todos->Js.Array.length - index)->Belt.Int.toString}] ${todo}${eol}`
  )
  ->Js.log
  }
}

let cmdAddTodo = (text: option<string>) => {
  switch text {
  | Some(text) =>
    appendToFile(todo_file, text)
    Js.log(`Added todo: "${text}"`)

  | None => Js.log("Error: Missing todo string. Nothing added!")
  }
}

let cmdDelTodo = (arg: option<int>) => {
  switch arg {
  | Some(number) =>
    if existsSync(todo_file) {
      updateFile(todo_file, todos => {
        if number < 1 || number > Belt.Array.length(todos) {
          Js.log(`Error: todo #${Belt.Int.toString(number)} does not exist. Nothing deleted.`)
          todos
        } else {
          let todos = Js.Array.filteri((_, index) => index + 1 != number, todos)
          Js.log(`Deleted todo #${Belt.Int.toString(number)}`)
          todos
        }
      })
    }
  | None => Js.log("Error: Missing NUMBER for deleting todo.")
  }
}

let cmdMarkDone = (arg: option<int>) => {
  switch arg {
  | Some(number) =>
    let todos = readFile(todo_file)
    if number < 1 || number > Belt.Array.length(todos) {
      Js.log(`Error: todo #${Belt.Int.toString(number)} does not exist. Nothing Marked as done.`)
    } else {
      let completedTodo = todos[number - 1]
      let todos = Js.Array.filteri((_, index) => index != number - 1, todos)
      writeFile(todo_file, todos)
      appendToFile(done_file, `x ${getToday()} ${completedTodo}`)
      Js.log(`Marked todo #${Belt.Int.toString(number)} as done.`)
    }

  | None => Js.log("Error: Missing NUMBER for marking todo as done.")
  }
  }


  let cmdReport = () => {
  let pending = readFile(todo_file)->Belt.Array.length
  let completed = readFile(done_file)->Belt.Array.length
  Js.log(`${getToday()} Pending : ${Belt.Int.toString(pending)} Completed : ${Belt.Int.toString(completed)}`)
}


let cmd = argv->Belt.Array.get(2)->Belt.Option.getWithDefault("help")
let cmdArg = argv->Belt.Array.get(3)
let cmd: command = parser(~cmd, ~arg=cmdArg)

switch cmd {
| Help => cmdHelp()
| Ls => cmdLs()
| Add(todo) => cmdAddTodo(todo)
| Del(pos) => cmdDelTodo(pos)
| Done(pos) => cmdMarkDone(pos)
| Report => cmdReport()
}



