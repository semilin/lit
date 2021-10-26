import stacks
import std/tables
import std/strutils
from strformat import fmt
import system
import os

type LitType = enum
  integer,
  character,
  literal,
  litsubexp

type Elem = ref object
  case t: LitType
  of integer: intVal: int
  of character: charVal: char
  of literal: litVal: string
  of litsubexp: subexpVal: string

type Operator = proc(x: var Stack[Elem])

func stringvalue(e: Elem): string =
  case e.t:
  of integer: return intToStr(e.intVal)
  of character: return fmt"$e.charVal"
  of literal: return e.litVal
  of litsubexp: return "( " & e.subexpVal & ")"

var replMode = false
var safe = true

var yline = 1
var xtoken = 1

var builtIns = initTable[string, Operator]()
var variables = initTable[string, Elem]()

proc error(s: string) =
  stdout.write "\e[31m" & "error\e[0m " & fmt"on line {yline}, token {xtoken}:" & "\n"
  echo "    " & s
  if not replMode:
    system.quit(1)
  else:
    safe = false

proc underflowError() =
  error(fmt"stack underflow")

proc funcTypeError(f: string, expectedT: LitType, got: Elem) =
  error(fmt"function {f} expected type {expectedT}, but instead got {got.t} of value {got.stringvalue}")

proc safePop(x: var Stack[Elem]): Elem =
  if not x.isEmpty():
    return x.pop()
  else:
    underflowError()

proc eval(s: string, main: bool): Stack[Elem] =
  safe = true
  var insub = false
  var stack = Stack[Elem]()
  var startlen = 0
  if main:
    yline = 0
  for line in s.splitLines():
    if main:
      yline += 1
      xtoken = 0
    var lit = false
    var now = false
    
    for word in line.splitWhiteSpace():
      if main:
        xtoken += 1
      if not safe:
        return stack
      if builtIns.hasKey(word):
        if lit:
          stack.push(Elem(t: literal, litVal: word))
          lit = false
        elif insub:
          if not now:
            stack.push(Elem(t: literal, litVal: word))
          else:
            builtIns[word](stack)
            now = false
        else:
          builtIns[word](stack)
      elif word == "start":
        insub = true
        startlen = stack.len()
      elif word == "end":
        insub = false
        var str = ""
        for i in 1..stack.len()-startlen:
          let e = stack.pop()
          str = e.stringvalue & " " & str
        stack.push(Elem(t: litsubexp, subexpVal: str))
      elif word == "now":
        now = true
      elif word == "lit":
        lit = true
        if insub:
          stack.push(Elem(t: literal, litVal: word))
        continue
      elif word == "comment":
        break
      elif variables.hasKey(word):
        if not insub:
          if lit:
            stack.push(Elem(t: literal, litVal: word))
          else:
            stack.push(variables[word])
        else:
          if not now:
            stack.push(Elem(t: literal, litVal: word))
          else:
            stack.push(variables[word])
            now = false
      else:
        try:
          let i = word.parseInt()
          stack.push(Elem(t: integer, intVal: i))
        except:
          stack.push(Elem(t: literal, litVal: word))

      if lit:
        lit = false

  return stack

builtIns["add"] = proc(x: var Stack[Elem]) =
  let a = x.safePop()

  if not safe:
    return
  if a.t != integer:
    funcTypeError("add", integer, a)
    return

  let b = x.safePop()
  if not safe:
    return
  if b.t != integer:
    funcTypeError("add", integer, a)
    return

  x.push(Elem(t: integer, intVal: (a.intVal+b.intVal)))

builtIns["sub"] = proc(x: var Stack[Elem]) =
  let a = x.safePop()
  if a.t != integer:
    funcTypeError("sub", integer, a)
    return

  let b = x.safePop()
  if b.t != integer:
    funcTypeError("sub", integer, a)
    return

  x.push(Elem(t: integer, intVal: (b.intVal-a.intVal)))

builtIns["inc"] = proc(x: var Stack[Elem]) =
  let a = x.safePop()
  if not safe:
    return
  if a.t != integer:
    x.push(a)
    return

  a.intVal += 1
  x.push(a)

builtIns["dec"] = proc(x: var Stack[Elem]) =
  let a = x.safePop()
  if not safe:
    return
  if a.t != integer:
    funcTypeError("dec", integer, a)
    return

  a.intVal -= 1
  x.push(a)

builtIns["multi"] = proc(x: var Stack[Elem]) =
  let a = x.safePop()
  if not safe:
    return
  if a.t != integer:
    funcTypeError("multi", integer, a)
    return

  let b = x.pop()
  if not safe:
    return
  if b.t != integer:
    funcTypeError("multi", integer, b)
    return

  x.push(Elem(t: integer, intVal: (b.intVal*a.intVal)))

builtIns["print"] = proc(x: var Stack[Elem]) =
  let e = x.safePop()
  if not safe:
    return
  let s = e.stringvalue()
  echo s

builtIns["set"] = proc(x: var Stack[Elem]) =
  let name = x.safePop()
  if not safe:
    return
  if name.t != literal:
    funcTypeError("set", literal, name)
    return
    
  let val = x.safePop()
  if not safe:
    return

  variables[name.litVal] = val

builtIns["eval"] = proc(x: var Stack[Elem]) =
  let a = x.safePop()
  if not safe:
    return
  if a.t != litsubexp:
    funcTypeError("eval", litsubexp, a)
    return
  var y = eval(a.subexpVal, false)
  while not y.isEmpty():
    x.push(y.pop())

builtIns["repeat"] = proc(x: var Stack[Elem]) =
  let n = x.safePop()
  if not safe:
    return
  if n.t != integer:
    funcTypeError("repeat", integer, n)
    return
  let sub = x.safePop()
  if not safe:
    return
  if sub.t != litsubexp:
    funcTypeError("repeat", litsubexp, sub)
    return

  for i in 1..n.intVal:
    discard eval(sub.subexpVal, false)

builtIns["exit"] = proc(x: var Stack[Elem]) =
  let code = x.safePop()
  if not safe:
    return
  if code.t != integer:
    funcTypeError("exit", integer, code)
    return
  system.quit(code.intVal)


proc print(s: var Stack[Elem]) =
  while not s.isEmpty():
    stdout.write s.pop().stringvalue()
    if s.isEmpty():
      stdout.write "\n"
      break
    stdout.write " "

proc ctrlc() {.noconv.} =
  system.quit(0)

setControlCHook(ctrlc)

when isMainModule:
  if paramCount() > 0:
    let path = paramStr(1)
    try:
      let file = readFile(path)

      discard eval(file, true)
      system.quit(0)
    except OverflowDefect:
      error("integer overflow")
    except IOError:
      echo "There was a problem opening the file"
      system.quit(1)

  # REPL
  echo "LIT REPL v0.1"
  replMode = true
  while true:
    stdout.write "lit> "
    var line: string = readLine(stdin)
    var stack = line.eval(true)
    stack.print()
