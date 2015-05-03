import Signal exposing (Signal, Address)
import Graphics.Element exposing (Element, show, flow, right, down, container, middle, centered)
import Text exposing (fromString)
import Graphics.Input as Input
import Window
import String
import Char


main : Signal Element
main =
  commands.signal
    |> Signal.foldp update (Start zero)
    |> Signal.map2 view Window.dimensions

-- INPUTS

-- A mailbox to send commands to, and to get commands from (see possible commands in model below)
-- We initialize it with the "Clear" command
commands : Signal.Mailbox Command
commands =
  Signal.mailbox Clear


-- MODEL

type Operation
  = Add
  | Subtract

type Command
    = Digit String
    | Op Operation
    | Equals
    | Clear

type alias Number = { negative : Bool, string : String }

zero : Number
zero = { negative = False, string = "" }

-- This is the state of the calculator
-- We either have a simple number, or are in the middle of an operation
type State
    = Start Number
    | Operator Number Operation Number


floatToNumber : Float -> Number
floatToNumber float =
    let neg = if (float < 0) then True else False
    in { negative = neg, string = (toString float)}

numberToFloat : Number -> Float
numberToFloat number =
    let neg = if number.negative then -1 else 1
    in
        case String.toFloat number.string of
          Ok n -> n * neg
          Err _ -> 0

-- DISPLAY

view : (Int, Int) -> State -> Element
view (w, h) state =
  container w h middle (flow down
  [ container w 60 middle (centered (fromString "This is a simple calculator made with Elm-lang"))
  , container w (6*60) middle (calculator state)])


-- Each button in the calculator sends a command to the command mailbox
calculator : State -> Element
calculator  state =
  flow down
  [
  container 180 60 middle (centered (fromString ((displayState state))))
  , flow right
      [ container 60 60 middle (Input.button (Signal.message commands.address (Digit "0")) "0")
      , container 60 60 middle (Input.button (Signal.message commands.address (Op Add)) "+")
      , container 60 60 middle (Input.button (Signal.message commands.address (Op Subtract)) "-")
      ]
  , flow right
      [ container 60 60 middle (Input.button (Signal.message commands.address (Digit "1")) "1")
      , container 60 60 middle (Input.button (Signal.message commands.address (Digit "2")) "2")
      , container 60 60 middle (Input.button (Signal.message commands.address (Digit "3")) "3")
      ]
  , flow right
      [ container 60 60 middle (Input.button (Signal.message commands.address (Digit "4")) "4")
      , container 60 60 middle (Input.button (Signal.message commands.address (Digit "5")) "5")
      , container 60 60 middle (Input.button (Signal.message commands.address (Digit "6")) "6")
      ]
  , flow right
      [ container 60 60 middle (Input.button (Signal.message commands.address (Digit "7")) "7")
      , container 60 60 middle (Input.button (Signal.message commands.address (Digit "8")) "8")
      , container 60 60 middle (Input.button (Signal.message commands.address (Digit "9")) "9")
      ]
  , flow right
      [ container 120 60 middle (Input.button (Signal.message commands.address Equals) "=")
      , container 60 60 middle (Input.button (Signal.message commands.address Clear) "clear")
      ]
  ]

displayState : State -> String
displayState state =
    case state of
      Start n -> let digits = toString (numberToFloat n)
                     neg = if n.negative then "-" else ""
                 in neg ++ digits
      Operator n op m ->
        n.string ++ (stringFromOp op) ++ m.string

stringFromOp : Operation -> String
stringFromOp op =
  case op of
    Add -> "+"
    Subtract -> "-"

-- UPDATE
update : Command -> State -> State
update command state =
    case command of
      -- Appends digit to the current number
      Digit digit ->
          -- Only allow 10 digit numbers
          let isShort n = String.length (String.filter Char.isDigit n.string) < 10
          in  modifyNumber (appendIf isShort digit) state
      Op Add ->
          operator (Add) state
      Op Subtract ->
          operator (Subtract) state
      Equals ->
          Start (equals state)
      Clear ->
          clear state

-- Used to modify the current number in the calculator (usually to append more digits)
modifyNumber : (Number -> Number) -> State -> State
modifyNumber f state =
    case state of
      Start n -> Start (f n)
      Operator n op m -> Operator n op (f m)

appendIf : (Number -> Bool) -> String -> Number -> Number
appendIf isOkay str number =
    if isOkay number
    then { number | string <- number.string ++ str }
    else number

-- takes in an operation and goes to the next state
operator : Operation -> State -> State
operator op state =
    case state of
      -- If we are in the start state, the calculator only has a number
      Start n -> Operator n op zero
      -- If we are in an operate state, then stay in an operate state
      Operator n _ m ->
          Operator (if m == zero then n else equals state) op zero

clear : State -> State
clear state =
    case state of
      Start n -> Start zero
      Operator n op m ->
          if m == zero then Start zero else Operator n op zero


-- The actual calculation is done here
equals : State -> Number
equals state =
    case state of
      Start n -> n
      Operator n op m -> case op of
        Add -> floatToNumber
          ((numberToFloat n)
          + (if m == zero then numberToFloat n else numberToFloat m))
        Subtract -> floatToNumber
          ((numberToFloat n)
          - (if m == zero then numberToFloat n else numberToFloat m))

