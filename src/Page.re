/* This is the basic component. */

type status =
  | Cross
  | Circle
  | Empty;

type action =
  | Click(int, int);

type state = {
  board: array(array(status)),
  player: status,
  winner: status
};

let changePlayer = curr =>
if (curr == Cross) {
    Circle;
  } else {
    Cross;
  };

let rowCheck = row => {
  if (row[0] != Empty && row[0] == row[1] && row[0] == row[2]) {
    row[0]
  } else {
    Empty
  }
};

let winCheck = board => {
  let allChecks = ref(Array.fold_left((acc, row) => {
    let currCheck = rowCheck(row);
    if (currCheck != Empty) {
      currCheck
    } else {
      acc
    }
  }, Empty, board));
  for (i in 0 to 2) {
    let column = [| board[0][i], board[1][i], board[2][i] |];
    let currCheck = rowCheck(column);
    if (currCheck != Empty) {
      allChecks := currCheck;
    }
  };
  let diag1Check = rowCheck([| board[0][0], board[1][1], board[2][2] |]);
  let diag2Check = rowCheck([| board[2][0], board[1][1], board[0][2] |]);
  if (diag1Check != Empty) {
    allChecks := diag1Check;
  } else if (diag2Check != Empty) {
    allChecks := diag2Check;
  };
  allChecks
};
  
  /* Your familiar handleClick from ReactJS. This mandatorily takes the payload,
   then the `self` record, which contains state (none here), `handle`, `reduce`
   and other utilities */
let handleClick = (_event, _self) => Js.log("clicked!");

let cell = status =>
  ReasonReact.stringToElement(
    switch status {
    | Cross => "X"
    | Circle => "O"
    | Empty => " "
  }
  );
  
  let component = ReasonReact.reducerComponent("Page");
  /* `make` is the function that mandatorily takes `children` (if you want to use
  `JSX). `message` is a named argument, which simulates ReactJS props. Usage:
  
  `<Page message="hello" />`
  
  Which desugars to
  
  `ReasonReact.element(Page.make(~message="hello", [||]))` */
  let make = _children => {
    ...component,
    initialState: () => {
    board: [|
    [|Empty, Empty, Empty|],
    [|Empty, Empty, Empty|],
      [|Empty, Empty, Empty|]
      |],
    player: Cross,
    winner: Empty
  },
  reducer: (action, state) =>
  switch action {
  | Click(row, column) =>
  if (state.board[row][column] == Empty) {
        let board =
          Array.mapi(
            (rowIdx, rowArr) =>
              Array.mapi(
                (columnIdx, el) =>
                  if (rowIdx == row && columnIdx == column) {
                    state.player;
                  } else {
                    el;
                  },
                rowArr
              ),
            state.board
          );
        let winner = winCheck(board);
        if (winner^ != Empty) {
          ReasonReact.Update({
            board: [|
            [|Empty, Empty, Empty|],
            [|Empty, Empty, Empty|],
            [|Empty, Empty, Empty|]|], 
            winner: winner^,
            player: Cross
          })
        } else {
          ReasonReact.Update({board, player: changePlayer(state.player), winner: Empty});
        }
      } else {
        ReasonReact.NoUpdate;
      }
    },
  render: self => {
    Js.log(self.state.winner);
    <div style=(
      ReactDOMRe.Style.make(~display="flex", ~flexDirection="column", ())
    )>
      (
        ReasonReact.arrayToElement(
          Array.mapi(
            (rowIdx, row) =>
            <div style=(
              ReactDOMRe.Style.make(~display="flex", ~flexDirection="row", ())
            )>
            {
              ReasonReact.arrayToElement(
                Array.mapi(
                  (columnIdx, el) =>
                    <div
                    style=(
                      ReactDOMRe.Style.make(~width="100px", ~height="100px", ~border="1px solid black", ())
                    )
                    onClick=(_b => self.send(Click(rowIdx, columnIdx)))>
                      (cell(el))
                    </div>,
                  row
                )
              )
            }
            </div>,
            self.state.board
          )
        )
      )
      {
        self.state.winner != Empty ? cell(self.state.winner) : <div />
      }
    </div>
  }
};