// Generated by BUCKLESCRIPT VERSION 2.2.2, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

function changePlayer(curr) {
  if (curr) {
    return /* Cross */0;
  } else {
    return /* Circle */1;
  }
}

function rowCheck(row) {
  if (Caml_array.caml_array_get(row, 0) !== /* Empty */2 && Caml_array.caml_array_get(row, 0) === Caml_array.caml_array_get(row, 1) && Caml_array.caml_array_get(row, 0) === Caml_array.caml_array_get(row, 2)) {
    return Caml_array.caml_array_get(row, 0);
  } else {
    return /* Empty */2;
  }
}

function winCheck(board) {
  var allChecks = [$$Array.fold_left((function (acc, row) {
            var currCheck = rowCheck(row);
            if (currCheck !== /* Empty */2) {
              return currCheck;
            } else {
              return acc;
            }
          }), /* Empty */2, board)];
  for(var i = 0; i <= 2; ++i){
    var column = /* int array */[
      Caml_array.caml_array_get(Caml_array.caml_array_get(board, 0), i),
      Caml_array.caml_array_get(Caml_array.caml_array_get(board, 1), i),
      Caml_array.caml_array_get(Caml_array.caml_array_get(board, 2), i)
    ];
    var currCheck = rowCheck(column);
    if (currCheck !== /* Empty */2) {
      allChecks[0] = currCheck;
    }
    
  }
  var diag1Check = rowCheck(/* int array */[
        Caml_array.caml_array_get(Caml_array.caml_array_get(board, 0), 0),
        Caml_array.caml_array_get(Caml_array.caml_array_get(board, 1), 1),
        Caml_array.caml_array_get(Caml_array.caml_array_get(board, 2), 2)
      ]);
  var diag2Check = rowCheck(/* int array */[
        Caml_array.caml_array_get(Caml_array.caml_array_get(board, 2), 0),
        Caml_array.caml_array_get(Caml_array.caml_array_get(board, 1), 1),
        Caml_array.caml_array_get(Caml_array.caml_array_get(board, 0), 2)
      ]);
  if (diag1Check !== /* Empty */2) {
    allChecks[0] = diag1Check;
  } else if (diag2Check !== /* Empty */2) {
    allChecks[0] = diag2Check;
  }
  return allChecks;
}

function handleClick(_, _$1) {
  console.log("clicked!");
  return /* () */0;
}

function cell(status) {
  switch (status) {
    case 0 : 
        return "X";
    case 1 : 
        return "O";
    case 2 : 
        return " ";
    
  }
}

var component = ReasonReact.reducerComponent("Page");

function make() {
  var newrecord = component.slice();
  newrecord[/* render */9] = (function (self) {
      console.log(self[/* state */2][/* winner */2]);
      var match = +(self[/* state */2][/* winner */2] !== /* Empty */2);
      return React.createElement("div", {
                  style: {
                    display: "flex",
                    flexDirection: "column"
                  }
                }, $$Array.mapi((function (rowIdx, row) {
                        return React.createElement("div", {
                                    style: {
                                      display: "flex",
                                      flexDirection: "row"
                                    }
                                  }, $$Array.mapi((function (columnIdx, el) {
                                          return React.createElement("div", {
                                                      style: {
                                                        border: "1px solid black",
                                                        height: "100px",
                                                        width: "100px"
                                                      },
                                                      onClick: (function () {
                                                          return Curry._1(self[/* send */4], /* Click */[
                                                                      rowIdx,
                                                                      columnIdx
                                                                    ]);
                                                        })
                                                    }, cell(el));
                                        }), row));
                      }), self[/* state */2][/* board */0]), match !== 0 ? cell(self[/* state */2][/* winner */2]) : React.createElement("div", undefined));
    });
  newrecord[/* initialState */10] = (function () {
      return /* record */[
              /* board : array */[
                /* int array */[
                  /* Empty */2,
                  /* Empty */2,
                  /* Empty */2
                ],
                /* int array */[
                  /* Empty */2,
                  /* Empty */2,
                  /* Empty */2
                ],
                /* int array */[
                  /* Empty */2,
                  /* Empty */2,
                  /* Empty */2
                ]
              ],
              /* player : Cross */0,
              /* winner : Empty */2
            ];
    });
  newrecord[/* reducer */12] = (function (action, state) {
      var column = action[1];
      var row = action[0];
      if (Caml_array.caml_array_get(Caml_array.caml_array_get(state[/* board */0], row), column) === /* Empty */2) {
        var board = $$Array.mapi((function (rowIdx, rowArr) {
                return $$Array.mapi((function (columnIdx, el) {
                              if (rowIdx === row && columnIdx === column) {
                                return state[/* player */1];
                              } else {
                                return el;
                              }
                            }), rowArr);
              }), state[/* board */0]);
        var winner = winCheck(board);
        if (winner[0] !== /* Empty */2) {
          return /* Update */Block.__(0, [/* record */[
                      /* board : array */[
                        /* int array */[
                          /* Empty */2,
                          /* Empty */2,
                          /* Empty */2
                        ],
                        /* int array */[
                          /* Empty */2,
                          /* Empty */2,
                          /* Empty */2
                        ],
                        /* int array */[
                          /* Empty */2,
                          /* Empty */2,
                          /* Empty */2
                        ]
                      ],
                      /* player : Cross */0,
                      /* winner */winner[0]
                    ]]);
        } else {
          return /* Update */Block.__(0, [/* record */[
                      /* board */board,
                      /* player */changePlayer(state[/* player */1]),
                      /* winner : Empty */2
                    ]]);
        }
      } else {
        return /* NoUpdate */0;
      }
    });
  return newrecord;
}

exports.changePlayer = changePlayer;
exports.rowCheck = rowCheck;
exports.winCheck = winCheck;
exports.handleClick = handleClick;
exports.cell = cell;
exports.component = component;
exports.make = make;
/* component Not a pure module */
