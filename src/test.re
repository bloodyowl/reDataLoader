module TestLoaderSpec = {
  type t = int;
  let name = "Test";
  let get key => Js.Promise.resolve (int_of_string key);
};

module TestLoader = DataLoader.Make TestLoaderSpec;

let test name func => {
  Js.log name;
  func ()
};

let expect exp => exp ? Js.log "  PASSED" : Js.log "  FAILED";

let rec sequence listOfPromises =>
  switch listOfPromises {
  | [] => Js.Promise.resolve ()
  | [a, ...b] => Js.Promise.then_ (fun _ => sequence b) (a ())
  };

module TestCallsLoaderSpec = {
  type t = int;
  let name = "Test";
  let calls = ref 0;
  let get key => {
    calls := !calls + 1;
    Js.Promise.resolve (int_of_string key)
  };
};

module TestCallsLoader = DataLoader.Make TestCallsLoaderSpec;

sequence [
  fun () =>
    test
      "shouldLoad"
      (
        fun () =>
          TestLoader.load "13" |>
          Js.Promise.then_ (
            fun result => {
              expect (result == 13);
              Js.Promise.resolve ()
            }
          )
      ),
  fun () =>
    test
      "shouldLoad 2"
      (
        fun () =>
          sequence [
            fun () =>
              TestCallsLoader.load "13" |>
              Js.Promise.then_ (
                fun result => {
                  expect (result == 13);
                  Js.Promise.resolve ()
                }
              ),
            fun () =>
              TestCallsLoader.load "13" |>
              Js.Promise.then_ (
                fun result => {
                  expect (result == 13);
                  Js.Promise.resolve ()
                }
              ),
            fun () => Js.Promise.resolve (expect (!TestCallsLoaderSpec.calls == 1))
          ]
      )
];
