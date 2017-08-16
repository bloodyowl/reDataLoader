module TestLoaderSpec = {
  type t = int;
  type context = string;
  let name = "Test";
  let get context::_context key => Js.Promise.resolve (int_of_string key);
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
  type context = string;
  let name = "Test";
  let calls = ref 0;
  let get context::_context key => {
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
          TestLoader.load context::"A" "13" |>
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
              TestCallsLoader.load context::"A" "13" |>
              Js.Promise.then_ (
                fun result => {
                  expect (result == 13);
                  Js.Promise.resolve ()
                }
              ),
            fun () =>
              TestCallsLoader.load context::"A" "13" |>
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
