module type Spec = {type t; let name: string; let get: string => Js.Promise.t t;};

module Make (T: Spec) => {
  module Loader = Map.Make String;
  let loader = ref Loader.empty;
  let load key =>
    try (!loader |> Loader.find key) {
    | _ =>
      let promise = T.get key;
      loader := !loader |> Loader.add key promise;
      promise
    };
  let loadMany = List.map load;
  let reload key => {
    let promise = T.get key;
    loader := !loader |> Loader.add key promise;
    promise
  };
  let clear key => {
    loader := !loader |> Loader.remove key;
    Js.Promise.resolve ()
  };
  let prime key value => loader := !loader |> Loader.add key value;
  let clearAll () => loader := Loader.empty;
};
