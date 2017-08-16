module type Spec = {
  type t;
  type context;
  let name: string;
  let get: context::context => string => Js.Promise.t t;
};

module Make (T: Spec) => {
  module Loader = Map.Make String;
  let loader = ref Loader.empty;
  let load ::context key =>
    try (!loader |> Loader.find key) {
    | _ =>
      let promise = T.get ::context key;
      loader := !loader |> Loader.add key promise;
      promise
    };
  let loadMany ::context keys => {
    let load = load ::context;
    List.map load keys
  };
  let reload ::context key => {
    let promise = T.get ::context key;
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
