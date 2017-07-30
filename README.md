# reDataLoader

> A small caching module for front-end requests

## Example

```reason
module MyLoaderSpec = {
  type t = int;
  let name = "MyLoader";
  let get key => Js.Promise.resolve (int_of_string key);
};

module MyLoader = DataLoader.Make MyLoaderSpec;

let promise = MyLoader.load "1";
let promises = MyLoader.loadMany ["1", "2", "3"];
let promise = MyLoader.reload "1";
let promise = MyLoader.clear "1";
```
