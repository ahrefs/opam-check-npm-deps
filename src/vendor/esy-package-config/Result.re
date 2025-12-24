type t('v, 'err) = result('v, 'err) = | Ok('v) | Error('err);

module Syntax = {
  let return = v => Ok(v);
  let error = err => Error(err);
  let ( let* ) = (v, f) =>
    switch (v) {
    | Ok(v) => f(v)
    | Error(e) => Error(e)
    };
};

module List = {
  let foldLeft = (~f: ('a, 'b) => result('a, 'e), ~init: 'a, xs: list('b)) => {
    let rec fold = (acc, xs) =>
      switch (acc, xs) {
      | (Error(err), _) => Error(err)
      | (Ok(acc), []) => Ok(acc)
      | (Ok(acc), [x, ...xs]) => fold(f(acc, x), xs)
      };
    fold(Ok(init), xs);
  };
};
