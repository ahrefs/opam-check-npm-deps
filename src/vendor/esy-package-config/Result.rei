type t('v, 'err) = result('v, 'err) = | Ok('v) | Error('err);

module List: {
  let foldLeft:
    (~f: ('a, 'b) => t('a, 'err), ~init: 'a, list('b)) => t('a, 'err);
};

module Syntax: {
  let return: 'v => t('v, _);
  let error: 'err => t(_, 'err);
  let ( let* ): (t('a, 'err), 'a => t('b, 'err)) => t('b, 'err);
};
