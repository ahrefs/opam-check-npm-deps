/**
 * An async computation which might result in an error.
 */

type t('a) = Lwt.t(Run.t('a));

/**
 * Computation which results in a value.
 */

let return: 'a => t('a);

/**
 * Computation which results in an error.
 */

let error: string => t('a);

/**
 * Run computation and throw an exception in case of a failure.
 *
 * Optional [err] will be used as error message.
 */

let runExn: (~err: string=?, t('a)) => 'a;

/**
 * Convert [Run.t] into [t].
 */

let ofRun: Run.t('a) => t('a);

/**
 * Convenience module which is designed to be openned locally with the
 * code which heavily relies on RunAsync.t.
 *
 * This also brings monadic let operators and Let_syntax module into scope
 * and thus compatible with ppx_let.
 *
 * Example
 *
 *    let open RunAsync.Syntax in
 *    let%bind v = fetchNumber ... in
 *    if v > 10
 *    then return (v + 1)
 *    else error "Less than 10"
 *
 */

module Syntax: {
  let return: 'a => t('a);

  let error: string => t('a);
  let errorf: format4('a, Format.formatter, unit, t('v)) => 'a;
  let ( let* ): (t('a), 'a => t('b)) => t('b);
};

