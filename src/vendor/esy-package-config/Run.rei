/**
 * A computation which might result in an error.
 */

type t('v) = result('v, error)
and error = (string, context)
and context = list(contextItem)
and contextItem =
  | Line(string)
  | LogOutput((string, string));

/**
 * Failied computation with an error specified by a message.
 */

let return: 'v => t('v);

/**
 * Failied computation with an error specified by a message.
 */

let error: string => t('v);

/**
 * Same with [error] but defined with a formatted string.
 */

let errorf: format4('a, Format.formatter, unit, t('v)) => 'a;

/**
 * Wrap computation with a context which will be reported in case of error
 */

let context: (string, t('v)) => t('v);

let runExn: (~err: string=?, t('a)) => 'a;

let ofStringError: result('a, string) => t('a);

let ofBosError:
  result(
    'a,
    [< | `Msg(string) | `CommandError(Bos.Cmd.t, Bos.OS.Cmd.status)],
  ) =>
  t('a);

