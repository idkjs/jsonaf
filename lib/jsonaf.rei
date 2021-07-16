/*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*/

type t = [
  | `Null
  | `False
  | `True
  | `String(string)
  | `Number(string)
  | `Object(list((string, t)))
  | `Array(list(t))
];

type bigstring = Angstrom.bigstring constraint bigstring = Faraday.bigstring;

let parse: Angstrom.t(t);
let serialize: (t, Faraday.t) => unit;

let of_string:
  (string, ~consume: Angstrom.Consume.t) =>
  result(
    [>
      | `Array(list('a))
      | `False
      | `Null
      | `Number(string)
      | `Object(list((string, 'a)))
      | `String(string)
      | `True
    ] as 'a,
    string,
  );
let of_bigstring:
  (bigstring, ~consume: Angstrom.Consume.t) =>
  result(
    [>
      | `Array(list('a))
      | `False
      | `Null
      | `Number(string)
      | `Object(list((string, 'a)))
      | `String(string)
      | `True
    ] as 'a,
    string,
  );

let to_string: t => string;
let to_bigstring: t => bigstring;

module With_number: {
  type t('number) = [
    | `Null
    | `False
    | `True
    | `String(string)
    | `Number('number)
    | `Object(list((string, t('number))))
    | `Array(list(t('number)))
  ];

  let parse:
    (string => Result.result('number, string)) => Angstrom.t(t('number));

  let serialize:
    ((Faraday.t, 'number) => unit, t('number), Faraday.t) => unit;

  let of_string:
    (string => result('a, string), string, ~consume: Angstrom.Consume.t) =>
    result(
      [>
        | `Array(list('b))
        | `False
        | `Null
        | `Number('a)
        | `Object(list((string, 'b)))
        | `String(string)
        | `True
      ] as 'b,
      string,
    );

  let of_bigstring:
    (string => result('a, string), bigstring, ~consume: Angstrom.Consume.t) =>
    result(
      [>
        | `Array(list('b))
        | `False
        | `Null
        | `Number('a)
        | `Object(list((string, 'b)))
        | `String(string)
        | `True
      ] as 'b,
      string,
    );

  let to_string: ((Faraday.t, 'number) => unit, t('number)) => string;

  let to_bigstring: ((Faraday.t, 'number) => unit, t('number)) => bigstring;
};
