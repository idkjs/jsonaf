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

module List = ListLabels;

type bigstring = Angstrom.bigstring constraint bigstring = Faraday.bigstring;

module With_number = {
  type t('number) = [
    | `Null
    | `False
    | `True
    | `String(string)
    | `Number('number)
    | `Object(list((string, t('number))))
    | `Array(list(t('number)))
  ];

  module Parser = {
    open Angstrom;

    let ws =
      skip_while(
        fun
        | ' '
        | '\n'
        | '\r'
        | '\t' => true
        | _ => false,
      );

    let lchar = c => ws *> char(c);

    let rsb = lchar(']');
    let rcb = lchar('}');
    let (ns, vs) = (lchar(':'), lchar(','));
    let quo = lchar('"');

    let _false = string("false") *> return(`False);
    let _true = string("true") *> return(`True);
    let _null = string("null") *> return(`Null);

    let num = number =>
      take_while1(
        fun
        | ' '
        | '\n'
        | '\r'
        | '\t'
        | '['
        | ']'
        | '{'
        | '}'
        | ':'
        | ',' => false
        | _ => true,
      )
      >>= (
        s =>
          switch (number(s)) {
          | Ok(x) => return(`Number(x))
          | Error(msg) => fail(msg)
          }
      );

    let parse = number => {
      open Angstrom;
      let advance1 = advance(1);
      let pair = (x, y) => (x, y);
      let buf = Buffer.create(0x1000);
      let str = Json_string.parse(buf);
      fix(json => {
        let mem = lift2(pair, quo *> str <* ns, json);
        let obj = advance1 *> sep_by(vs, mem) <* rcb >>| (ms => `Object(ms));
        let arr = advance1 *> sep_by(vs, json) <* rsb >>| (vs => `Array(vs));
        let str = advance1 *> str >>| (s => `String(s));
        ws
        *> peek_char_fail
        >>= (
          fun
          | 'f' => _false
          | 'n' => _null
          | 't' => _true
          | '{' => obj
          | '[' => arr
          | '"' => str
          | _ => num(number)
        );
      })
      <?> "json";
    };
  };

  let parse = Parser.parse;

  let of_string = (number_of_string, string) =>
    Angstrom.parse_string(parse(number_of_string), string);

  let of_bigstring = (number_of_string, bigstring) =>
    Angstrom.parse_bigstring(parse(number_of_string), bigstring);

  let rec serialize = (serialize_number, t, faraday) =>
    switch (t) {
    | `Null => Faraday.write_string(faraday, "null")
    | `False => Faraday.write_string(faraday, "false")
    | `True => Faraday.write_string(faraday, "true")
    | `String(string) => Json_string.serialize(faraday, string)
    | `Number(number) => serialize_number(faraday, number)
    | `Object([]) => Faraday.write_string(faraday, "{}")
    | `Object([(k, v), ...kvs]) =>
      Faraday.write_char(faraday, '{');
      serialize_kv(faraday, serialize_number, k, v);
      List.iter(
        kvs,
        ~f=((k, v)) => {
          Faraday.write_char(faraday, ',');
          serialize_kv(faraday, serialize_number, k, v);
        },
      );
      Faraday.write_char(faraday, '}');
    | `Array([]) => Faraday.write_string(faraday, "[]")
    | `Array([t, ...ts]) =>
      Faraday.write_char(faraday, '[');
      serialize(serialize_number, t, faraday);
      List.iter(
        ts,
        ~f=t => {
          Faraday.write_char(faraday, ',');
          serialize(serialize_number, t, faraday);
        },
      );
      Faraday.write_char(faraday, ']');
    }
  and serialize_kv = (faraday, serialize_number, k, v) => {
    Faraday.write_char(faraday, '"');
    Faraday.write_string(faraday, k);
    Faraday.write_char(faraday, '"');
    Faraday.write_char(faraday, ':');
    serialize(serialize_number, v, faraday);
  };

  let to_string = (serialize_number, t) => {
    let faraday = Faraday.create(0x1000);
    serialize(serialize_number, t, faraday);
    Faraday.serialize_to_string(faraday);
  };

  let to_bigstring = (serialize_number, t) => {
    let faraday = Faraday.create(0x1000);
    serialize(serialize_number, t, faraday);
    Faraday.serialize_to_bigstring(faraday);
  };
};

type t = With_number.t(string);

let parse: Angstrom.t(t) = (With_number.parse(x => Ok(x)): Angstrom.t(t));

let serialize = (t: t, faraday) =>
  With_number.serialize((f, n) => Faraday.write_string(f, n), t, faraday);

let of_string = With_number.of_string(x => Ok(x));
let of_bigstring = With_number.of_bigstring(x => Ok(x));

let to_string = With_number.to_string((f, n) => Faraday.write_string(f, n));
let to_bigstring =
  With_number.to_bigstring((f, n) => Faraday.write_string(f, n));
