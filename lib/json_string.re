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

module Parse = {
  open Angstrom;

  type t = [
    | `Unescaped
    | `Escaped
    | `UTF8(list(char))
    | `UTF16(int, [ | `S | `U | `C(list(char))])
    | `Error(string)
    | `Done
  ];

  let to_string: [ | `Terminate | t] => string = (
    fun
    | `Unescaped => "unescaped"
    | `Escaped => "escaped"
    | `UTF8(_) => "utf-8 _"
    | `UTF16(_) => "utf-16 _ _"
    | `Error(e) => Printf.sprintf("error %S", e)
    | `Terminate => "terminate"
    | `Done => "done":
      [ | `Terminate | t] => string
  );

  let unescaped = buf =>
    fun
    | '"' => `Terminate
    | '\\' => `Escaped
    | c =>
      if (c <= '\031') {
        `Error(Printf.sprintf("unexpected character '%c'", c));
      } else {
        Buffer.add_char(buf, c);
        `Unescaped;
      };

  let escaped = buf =>
    fun
    | '"' => {
        Buffer.add_char(buf, '"');
        `Unescaped;
      }
    | '\\' => {
        Buffer.add_char(buf, '\\');
        `Unescaped;
      }
    | '/' => {
        Buffer.add_char(buf, '/');
        `Unescaped;
      }
    | 'b' => {
        Buffer.add_char(buf, '\b');
        `Unescaped;
      }
    | 'f' => {
        Buffer.add_char(buf, '\012');
        `Unescaped;
      }
    | 'n' => {
        Buffer.add_char(buf, '\n');
        `Unescaped;
      }
    | 'r' => {
        Buffer.add_char(buf, '\r');
        `Unescaped;
      }
    | 't' => {
        Buffer.add_char(buf, '\t');
        `Unescaped;
      }
    | 'u' => `UTF8([])
    | _ => `Error("invalid escape sequence");

  let hex = c =>
    switch (c) {
    | '0' .. '9' => Char.code(c) - 0x30 /* '0' */
    | 'a' .. 'f' => Char.code(c) - 87
    | 'A' .. 'F' => Char.code(c) - 55
    | _ => 255
    };

  let utf_8 = (buf, d) =>
    fun
    | [c, b, a] => {
        let a = hex(a)
        and b = hex(b)
        and c = hex(c)
        and d = hex(d);
        if (a lor b lor c lor d == 255) {
          `Error("invalid hex escape");
        } else {
          let cp = a lsl 12 lor b lsl 8 lor c lsl 4 lor d;
          if (cp >= 0xd800 && cp <= 0xdbff) {
            `UTF16((cp, `S));
          } else {
            Buffer.add_char(
              buf,
              Char.unsafe_chr(0b11100000 lor (cp lsr 12 land 0b00001111)),
            );
            Buffer.add_char(
              buf,
              Char.unsafe_chr(0b10000000 lor (cp lsr 6 land 0b00111111)),
            );
            Buffer.add_char(
              buf,
              Char.unsafe_chr(0b10000000 lor (cp land 0b00111111)),
            );
            `Unescaped;
          };
        };
      }
    | cs => `UTF8([d, ...cs]);

  let utf_16 = (buf, d, x, s) =>
    switch (s, d) {
    | (`S, '\\') => `UTF16((x, `U))
    | (`U, 'u') => `UTF16((x, `C([])))
    | (`C([c, b, a]), _) =>
      let a = hex(a)
      and b = hex(b)
      and c = hex(c)
      and d = hex(d);
      if (a lor b lor c lor d == 255) {
        `Error("invalid hex escape");
      } else {
        let y = a lsl 12 lor b lsl 8 lor c lsl 4 lor d;
        if (y >= 0xdc00 && y <= 0xdfff) {
          let hi = x - 0xd800;
          let lo = y - 0xdc00;
          let cp = 0x10000 + hi lsl 10 lor lo;
          Buffer.add_char(
            buf,
            Char.unsafe_chr(0b11110000 lor (cp lsr 18 land 0b00000111)),
          );
          Buffer.add_char(
            buf,
            Char.unsafe_chr(0b10000000 lor (cp lsr 12 land 0b00111111)),
          );
          Buffer.add_char(
            buf,
            Char.unsafe_chr(0b10000000 lor (cp lsr 6 land 0b00111111)),
          );
          Buffer.add_char(
            buf,
            Char.unsafe_chr(0b10000000 lor (cp land 0b00111111)),
          );
          `Unescaped;
        } else {
          `Error("invalid escape sequence for utf-16 low surrogate");
        };
      };
    | (`C(cs), _) => `UTF16((x, `C([d, ...cs])))
    | (_, _) => `Error("invalid escape sequence for utf-16 low surrogate")
    };

  let str = buf => {
    let state: ref(t) = (ref(`Unescaped): ref(t));
    skip_while(c =>
      switch (
        switch (state^) {
        | `Unescaped => unescaped(buf, c)
        | `Escaped => escaped(buf, c)
        | `UTF8(cs) => utf_8(buf, c, cs)
        | `UTF16(x, cs) => utf_16(buf, c, x, cs)
        | (`Error(_) | `Done) as state => state
        }
      ) {
      | `Error(_)
      | `Done => false
      | `Terminate =>
        state := `Done;
        true;
      | #t as state' =>
        state := state';
        true;
      }
    )
    >>= (
      () =>
        switch (state^) {
        | `Done =>
          let result = Buffer.contents(buf);
          Buffer.clear(buf);
          state := `Unescaped;
          return(result);
        | `Error(msg) =>
          Buffer.clear(buf);
          state := `Unescaped;
          fail(msg);
        | `Unescaped
        | `Escaped
        | `UTF8(_)
        | `UTF16(_) =>
          Buffer.clear(buf);
          state := `Unescaped;
          fail("unterminated string");
        }
    );
  };
};

let parse = buffer => Parse.str(buffer);

let to_hex_digit = i =>
  if (i < 10) {
    i + 48;
  } else {
    i + 87;
  };

let serialize = (t, s) => {
  open Faraday;
  /* TODO: Implement proper unicode verification. */
  let flush = (~off, ~len) =>
    if (len != 0) {
      write_string(t, ~off, ~len, s);
    };
  let rec go = (~off, ~len) =>
    if (String.length(s) == off + len) {
      flush(~off, ~len);
    } else {
      let i = off + len;
      switch (s.[i]) {
      | c when c <= '\031' =>
        /* non-visible characters have to be escaped */
        let c = Char.code(c);
        flush(~off, ~len);
        write_string(t, "\\u00");
        write_uint8(t, to_hex_digit(c lsr 4));
        write_uint8(t, to_hex_digit(c land 0xf));
        go(~off=i + 1, ~len=0);
      | '"' =>
        flush(~off, ~len);
        write_string(t, "\\\"");
        go(~off=i + 1, ~len=0);
      | '\b' =>
        flush(~off, ~len);
        write_string(t, "\\b");
        go(~off=i + 1, ~len=0);
      | '\012' =>
        flush(~off, ~len);
        write_string(t, "\\f");
        go(~off=i + 1, ~len=0);
      | '\n' =>
        flush(~off, ~len);
        write_string(t, "\\n");
        go(~off=i + 1, ~len=0);
      | '\r' =>
        flush(~off, ~len);
        write_string(t, "\\r");
        go(~off=i + 1, ~len=0);
      | '\t' =>
        flush(~off, ~len);
        write_string(t, "\\t");
        go(~off=i + 1, ~len=0);
      | '\\' =>
        flush(~off, ~len);
        write_string(t, "\\\\");
        go(~off=i + 1, ~len=0);
      | _ => go(~off, ~len=len + 1)
      };
    };

  write_char(t, '"');
  go(~off=0, ~len=0);
  write_char(t, '"');
};
