(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Hannes Mehnert <hannes@mehnert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt

let debug = ref false
let debug_print = ref Printf.eprintf
let () =
  try
    ignore(Sys.getenv "CONDUIT_DEBUG");
    debug := true
  with Not_found -> ()

let ignore_exn exn =
  if !debug then !debug_print "safe: ignoring exception %s" (Printexc.to_string exn);
  Lwt.return_unit
let safe f = Lwt.catch f ignore_exn

let safe_close t =
  safe (fun () -> Lwt_io.close t)

let safe_close_unix s =
  safe (fun () -> Lwt_unix.close s)

let with_socket sockaddr f =
  let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Lwt.catch (fun () -> f fd) (fun e ->
      safe_close_unix fd >>= fun () ->
      fail e
    )
