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

(* internal module *)

val debug : bool ref
val debug_print : ((string -> unit, out_channel, unit) format -> string -> unit) ref
val safe_close : 'a Lwt_io.channel -> unit Lwt.t
val safe_close_unix : Lwt_unix.file_descr -> unit Lwt.t
val with_socket : Unix.sockaddr -> (Lwt_unix.file_descr -> 'a Lwt.t) -> 'a Lwt.t
