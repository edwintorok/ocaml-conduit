(*
 * Copyright (c) 2016 Skylable Ltd. <info-copyright@skylable.com>
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
*)

open Lwt

let port = 9192
let config = `Crt_file_path "server.pem", `Key_file_path "server.key", `No_password, `Port port

let rec repeat n f =
  if n = 0 then return_unit
  else f () >>= fun () -> repeat (n-1) f

let perform () =
  let stop, do_stop = Lwt.wait () in
  Conduit_lwt_unix.init ~src:"127.0.0.1" () >>= fun ctx ->
  let _ =
    Conduit_lwt_unix.serve ~stop ~ctx ~mode:(`TLS config) (fun  _ ic oc ->
        Lwt_io.read ic >>= fun _ -> Lwt_io.write oc "foo"  >>= fun () -> Lwt_io.flush oc)
  in
  let sa = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let wait, wake = Lwt.task () in
  let client_test_wait wait =
    (* connect using low-level operations to check what happens if client closes connection
       without calling ssl_shutdown (e.g. TCP connection is lost) *)
    let s = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
    Lwt_unix.with_timeout 5. (fun () ->
        Lwt.finalize (fun () ->
            Lwt_unix.connect s sa >>= fun () ->
            Lwt_ssl.ssl_connect s ctx >>= fun ss ->
            wait)
          (fun () -> Lwt_unix.close s))
  in
  let client_test _ = client_test_wait return_unit in

  Conduit_lwt_unix.set_max_active 5;
  (* when clients = max_active no more clients are allowed and some get errors *)
  let t = Array.init 5 (fun _ -> client_test_wait wait) |> Array.to_list |> Lwt.join in
  Lwt.catch (fun () ->
      print_endline "Waiting for error";
      Array.init 10 client_test |> Array.to_list |> Lwt.pick >>= fun () ->
      prerr_endline "Expected errors, but got none";
      exit 2
    )
    (fun _exn ->
       print_endline "Waking up connections";
       Lwt.wakeup wake ();
       Lwt.catch (fun () -> t) (fun _ -> return_unit) >>= fun () ->
       print_endline "Opening more connections";
       (* clients can connect again, handled in batches of 5 *)
       Array.init 10 client_test |> Array.to_list |> Lwt.join
    ) >>= fun () ->
  print_endline "Running single connection leak test";
  repeat 1024 client_test >>= fun () ->
  Lwt.wakeup do_stop ();
  return_unit

let () =
  Lwt.async_exception_hook := ignore;
  Lwt_main.run (Lwt_unix.handle_unix_error perform ());
  print_endline "OK"
