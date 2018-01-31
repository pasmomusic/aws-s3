(** Utilites *)

(**/**)
module R = Result
(**/**)

open Core
open Lwt
open Cohttp
open Cohttp_lwt

type region =
  | Ap_northeast_1
  | Ap_southeast_1
  | Ap_southeast_2
  | Eu_central_1
  | Eu_west_1
  | Sa_east_1
  | Us_east_1
  | Us_west_1
  | Us_west_2

val region_of_string : string -> region

(**/**)
val gzip_data : ?level:int -> String.t -> string
val make_request :
  ?body:String.t ->
  ?region:region ->
  ?credentials:Credentials.t ->
  headers:(string * string) list ->
  meth:Code.meth ->
  path:string ->
  query:(string * string) list ->
  unit ->
  (Response.t * Body.t) Lwt.t


module Test : sig
  val async : ('a -> 'b Lwt.t) -> 'a -> unit
  val gunzip : string -> string Or_error.t Lwt.t
  val test_gzip : 'a -> unit Lwt.t
  val unit_test : OUnit2.test
end

module Lwt_try_with_join : sig
  val do_ : (unit -> 'a Or_error.t Lwt.t) -> 'a Or_error.t Lwt.t 
end
