module R = Result
open Core
open Cohttp
open Protocol_conv_json
open Lwt.Infix
open Lwt
open Cohttp_lwt

type time = Time.t
let time_of_json t =
  Json.to_string t |> Time.of_string

type t = {
  aws_access_key: string [@key "AccessKeyId"];
  aws_secret_key: string [@key "SecretAccessKey"];
  aws_token: string option [@key "Token"];
  expiration: time option [@key "Expiration"];
} [@@deriving of_protocol ~driver:(module Json)]

let make_credentials ~access_key ~secret_key ?token ?expiration () =
  { aws_access_key=access_key; aws_secret_key=secret_key; aws_token=token; expiration }

module Iam = struct
  let instance_data_host = "instance-data.ec2.internal"
  let get_role () =
    let inner () =
      let uri = Uri.make ~host:instance_data_host ~scheme:"http" ~path:"/latest/meta-data/iam/security-credentials/" () in
      Cohttp_lwt_unix.Client.call `GET uri >>= fun (response, body) ->
      match Cohttp.Response.status response with
      | #Code.success_status ->
          Cohttp_lwt.Body.to_string body >>= fun body ->
          return (Ok body)
      | _ ->
          Body.to_string body >>= fun body ->
          return (Or_error.errorf "Failed to get role from %s. Response was: %s" (Uri.to_string uri) body)
    in
    Lwt.catch inner (fun exn -> Lwt.return @@ Or_error.of_exn exn)

  let get_credentials role =
    let inner () =
      let path = sprintf "/latest/meta-data/iam/security-credentials/%s" role in
      let uri = Uri.make ~host:instance_data_host ~scheme:"http" ~path () in
      Cohttp_lwt_unix.Client.call `GET uri >>= fun (response, body) ->
      match Cohttp.Response.status response with
      | #Code.success_status -> begin
          Body.to_string body >>= fun body ->
          let json = Yojson.Safe.from_string body in
          t_of_json json |> Or_error.return |> return
        end
      | _ ->
          Body.to_string body >>= fun body ->
          return (Or_error.errorf "Failed to get credentials from %s. Response was: %s" (Uri.to_string uri) body)
    in
    Lwt.catch inner (fun exn -> Lwt.return @@ Or_error.of_exn exn)
end

module Local = struct
  let get_credentials ?(profile="default") () =
    let home = Sys.getenv "HOME" |> Option.value ~default:"." in
    let creds_file = sprintf "%s/.aws/credentials" home in
    let ini = new Inifiles.inifile creds_file in
    let access_key = ini#getval profile "aws_access_key_id" in
    let secret_key = ini#getval profile "aws_secret_access_key" in
    let inner () = Result.Ok (make_credentials ~access_key ~secret_key ()) |> return in
    Lwt.catch inner (fun exn -> Lwt.return @@ Or_error.of_exn exn)
end

module Helper = struct
  let get_credentials ?profile (): t Core.Or_error.t Lwt.t =
    match profile with
    | Some profile -> Local.get_credentials ~profile ()
    | None -> begin
        Local.get_credentials ~profile:"default" () >>= function
        | Result.Ok c -> Lwt_result.return c
        | Error _ ->
            Iam.get_role () >>= function
              | Result.Ok role -> Iam.get_credentials role
              | Error x -> Lwt.return @@ Error x
      end
end
