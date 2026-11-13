module Make
    (T : sig
       type -'a connection
       type execute_response

       module IO : sig
         type 'a future
       end
     end) =
struct
  type t =
    | Reversible of {
        id : Migration_id.t;
        apply :
          [ `WR ] T.connection ->
          T.execute_response T.IO.future;
        revert :
          [ `WR ] T.connection ->
          T.execute_response T.IO.future;
      }
    | Irreversible of {
        id : Migration_id.t;
        apply :
          [ `WR ] T.connection ->
          T.execute_response T.IO.future;
      }

  let make_reversible ~id ~apply ~revert :
      (t, [ `Invalid_id of Migration_id.error ]) result =
    match Migration_id.parse id with
    | Ok id -> Ok (Reversible { id; apply; revert })
    | Error error -> Error (`Invalid_id error)

  let make_irreversible ~id ~apply :
      (t, [ `Invalid_id of Migration_id.error ]) result =
    match Migration_id.parse id with
    | Ok id -> Ok (Irreversible { id; apply })
    | Error error -> Error (`Invalid_id error)

  let generated_id id =
    match Migration_id.parse id with
    | Ok id -> id
    | Error error ->
        invalid_arg
          ("sqlgg generated " ^ Migration_id.error_to_string error)

  let make_generated_reversible ~id ~apply ~revert =
    Reversible { id = generated_id id; apply; revert }

  let make_generated_irreversible ~id ~apply =
    Irreversible { id = generated_id id; apply }
end
