module Make
    (T : sig
       type -'a connection
       type execute_response

       module IO : sig
         type 'a future
       end
     end) :
  sig
    type t = private
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

    val make_reversible :
      id:string ->
      apply:
        ([ `WR ] T.connection ->
         T.execute_response T.IO.future) ->
      revert:
        ([ `WR ] T.connection ->
         T.execute_response T.IO.future) ->
      (t, [ `Invalid_id of Migration_id.error ]) result

    val make_irreversible :
      id:string ->
      apply:
        ([ `WR ] T.connection ->
         T.execute_response T.IO.future) ->
      (t, [ `Invalid_id of Migration_id.error ]) result

    val make_generated_reversible :
      id:string ->
      apply:
        ([ `WR ] T.connection ->
         T.execute_response T.IO.future) ->
      revert:
        ([ `WR ] T.connection ->
         T.execute_response T.IO.future) ->
      t

    val make_generated_irreversible :
      id:string ->
      apply:
        ([ `WR ] T.connection ->
         T.execute_response T.IO.future) ->
      t
  end
