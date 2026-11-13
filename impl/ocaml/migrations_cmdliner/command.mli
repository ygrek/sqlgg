

module Make
    (T : sig
       type -'a connection

       module IO : sig
         type 'a future
       end
     end)
    (_ : Sqlgg_migrations.Runner.S
           with type 'a connection := 'a T.connection
            and type 'a future := 'a T.IO.future)
    (_ : sig
       
       type options

       val options : options Cmdliner.Term.t

       val with_connection :
         options ->
         ([ `RO | `WR ] T.connection -> 'a T.IO.future) ->
         'a
     end) : sig
  
  val command : Cmdliner.Cmd.Exit.code Cmdliner.Cmd.t
end
