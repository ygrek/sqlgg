Per-column `-- [sqlgg]` annotations are attached to a column while its statement is
parsed; they are not part of what `Tables` stores. Materializing a schema reprints it
from `Tables`, so the annotations do not survive the round trip and code generated
from a materialized schema silently loses the codecs.

This test pins that boundary. Lifting it takes two things: `Schema_diff.dump` has to
reprint the annotations, and the replay path behind `-base` has to activate them in
the first place (today only `Compile.statement` calls `Statements.activate`, and the
block pipeline in `Main` never does).

Read straight from the annotated DDL, `cid` goes through its codec:

  $ sqlgg -no-header -dialect mysql -gen none initial.sql -gen caml -name Q q.sql
  module Q (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let by_cid db ~cid callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_int64 p (Codecs.Cid.set_param cid);
        T.finish_params p
      in
      T.select db (Sqlgg_traits.Query.make ~filename:"q.sql" ~sql:("SELECT id FROM accounts WHERE cid = ?") ~name:"by_cid" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback
  
    module Fold = struct
      let by_cid db ~cid callback acc =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_int64 p (Codecs.Cid.set_param cid);
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~filename:"q.sql" ~sql:("SELECT id FROM accounts WHERE cid = ?") ~name:"by_cid" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let by_cid db ~cid callback =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_int64 p (Codecs.Cid.set_param cid);
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~filename:"q.sql" ~sql:("SELECT id FROM accounts WHERE cid = ?") ~name:"by_cid" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Q *)

Materializing drops the annotation:

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql > materialized.sql
  $ cat materialized.sql
  CREATE TABLE `plain` (`id` INT NOT NULL);
  CREATE TABLE `accounts` (`id` INT NOT NULL, `cid` BIGINT NOT NULL);

So the same query built against the materialized schema binds `cid` as a plain Int:

  $ sqlgg -no-header -dialect mysql -gen none materialized.sql -gen caml -name Q q.sql
  module Q (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let by_cid db ~cid callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p cid;
        T.finish_params p
      in
      T.select db (Sqlgg_traits.Query.make ~filename:"q.sql" ~sql:("SELECT id FROM accounts WHERE cid = ?") ~name:"by_cid" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback
  
    module Fold = struct
      let by_cid db ~cid callback acc =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p cid;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~filename:"q.sql" ~sql:("SELECT id FROM accounts WHERE cid = ?") ~name:"by_cid" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let by_cid db ~cid callback =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p cid;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~filename:"q.sql" ~sql:("SELECT id FROM accounts WHERE cid = ?") ~name:"by_cid" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Q *)
