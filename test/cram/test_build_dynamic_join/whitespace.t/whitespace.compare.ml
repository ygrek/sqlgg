module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Ws = struct
    type brand = Profiles | Billing
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("u.id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("u.name");
          count = 0;
          deps = [];
        }
      let bio : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("p.bio");
          count = 0;
          deps = [Profiles];
        }
      let plan : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("b.plan");
          count = 0;
          deps = [Billing];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
      method bio = Cols.bio
      method plan = Cols.plan
    end

    let select db (col : _ t) ~uid callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p uid;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ "\n\
FROM users u" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles p ON p.user_id = u.id" else "") ^ (if List.mem Billing col.deps then " LEFT JOIN billing b ON b.user_id = u.id" else "") ^ "\n\
WHERE u.note = '  two  spaces  inside  ' AND u.id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~uid callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ "\n\
FROM users u" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles p ON p.user_id = u.id" else "") ^ (if List.mem Billing col.deps then " LEFT JOIN billing b ON b.user_id = u.id" else "") ^ "\n\
WHERE u.note = '  two  spaces  inside  ' AND u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~uid callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ "\n\
FROM users u" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles p ON p.user_id = u.id" else "") ^ (if List.mem Billing col.deps then " LEFT JOIN billing b ON b.user_id = u.id" else "") ^ "\n\
WHERE u.note = '  two  spaces  inside  ' AND u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_users db  =
    T.execute db ("CREATE TABLE users (id INT PRIMARY KEY, name TEXT, note TEXT)") T.no_params

  let create_profiles db  =
    T.execute db ("CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT)") T.no_params

  let create_billing db  =
    T.execute db ("CREATE TABLE billing (user_id INT PRIMARY KEY, plan TEXT)") T.no_params

  module Fold = struct
  end (* module Fold *)
  
  module List = struct
  end (* module List *)
end (* module Sqlgg *)
