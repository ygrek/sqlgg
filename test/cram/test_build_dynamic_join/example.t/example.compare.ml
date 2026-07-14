module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module User_info = struct
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
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("u.name");
          count = 0;
          deps = [];
        }
      let email : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("u.email");
          count = 0;
          deps = [];
        }
      let created_at : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Datetime row idx, idx + 1));
          column = ("u.created_at");
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
      let avatar_url : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("p.avatar_url");
          count = 0;
          deps = [Profiles];
        }
      let location : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("p.location");
          count = 0;
          deps = [Profiles];
        }
      let website : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("p.website");
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
      let paid_until : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Datetime_nullable row idx, idx + 1));
          column = ("b.paid_until");
          count = 0;
          deps = [Billing];
        }
      let balance : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("b.balance");
          count = 0;
          deps = [Billing];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
      method email = Cols.email
      method created_at = Cols.created_at
      method bio = Cols.bio
      method avatar_url = Cols.avatar_url
      method location = Cols.location
      method website = Cols.website
      method plan = Cols.plan
      method paid_until = Cols.paid_until
      method balance = Cols.balance
    end

    let select db (col : _ t) ~org callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p org;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ "\n\
FROM users u" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles p ON p.user_id = u.id" else "") ^ (if List.mem Billing col.deps then " LEFT JOIN billing  b ON b.user_id = u.id" else "") ^ "\n\
WHERE u.org_id = ? AND u.deleted = FALSE")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~org callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p org;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ "\n\
FROM users u" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles p ON p.user_id = u.id" else "") ^ (if List.mem Billing col.deps then " LEFT JOIN billing  b ON b.user_id = u.id" else "") ^ "\n\
WHERE u.org_id = ? AND u.deleted = FALSE")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~org callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p org;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ "\n\
FROM users u" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles p ON p.user_id = u.id" else "") ^ (if List.mem Billing col.deps then " LEFT JOIN billing  b ON b.user_id = u.id" else "") ^ "\n\
WHERE u.org_id = ? AND u.deleted = FALSE")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_users db  =
    T.execute db ("CREATE TABLE users (\n\
  id INT PRIMARY KEY,\n\
  org_id INT NOT NULL,\n\
  name TEXT NOT NULL,\n\
  email TEXT NOT NULL,\n\
  created_at TIMESTAMP NOT NULL,\n\
  deleted BOOLEAN NOT NULL\n\
)") T.no_params

  let create_profiles db  =
    T.execute db ("CREATE TABLE profiles (\n\
  user_id INT PRIMARY KEY,\n\
  bio TEXT,\n\
  avatar_url TEXT,\n\
  location TEXT,\n\
  website TEXT\n\
)") T.no_params

  let create_billing db  =
    T.execute db ("CREATE TABLE billing (\n\
  user_id INT PRIMARY KEY,\n\
  plan TEXT NOT NULL,\n\
  paid_until DATETIME,\n\
  balance INT NOT NULL\n\
)") T.no_params

  module Fold = struct
  end (* module Fold *)
  
  module List = struct
  end (* module List *)
end (* module Sqlgg *)
