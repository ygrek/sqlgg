module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module User_info_col = struct
    type source = Billing | Profiles

    type 'a projection = {
      set: T.params -> unit;
      read: T.row -> int -> 'a * int;
      column: string;
      count: int;
    }

    type 'a t = {
      projection: 'a projection;
      deps: source list;
    }

    let pure x = {
      projection = {
        set = (fun _p -> ());
        read = (fun _row idx -> (x, idx));
        column = "";
        count = 0;
      };
      deps = [];
    }

    let apply f a = {
      projection = {
        set = (fun p -> f.projection.set p; a.projection.set p);
        read = (fun row idx ->
          let (vf, i1) = f.projection.read row idx in
          let (va, i2) = a.projection.read row i1 in
          (vf va, i2));
        column = (match f.projection.column, a.projection.column with
          | "", c | c, "" -> c
          | c1, c2 -> c1 ^ ", " ^ c2);
        count = f.projection.count + a.projection.count;
      };
      deps = f.deps @ List.filter (fun d -> not (List.mem d f.deps)) a.deps;
    }

    let map f a = apply (pure f) a

    let (let+) t f = map f t
    let (and+) a b = apply (map (fun a b -> (a, b)) a) b

    let lift deps projection = { projection; deps }
    let id =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
        column = ("u.id");
        count = 0;
      }
    let name =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
        column = ("u.name");
        count = 0;
      }
    let email =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
        column = ("u.email");
        count = 0;
      }
    let created_at =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Datetime row idx, idx + 1));
        column = ("u.created_at");
        count = 0;
      }
    let bio =
      lift [Profiles] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("p.bio");
        count = 0;
      }
    let avatar_url =
      lift [Profiles] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("p.avatar_url");
        count = 0;
      }
    let location =
      lift [Profiles] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("p.location");
        count = 0;
      }
    let website =
      lift [Profiles] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("p.website");
        count = 0;
      }
    let plan =
      lift [Billing] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("b.plan");
        count = 0;
      }
    let paid_until =
      lift [Billing] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Datetime_nullable row idx, idx + 1));
        column = ("b.paid_until");
        count = 0;
      }
    let balance =
      lift [Billing] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
        column = ("b.balance");
        count = 0;
      }

    let select db (col : _ t) ~org callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.projection.count) in
        col.projection.set p;
        T.set_param_Int p org;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.projection.column ^ "\n\
FROM users u" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles p ON p.user_id = u.id" else "") ^ (if List.mem Billing col.deps then " LEFT JOIN billing  b ON b.user_id = u.id" else "") ^ "\n\
WHERE u.org_id = ? AND u.deleted = FALSE")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~org callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.projection.count) in
          col.projection.set p;
          T.set_param_Int p org;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ "\n\
FROM users u" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles p ON p.user_id = u.id" else "") ^ (if List.mem Billing col.deps then " LEFT JOIN billing  b ON b.user_id = u.id" else "") ^ "\n\
WHERE u.org_id = ? AND u.deleted = FALSE")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~org callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.projection.count) in
          col.projection.set p;
          T.set_param_Int p org;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ "\n\
FROM users u" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles p ON p.user_id = u.id" else "") ^ (if List.mem Billing col.deps then " LEFT JOIN billing  b ON b.user_id = u.id" else "") ^ "\n\
WHERE u.org_id = ? AND u.deleted = FALSE")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
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
