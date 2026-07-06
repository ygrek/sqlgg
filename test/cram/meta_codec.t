Standalone fully qualified get_column=/set_param= without module= on an ENUM column:
codec is used for reads and writes over the string representation,
no dead Enum_N module is emitted:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE orders (
  >   id INT NOT NULL,
  >   -- [sqlgg] get_column=Order_status.of_db
  >   -- [sqlgg] set_param=Order_status.to_db
  >   status ENUM('new','paid','shipped') NOT NULL
  > );
  > -- @get_status
  > SELECT status FROM orders WHERE id = @id;
  > -- @set_status
  > UPDATE orders SET status = @status WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_orders db  =
      T.execute db ("CREATE TABLE orders (\n\
    id INT NOT NULL,\n\
        status ENUM('new','paid','shipped') NOT NULL\n\
  )") T.no_params
  
    let get_status db ~id callback =
      let invoke_callback stmt =
        callback
          ~status:(Order_status.of_db (T.get_column_string stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db ("SELECT status FROM orders WHERE id = ?") set_params invoke_callback
  
    let set_status db ~status ~id =
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_string p (Order_status.to_db status);
        T.set_param_Int p id;
        T.finish_params p
      in
      T.execute db ("UPDATE orders SET status = ? WHERE id = ?") set_params
  
    module Fold = struct
      let get_status db ~id callback acc =
        let invoke_callback stmt =
          callback
            ~status:(Order_status.of_db (T.get_column_string stmt 0))
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT status FROM orders WHERE id = ?") set_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let get_status db ~id callback =
        let invoke_callback stmt =
          callback
            ~status:(Order_status.of_db (T.get_column_string stmt 0))
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT status FROM orders WHERE id = ?") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Same codec, IN-parameter: to_literal goes through set_param= codec:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE orders_in (
  >   id INT NOT NULL,
  >   -- [sqlgg] get_column=Order_status.of_db
  >   -- [sqlgg] set_param=Order_status.to_db
  >   status ENUM('new','paid','shipped') NOT NULL
  > );
  > -- @find_orders
  > SELECT id FROM orders_in WHERE status IN @statuses;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_orders_in db  =
      T.execute db ("CREATE TABLE orders_in (\n\
    id INT NOT NULL,\n\
        status ENUM('new','paid','shipped') NOT NULL\n\
  )") T.no_params
  
    let find_orders db ~statuses callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match statuses with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      T.select db ("SELECT id FROM orders_in WHERE " ^ (match statuses with [] -> "FALSE" | _ :: _ -> "status IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Text.string_to_literal (Order_status.to_db v)) statuses) ^ ")")) set_params invoke_callback
  
    module Fold = struct
      let find_orders db ~statuses callback acc =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
        in
        let set_params stmt =
          let p = T.start_params stmt (0 + (match statuses with [] -> 0 | _ :: _ -> 0)) in
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT id FROM orders_in WHERE " ^ (match statuses with [] -> "FALSE" | _ :: _ -> "status IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Text.string_to_literal (Order_status.to_db v)) statuses) ^ ")")) set_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let find_orders db ~statuses callback =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
        in
        let set_params stmt =
          let p = T.start_params stmt (0 + (match statuses with [] -> 0 | _ :: _ -> 0)) in
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT id FROM orders_in WHERE " ^ (match statuses with [] -> "FALSE" | _ :: _ -> "status IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Text.string_to_literal (Order_status.to_db v)) statuses) ^ ")")) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Partial codec on ENUM: only set_param= is customized, reads still need Enum_N,
so it is emitted and referenced by get_column, while writes go through the codec
(Enum_N is only generated in -enum-poly-variant mode):
  $ sqlgg -gen caml -no-header -dialect=mysql -enum-poly-variant - <<'EOF' 2>&1
  > CREATE TABLE orders_partial (
  >   id INT NOT NULL,
  >   -- [sqlgg] set_param=Order_status.to_db
  >   status ENUM('new','paid','shipped') NOT NULL
  > );
  > -- @get_status
  > SELECT status FROM orders_partial WHERE id = @id;
  > -- @set_status
  > UPDATE orders_partial SET status = @status WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
      module Enum_0 = T.Make_enum(struct
        type t = [`New | `Paid | `Shipped]
        let inj = function | "new" -> `New | "paid" -> `Paid | "shipped" -> `Shipped | s -> failwith (Printf.sprintf "Invalid enum value: %s" s)
        let proj = function  | `New -> "new"| `Paid -> "paid"| `Shipped -> "shipped"
      end)
  
    let create_orders_partial db  =
      T.execute db ("CREATE TABLE orders_partial (\n\
    id INT NOT NULL,\n\
      status ENUM('new','paid','shipped') NOT NULL\n\
  )") T.no_params
  
    let get_status db ~id callback =
      let invoke_callback stmt =
        callback
          ~status:(Enum_0.get_column stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db ("SELECT status FROM orders_partial WHERE id = ?") set_params invoke_callback
  
    let set_status db ~status ~id =
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_string p (Order_status.to_db status);
        T.set_param_Int p id;
        T.finish_params p
      in
      T.execute db ("UPDATE orders_partial SET status = ? WHERE id = ?") set_params
  
    module Fold = struct
      let get_status db ~id callback acc =
        let invoke_callback stmt =
          callback
            ~status:(Enum_0.get_column stmt 0)
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT status FROM orders_partial WHERE id = ?") set_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let get_status db ~id callback =
        let invoke_callback stmt =
          callback
            ~status:(Enum_0.get_column stmt 0)
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT status FROM orders_partial WHERE id = ?") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Partial codec on ENUM, reverse: only get_column= is customized, reads go through
the codec, writes still need Enum_N, so it is emitted and referenced by set_param
(Enum_N is only generated in -enum-poly-variant mode):
  $ sqlgg -gen caml -no-header -dialect=mysql -enum-poly-variant - <<'EOF' 2>&1
  > CREATE TABLE orders_partial2 (
  >   id INT NOT NULL,
  >   -- [sqlgg] get_column=Order_status.of_db
  >   status ENUM('new','paid','shipped') NOT NULL
  > );
  > -- @get_status
  > SELECT status FROM orders_partial2 WHERE id = @id;
  > -- @set_status
  > UPDATE orders_partial2 SET status = @status WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
      module Enum_0 = T.Make_enum(struct
        type t = [`New | `Paid | `Shipped]
        let inj = function | "new" -> `New | "paid" -> `Paid | "shipped" -> `Shipped | s -> failwith (Printf.sprintf "Invalid enum value: %s" s)
        let proj = function  | `New -> "new"| `Paid -> "paid"| `Shipped -> "shipped"
      end)
  
    let create_orders_partial2 db  =
      T.execute db ("CREATE TABLE orders_partial2 (\n\
    id INT NOT NULL,\n\
      status ENUM('new','paid','shipped') NOT NULL\n\
  )") T.no_params
  
    let get_status db ~id callback =
      let invoke_callback stmt =
        callback
          ~status:(Order_status.of_db (T.get_column_string stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db ("SELECT status FROM orders_partial2 WHERE id = ?") set_params invoke_callback
  
    let set_status db ~status ~id =
      let set_params stmt =
        let p = T.start_params stmt (2) in
        Enum_0.set_param p status;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.execute db ("UPDATE orders_partial2 SET status = ? WHERE id = ?") set_params
  
    module Fold = struct
      let get_status db ~id callback acc =
        let invoke_callback stmt =
          callback
            ~status:(Order_status.of_db (T.get_column_string stmt 0))
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT status FROM orders_partial2 WHERE id = ?") set_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let get_status db ~id callback =
        let invoke_callback stmt =
          callback
            ~status:(Order_status.of_db (T.get_column_string stmt 0))
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT status FROM orders_partial2 WHERE id = ?") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Standalone fully qualified codec on a non-ENUM column works over the runtime
representation (int64 for INT):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE items (
  >   -- [sqlgg] get_column=Item_id.of_int
  >   -- [sqlgg] set_param=Item_id.to_int
  >   id INT NOT NULL
  > );
  > -- @get_item
  > SELECT id FROM items;
  > -- @insert_item
  > INSERT INTO items (id) VALUES (@id);
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_items db  =
      T.execute db ("CREATE TABLE items (\n\
        id INT NOT NULL\n\
  )") T.no_params
  
    let get_item db  callback =
      let invoke_callback stmt =
        callback
          ~id:(Item_id.of_int (T.get_column_int64 stmt 0))
      in
      T.select db ("SELECT id FROM items") T.no_params invoke_callback
  
    let insert_item db ~id =
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_int64 p (Item_id.to_int id);
        T.finish_params p
      in
      T.execute db ("INSERT INTO items (id) VALUES (?)") set_params
  
    module Fold = struct
      let get_item db  callback acc =
        let invoke_callback stmt =
          callback
            ~id:(Item_id.of_int (T.get_column_int64 stmt 0))
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT id FROM items") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let get_item db  callback =
        let invoke_callback stmt =
          callback
            ~id:(Item_id.of_int (T.get_column_int64 stmt 0))
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT id FROM items") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

module= with get_column=/set_param= renames still works as before:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE orders_mod (
  >   id INT NOT NULL,
  >   -- [sqlgg] module=Order_status
  >   -- [sqlgg] get_column=of_db
  >   -- [sqlgg] set_param=to_db
  >   status ENUM('new','paid','shipped') NOT NULL
  > );
  > -- @get_status
  > SELECT status FROM orders_mod WHERE id = @id;
  > -- @set_status
  > UPDATE orders_mod SET status = @status WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_orders_mod db  =
      T.execute db ("CREATE TABLE orders_mod (\n\
    id INT NOT NULL,\n\
          status ENUM('new','paid','shipped') NOT NULL\n\
  )") T.no_params
  
    let get_status db ~id callback =
      let invoke_callback stmt =
        callback
          ~status:(Order_status.of_db (T.get_column_string stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db ("SELECT status FROM orders_mod WHERE id = ?") set_params invoke_callback
  
    let set_status db ~status ~id =
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_string p (Order_status.to_db status);
        T.set_param_Int p id;
        T.finish_params p
      in
      T.execute db ("UPDATE orders_mod SET status = ? WHERE id = ?") set_params
  
    module Fold = struct
      let get_status db ~id callback acc =
        let invoke_callback stmt =
          callback
            ~status:(Order_status.of_db (T.get_column_string stmt 0))
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT status FROM orders_mod WHERE id = ?") set_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let get_status db ~id callback =
        let invoke_callback stmt =
          callback
            ~status:(Order_status.of_db (T.get_column_string stmt 0))
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT status FROM orders_mod WHERE id = ?") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Nullable ENUM column with standalone codec: _nullable suffix is appended
to the codec function on reads, writes unwrap the option before the codec:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE orders_null (
  >   id INT NOT NULL,
  >   -- [sqlgg] get_column=Order_status.of_db
  >   -- [sqlgg] set_param=Order_status.to_db
  >   status ENUM('new','paid','shipped') NULL
  > );
  > -- @get_status
  > SELECT status FROM orders_null WHERE id = @id;
  > -- @set_status
  > UPDATE orders_null SET status = @status WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_orders_null db  =
      T.execute db ("CREATE TABLE orders_null (\n\
    id INT NOT NULL,\n\
        status ENUM('new','paid','shipped') NULL\n\
  )") T.no_params
  
    let get_status db ~id callback =
      let invoke_callback stmt =
        callback
          ~status:(Order_status.of_db_nullable (T.get_column_string_nullable stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db ("SELECT status FROM orders_null WHERE id = ?") set_params invoke_callback
  
    let set_status db ~status ~id =
      let set_params stmt =
        let p = T.start_params stmt (2) in
        begin match status with None -> T.set_param_null p | Some status -> T.set_param_string p (Order_status.to_db status); end;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.execute db ("UPDATE orders_null SET status = ? WHERE id = ?") set_params
  
    module Fold = struct
      let get_status db ~id callback acc =
        let invoke_callback stmt =
          callback
            ~status:(Order_status.of_db_nullable (T.get_column_string_nullable stmt 0))
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT status FROM orders_null WHERE id = ?") set_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let get_status db ~id callback =
        let invoke_callback stmt =
          callback
            ~status:(Order_status.of_db_nullable (T.get_column_string_nullable stmt 0))
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT status FROM orders_null WHERE id = ?") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

