module Sqlgg :
  (T : Sqlgg_traits.M) ->
    sig
      module IO = Sqlgg_io.Blocking
      val create_users : [> `WR ] T.connection -> T.execute_response
      val insert_returning_id :
        [> `RO ] T.connection ->
        name:T.Types.Text.t -> nick:T.Types.Text.t option -> T.Types.Int.t
      val insert_returning_all :
        [> `RO ] T.connection ->
        name:T.Types.Text.t ->
        nick:T.Types.Text.t option ->
        T.Types.Int.t * T.Types.Text.t * T.Types.Text.t option
      val insert_returning_nullable :
        [> `RO ] T.connection ->
        name:T.Types.Text.t ->
        nick:T.Types.Text.t option -> T.Types.Text.t option
      val insert_returning_expr :
        [> `RO ] T.connection ->
        name:T.Types.Text.t ->
        nick:T.Types.Text.t option -> T.Types.Int.t * T.Types.Text.t
      val insert_returning_param :
        [> `RO ] T.connection ->
        name:T.Types.Text.t ->
        nick:T.Types.Text.t option ->
        suffix:T.Types.Text.t -> T.Types.Int.t * T.Types.Text.t
      val insert_set_returning :
        [> `RO ] T.connection ->
        name:T.Types.Text.t -> nick:T.Types.Text.t option -> T.Types.Int.t
      val insert_tuple_list_returning :
        [> `RO ] T.connection ->
        values:(T.Types.Text.t * T.Types.Text.t option) list ->
        (id:T.Types.Int.t -> nick:T.Types.Text.t option -> unit) ->
        unit IO.future
      val insert_multi_values_returning :
        [> `RO ] T.connection ->
        name1:T.Types.Text.t ->
        nick1:T.Types.Text.t option ->
        name2:T.Types.Text.t ->
        nick2:T.Types.Text.t option -> (id:T.Types.Int.t -> unit) -> unit
      val insert_select_returning :
        [> `RO ] T.connection ->
        min:T.Types.Int.t -> (id:T.Types.Int.t -> unit) -> unit
      val insert_on_conflict_returning :
        [> `RO ] T.connection ->
        id:T.Types.Int.t ->
        name:T.Types.Text.t -> T.Types.Int.t * T.Types.Text.t option
      val insert_do_nothing_returning :
        [> `RO ] T.connection ->
        id:T.Types.Int.t ->
        name:T.Types.Text.t -> (T.Types.Int.t * T.Types.Text.t option) option
      val update_returning :
        [> `RO ] T.connection ->
        name:T.Types.Text.t ->
        id:T.Types.Int.t ->
        (id:T.Types.Int.t -> nick:T.Types.Text.t option -> unit) -> unit
      val update_returning_param :
        [> `RO ] T.connection ->
        name:T.Types.Text.t ->
        id:T.Types.Int.t ->
        suffix:T.Types.Text.t ->
        (id:T.Types.Int.t -> tagged:T.Types.Text.t -> unit) -> unit
      val delete_returning :
        [> `RO ] T.connection ->
        id:T.Types.Int.t ->
        (id:T.Types.Int.t ->
         name:T.Types.Text.t -> nick:T.Types.Text.t option -> unit) ->
        unit
      val delete_returning_param :
        [> `RO ] T.connection ->
        id:T.Types.Int.t ->
        suffix:T.Types.Text.t ->
        (id:T.Types.Int.t -> tagged:T.Types.Text.t -> unit) -> unit
      module Single :
        sig
          val insert_returning_id :
            [> `RO ] T.connection ->
            name:T.Types.Text.t ->
            nick:T.Types.Text.t option -> (id:T.Types.Int.t -> 'a) -> 'a
          val insert_returning_all :
            [> `RO ] T.connection ->
            name:T.Types.Text.t ->
            nick:T.Types.Text.t option ->
            (id:T.Types.Int.t ->
             name:T.Types.Text.t -> nick:T.Types.Text.t option -> 'a) ->
            'a
          val insert_returning_nullable :
            [> `RO ] T.connection ->
            name:T.Types.Text.t ->
            nick:T.Types.Text.t option ->
            (nick:T.Types.Text.t option -> 'a) -> 'a
          val insert_returning_expr :
            [> `RO ] T.connection ->
            name:T.Types.Text.t ->
            nick:T.Types.Text.t option ->
            (id:T.Types.Int.t -> greeting:T.Types.Text.t -> 'a) -> 'a
          val insert_returning_param :
            [> `RO ] T.connection ->
            name:T.Types.Text.t ->
            nick:T.Types.Text.t option ->
            suffix:T.Types.Text.t ->
            (id:T.Types.Int.t -> tagged:T.Types.Text.t -> 'a) -> 'a
          val insert_set_returning :
            [> `RO ] T.connection ->
            name:T.Types.Text.t ->
            nick:T.Types.Text.t option -> (id:T.Types.Int.t -> 'a) -> 'a
          val insert_on_conflict_returning :
            [> `RO ] T.connection ->
            id:T.Types.Int.t ->
            name:T.Types.Text.t ->
            (id:T.Types.Int.t -> nick:T.Types.Text.t option -> 'a) -> 'a
          val insert_do_nothing_returning :
            [> `RO ] T.connection ->
            id:T.Types.Int.t ->
            name:T.Types.Text.t ->
            (id:T.Types.Int.t -> nick:T.Types.Text.t option -> 'a) ->
            'a option
        end
      module Fold :
        sig
          val insert_tuple_list_returning :
            [> `RO ] T.connection ->
            values:(T.Types.Text.t * T.Types.Text.t option) list ->
            (id:T.Types.Int.t ->
             nick:T.Types.Text.t option -> 'a IO.future -> 'a IO.future) ->
            'a IO.future -> 'a IO.future
          val insert_multi_values_returning :
            [> `RO ] T.connection ->
            name1:T.Types.Text.t ->
            nick1:T.Types.Text.t option ->
            name2:T.Types.Text.t ->
            nick2:T.Types.Text.t option ->
            (id:T.Types.Int.t -> 'a IO.future -> 'a IO.future) ->
            'a IO.future -> 'a IO.future
          val insert_select_returning :
            [> `RO ] T.connection ->
            min:T.Types.Int.t ->
            (id:T.Types.Int.t -> 'a IO.future -> 'a IO.future) ->
            'a IO.future -> 'a IO.future
          val update_returning :
            [> `RO ] T.connection ->
            name:T.Types.Text.t ->
            id:T.Types.Int.t ->
            (id:T.Types.Int.t ->
             nick:T.Types.Text.t option -> 'a IO.future -> 'a IO.future) ->
            'a IO.future -> 'a IO.future
          val update_returning_param :
            [> `RO ] T.connection ->
            name:T.Types.Text.t ->
            id:T.Types.Int.t ->
            suffix:T.Types.Text.t ->
            (id:T.Types.Int.t ->
             tagged:T.Types.Text.t -> 'a IO.future -> 'a IO.future) ->
            'a IO.future -> 'a IO.future
          val delete_returning :
            [> `RO ] T.connection ->
            id:T.Types.Int.t ->
            (id:T.Types.Int.t ->
             name:T.Types.Text.t ->
             nick:T.Types.Text.t option -> 'a IO.future -> 'a IO.future) ->
            'a IO.future -> 'a IO.future
          val delete_returning_param :
            [> `RO ] T.connection ->
            id:T.Types.Int.t ->
            suffix:T.Types.Text.t ->
            (id:T.Types.Int.t ->
             tagged:T.Types.Text.t -> 'a IO.future -> 'a IO.future) ->
            'a IO.future -> 'a IO.future
        end
      module List :
        sig
          val insert_tuple_list_returning :
            [> `RO ] T.connection ->
            values:(T.Types.Text.t * T.Types.Text.t option) list ->
            (id:T.Types.Int.t -> nick:T.Types.Text.t option -> 'a) ->
            'a list IO.future IO.future
          val insert_multi_values_returning :
            [> `RO ] T.connection ->
            name1:T.Types.Text.t ->
            nick1:T.Types.Text.t option ->
            name2:T.Types.Text.t ->
            nick2:T.Types.Text.t option ->
            (id:T.Types.Int.t -> 'a) -> 'a list IO.future IO.future
          val insert_select_returning :
            [> `RO ] T.connection ->
            min:T.Types.Int.t ->
            (id:T.Types.Int.t -> 'a) -> 'a list IO.future IO.future
          val update_returning :
            [> `RO ] T.connection ->
            name:T.Types.Text.t ->
            id:T.Types.Int.t ->
            (id:T.Types.Int.t -> nick:T.Types.Text.t option -> 'a) ->
            'a list IO.future IO.future
          val update_returning_param :
            [> `RO ] T.connection ->
            name:T.Types.Text.t ->
            id:T.Types.Int.t ->
            suffix:T.Types.Text.t ->
            (id:T.Types.Int.t -> tagged:T.Types.Text.t -> 'a) ->
            'a list IO.future IO.future
          val delete_returning :
            [> `RO ] T.connection ->
            id:T.Types.Int.t ->
            (id:T.Types.Int.t ->
             name:T.Types.Text.t -> nick:T.Types.Text.t option -> 'a) ->
            'a list IO.future IO.future
          val delete_returning_param :
            [> `RO ] T.connection ->
            id:T.Types.Int.t ->
            suffix:T.Types.Text.t ->
            (id:T.Types.Int.t -> tagged:T.Types.Text.t -> 'a) ->
            'a list IO.future IO.future
        end
    end
