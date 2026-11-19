type 'run t =
  | Reversible of {
      id : Migration_id.t;
      apply : 'run;
      revert : 'run;
    }
  | Irreversible of {
      id : Migration_id.t;
      apply : 'run;
    }

let reversible id ~apply ~revert = Reversible { id; apply; revert }

let irreversible id ~apply = Irreversible { id; apply }

let id = function
  | Reversible { id; _ }
  | Irreversible { id; _ } ->
      id
