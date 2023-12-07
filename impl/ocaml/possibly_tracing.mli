val span :
   ?operation:string ->
   ?tables:string list ->
   system: [ `mariadb | `mysql | `sqlite ] ->
   sql:string ->
   string (* span-name *) ->
   (unit -> 'rv) ->
   'rv
