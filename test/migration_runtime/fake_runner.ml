module Linear
    (Migrations : Sqlgg_migrations.Runner.Migrations
                    with type 'a connection := 'a Fake_database.connection
                     and type execute_response := Fake_database.execute_response
                     and type 'a future := 'a Fake_database.IO.future) =
  Sqlgg_migrations.Runner.Make_with_store
    (Fake_database)
    (Fake_database)
    (struct
      let history = Sqlgg_migrations.Planner.Linear
    end)
    (Migrations)
