let () =
  let file = Sys.argv.(1) in
  Mybuild.Version.save ~identify:false file
