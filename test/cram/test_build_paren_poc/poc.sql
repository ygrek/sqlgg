-- @poc_sqlgg_no_parens
SELECT 1 WHERE FALSE AND @b {
    None { TRUE } |
    Some { FALSE OR TRUE }
  };

-- @poc_sqlgg_with_parens
SELECT 1 WHERE FALSE AND @b {
    None { TRUE } |
    Some { (FALSE OR TRUE) }
  };
