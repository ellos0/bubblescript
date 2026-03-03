module Codegen (showC) where

import Struct

showC :: LispVal -> String
showC (Atom x) = x
showC (Number x) = show x
showC (String x) = "\"" ++ x ++ "\""
showC (Bool x) = if x then "1" else "0"
showC (List []) = ""
showC (List (op:args)) = case (op) of
  (Atom "+") -> concat [showC (args !! 0), "+", showC (args !! 1)]
  (Atom "-") -> concat [showC (args !! 0), "-", showC (args !! 1)]
  (Atom "*") -> concat [showC (args !! 0), "*", showC (args !! 1)]
  (Atom "/") -> concat [showC (args !! 0), "/", showC (args !! 1)]
  (Atom "print") -> concat ["printf(", showC (args !! 0), ");\n"]
  (Atom "defun") -> concat [showC (args !! 1), " ", showC (args !! 0), "(){\n", showC (args !! 2), "}"]
  (Atom "type") -> (case (args !! 0) of
                      (Atom "i4") -> "int"
                      (Atom "i2") -> "short"
                      _ -> "int"
                      )
  (List _) -> concat (map showC (op:args))
  _ -> ""
