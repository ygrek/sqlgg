#!/bin/sh

SQL_FILE=$1
COMPARE_FILE=$2

cat "$SQL_FILE" | sqlgg -no-header -gen caml_io -params unnamed -gen caml - > output.ml

# Note: We normalize line endings before comparison to ensure consistent results
# across different operating systems (Windows CRLF vs Unix LF)
# This prevents diff from reporting false differences on Windows systems
# where the standard diff command doesn't support the --strip-trailing-cr option
tr -d '\r' < output.ml > output_normalized.ml
tr -d '\r' < "$COMPARE_FILE" > compare_normalized.ml
diff output_normalized.ml compare_normalized.ml
exit $?
