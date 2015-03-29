#! /usr/bin/spf4
\ #! /home/ygrek/work/forth/spf/spf4

REQUIRE ATTACH ~pinka/samples/2005/lib/append-file.f
REQUIRE USER-TYPE ~ygrek/lib/typestr.f
REQUIRE XHTML ~ygrek/lib/xhtml/core.f
REQUIRE NOT ~profit/lib/logic.f
REQUIRE DateTime>PAD ~ygrek/lib/spec/unixdate.f
REQUIRE FileLines=> ~ygrek/lib/filelines.f
REQUIRE BACKSTRFREE ~ygrek/lib/backstr.f
REQUIRE StartAppWait ~ygrek/lib/linux/process.f
REQUIRE content:html ~ygrek/lib/net/cgi.f
REQUIRE XHTML-EXTRA ~ygrek/lib/xhtml/extra.f

ALSO XMLSAFE
ALSO XHTML

\ Every page
: <page> ( `title -- )
   PRO
   xml-declaration
   doctype-strict
   xhtml
   << `head tag
     << `application/xhtml+xml;charset=utf-8 `content-type http-equiv >>
     << `title tag ( `title ) TYPE >>
   >>

   `body tag
   CONT ;

: sdiv ( `style --> \ <-- ) PRO %[ `style $$ ]% `div atag CONT ;
: option ( `value `name -- ) 2SWAP %[ `value $$ ]% `option atag TYPE ;

: render-edit ( a u -- )
  S" " form-post
  <<
   S" float:left; margin: 0 1em 1em 0" sdiv

   %[ `content `name $$ `25 `rows $$ `80 `cols $$ ]% `textarea atag
   ( a u ) TYPE
  >>

  << S" float:left; padding: 0 1em; background-color: #eee; border: 1px solid green" sdiv
    << `h3 tag S" Example" TYPE >>
    << `pre tag
" CREATE TABLE t1 (x INT, name TEXT);
CREATE TABLE t2 (y INT, name TEXT);
CREATE TABLE t3 (z INT, r INT);

SELECT *, y+z AS q FROM t1
  JOIN t2 USING (name)
  JOIN t3 ON x = r
WHERE x = @val;" STYPE
    >>
  >>

  <<
   S" clear:left" sdiv

( << `div tag S" Output query parameters substitution :" TYPE
   <<
    %[ `params `name $$ ]% `select atag
    `input S" As is" option
    `named S" Only named" option
    `unnamed S" Only unnamed" option
   >>
  >>
)

   <<
   %[ `gen `name $$ ]% `select atag
   `cxx `C++ option
   `csharp `C# option
   `java `Java option
   `caml `OCaml option
   `xml  `XML option
   >>

   S" generate code " `button `submit input
  >>
;

20 1024 * CONSTANT limit

: gen-param ( `s -- `s2 )
  2DUP `caml CEQUAL IF EXIT THEN
  2DUP `xml  CEQUAL IF EXIT THEN
  2DUP `java CEQUAL IF EXIT THEN
  2DUP `csharp CEQUAL IF EXIT THEN
  2DROP `cxx ;

: process ( a u -- )
  `p tag
  hrule
  << `h2 tag S" Input" TYPE >>
  DUP limit > IF DROP limit S" Input too long, truncated" TYPE CR THEN
  << `pre tag 2DUP TYPE >>
  hrule
	(( S" data_sqlgg" DROP 0x1FF )) mkdir DROP
	ms@ { tick | src dst err gen }
  `gen GetParam gen-param >STR TO gen
  tick " data_sqlgg/{n}.in" -> src
  tick " data_sqlgg/{n}.out" -> dst
  tick " data_sqlgg/{n}.err" -> err
  \ src STR@ TYPE CR dst STR@ TYPE CR err STR@ TYPE CR
  src STR@ OCCUPY
  err STR@ dst STR@ src STR@ gen STR@
  " ./sqlgg.native -gen {s} {s} > {s} 2> {s}" STR@ StartAppWait DROP
  dst STR@ FILE
  << `h2 tag S" Output" TYPE >>
  << `pre tag TYPE >>
  hrule
  err STR@ FILE DUP 0= IF 2DROP EXIT THEN
  << `h2 tag S" Errors" TYPE >>
  << `pre tag TYPE >>
  hrule
  ;

: ask-input
  << `p tag
    S" Input SQL statements terminated with semicolon (;) each. " TYPE
    S" Use ? or @name for binding slots" TYPE
  >>
  S" " render-edit ;

: main ( -- )
  S" SQL Guided (code) generator" 2DUP <page>
	<< `h1 tag `/p/sqlgg/ link-text >>
  `content GetParam DUP 0= IF 2DROP ask-input ELSE process THEN
  \ S" CREATE TABLE x (z INT);" process
;

PREVIOUS
PREVIOUS

: env ENVIRONMENT? NOT IF S" " THEN ;

: TAB 0x09 EMIT ;

ALSO CGI

: log_request
  LAMBDA{
  TIME&DATE DateTime>PAD TYPE TAB
  `REMOTE_ADDR env TYPE TAB
  `REQUEST_METHOD env TYPE TAB
  `SCRIPT_NAME env TYPE SPACE
  `QUERY_STRING env TYPE TAB
  `HTTP_USER_AGENT env TYPE
  } TYPE>STR BACKSTRFREE STR@ `request.log ATTACH-LINE-CATCH DROP ;

: headers
  content:xhtml
\  S" Cache-Control: no-cache" TYPE CR
;

: content
  log_request
  get-params
  main
  CR ;

: index
  headers
  ['] content TYPE>STR DUP STRLEN content-length
  CR
  STYPE
  BYE ;

PREVIOUS

\ : index headers CR content ;

\ : REQUEST_METHOD S" GET" ;
\ : QUERY_STRING S" page_name=MainPage" ;

\ ' TYPE1 TO USER-TYPE

: save
  ['] index MAINX !
  `sqlgg.cgi SAVE
  `sqlgg.cgi.o DELETE-FILE DROP
  `sqlgg.cgi.ld DELETE-FILE DROP
;

save BYE

