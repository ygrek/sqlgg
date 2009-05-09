#! /usr/bin/spf4
\ #! /home/ygrek/work/forth/spf/spf4

REQUIRE ATTACH ~pinka/samples/2005/lib/append-file.f
REQUIRE USER-TYPE ~ygrek/lib/typestr.f
\ :NONAME 2DROP ; TO USER-TYPE \ no stdout

REQUIRE XSLTmm ~ac/lib/lin/xml/xslt.f
REQUIRE XHTML ~ygrek/lib/xhtml/core.f
REQUIRE DumpParams ~ac/lib/string/get_params.f
REQUIRE EQUAL ~pinka/spf/string-equal.f
REQUIRE NOT ~profit/lib/logic.f
REQUIRE cat ~ygrek/lib/cat.f
REQUIRE ALLOCATED ~pinka/lib/ext/basics.f
REQUIRE NUMBER ~ygrek/lib/parse.f
REQUIRE DateTime>PAD ~ygrek/lib/spec/unixdate.f
REQUIRE FileLines=> ~ygrek/lib/filelines.f
REQUIRE READ-FILE-EXACT ~pinka/lib/files-ext.f
\ REQUIRE CREATE-ANON-PIPE ~ygrek/lib/sys/pipe.f

: (sys) ( az -- x )
  (()) fork ?DUP 
  IF
    NIP
    1 <( 0 0 )) waitpid
  ELSE
    \ FIXME
    >R
    S" /bin/sh" DROP DUP 2 <( S" -c" DROP R> 0 )) execlp \ no return
  THEN ;

: sys ( a u -- ) DROP (sys) DROP ;

: BACKSTRFREE ( s --> s \ <-- ) PRO BACK STRFREE TRACKING RESTB CONT ;
: SEVALUATE BACKSTRFREE STR@ EVALUATE ;
\ append s1 to s
: SAPPEND ( s s1 -- s' ) OVER S+ ;

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
\     << `wiki.css link-stylesheet >>
   >>

   `body tag
   CONT ;

: input ( `value `name `type -- ) %[ `type $$ `name $$ `value $$ ]% `input /atag ;

: render-edit ( a u -- )
  \ << `h1 tag S" Nota bene: Editing is disabled ('save' will ignore your changes)" TYPE >>
  %[ `POST `method $$ S" " `action $$ ]% `form atag
  `div tag

  <<
    %[ `content `name $$ `25 `rows $$ `80 `cols $$ ]% `textarea atag
    ( a u ) TYPE
  >>

  `save `button `submit input
;

: GetParamInt ( `str -- n ) GetParam NUMBER NOT IF 0 THEN ;

20 1024 * CONSTANT limit

: process ( a u -- ) 
  `p tag 
  << `h2 tag S" Input" TYPE >>
  DUP limit > IF DROP limit S" Input too long, truncated" TYPE CR THEN
  << `pre tag 2DUP TYPE >>
  hrule
	(( S" sql" DROP 0x1FF )) mkdir DROP
	ms@ { tick | src dst err }
  tick " sql/{n}.in" -> src
  tick " sql/{n}.out" -> dst
  tick " sql/{n}.err" -> err
  \ src STR@ TYPE CR dst STR@ TYPE CR err STR@ TYPE CR
  src STR@ OCCUPY
  err STR@ dst STR@ src STR@ " ./sqlgg.native {s} > {s} 2> {s}" STR@ sys
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
  S" SQL to C++ code generator" 2DUP <page>
	<< `h1 tag `/p/sqlgg.html link-text >>
  `content GetParam DUP 0= IF 2DROP ask-input ELSE process THEN
  \ S" CREATE TABLE x (z INT);" process
;

PREVIOUS
PREVIOUS

: content:html S" Content-type: text/html" TYPE CR ;
: content:xhtml S" Content-type: application/xhtml+xml" TYPE CR ;
: content-length " Content-Length: {n}" STYPE CR ;

\ : comment PRO ." <!-- " CONT ." -->" ;

: get_post_params
  S" CONTENT_LENGTH" ENVIRONMENT? NOT IF EXIT THEN
  NUMBER NOT IF EXIT THEN
  ALLOCATED 2DUP H-STDIN READ-FILE-EXACT IF 2DROP ELSE GetParamsFromString THEN ;
  \ ALLOCATED 2DUP H-STDIN READ-FILE . NIP 2DUP TYPE CR GetParamsFromString ;

: get_get_params
  S" QUERY_STRING" ENVIRONMENT? IF GetParamsFromString THEN ;

: get_params
  `REQUEST_METHOD ENVIRONMENT? NOT IF EXIT THEN
   2DUP `POST CEQUAL IF 2DROP get_post_params EXIT THEN
   2DUP `GET  CEQUAL IF 2DROP get_get_params EXIT THEN
   2DROP ;

[UNDEFINED] WINAPI: [IF]
: environ 
  S" environ" symbol-lookup symbol-address @
  BEGIN
   DUP @
  WHILE
   DUP @ ASCIIZ> TYPE CR
   CELL+
  REPEAT
  DROP ;
[ELSE]

WINAPI: GetEnvironmentStrings KERNEL32.DLL

: environ
  GetEnvironmentStrings 
  BEGIN
   DUP B@
  WHILE
   ASCIIZ> 2DUP TYPE CR
   + 1+
  REPEAT 
  DROP ;

[THEN]

: env ENVIRONMENT? NOT IF S" " THEN ;

: TAB 0x09 EMIT ;

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
  get_params
  main
  CR ;

: index
  headers
  ['] content TYPE>STR DUP STRLEN content-length 
  CR
  STYPE
  BYE ;

\ : index headers CR content ;

\ : REQUEST_METHOD S" GET" ;
\ : QUERY_STRING S" page_name=MainPage" ;

\ ' TYPE1 TO USER-TYPE

: save ['] index MAINX ! `sql.cgi SAVE ; 
 save BYE

index
