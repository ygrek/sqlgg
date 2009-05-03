#! /home/ygrek/work/forth/spf/spf4
\ #! /usr/bin/spf4

REQUIRE ATTACH ~pinka/samples/2005/lib/append-file.f
REQUIRE USER-TYPE ~ygrek/lib/typestr.f
\ :NONAME S" wiki.err" ATTACH ; TO USER-TYPE \ no stdout

REQUIRE XSLTmm ~ac/lib/lin/xml/xslt.f
REQUIRE XHTML ~ygrek/lib/xhtml/core.f
REQUIRE DumpParams ~ac/lib/string/get_params.f
REQUIRE EQUAL ~pinka/spf/string-equal.f
REQUIRE NOT ~profit/lib/logic.f
REQUIRE cat ~ygrek/lib/cat.f
REQUIRE ALLOCATED ~pinka/lib/ext/basics.f
REQUIRE NUMBER ~ygrek/lib/parse.f
REQUIRE [env] ~ygrek/lib/env.f
REQUIRE DateTime>PAD ~ygrek/lib/spec/unixdate.f
REQUIRE FileLines=> ~ygrek/lib/filelines.f
REQUIRE READ-FILE-EXACT ~pinka/lib/files-ext.f

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

: process ( a u -- ) 
  `p tag TYPE ;

: main ( -- )
  S" Main" <page>
  `content GetParam DUP 0= IF 2DROP S" " render-edit ELSE process THEN
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

: save ['] index MAINX ! `test.cgi SAVE ; 
\ save BYE

index
