Cache prepared statements:
  $ sqlgg -no-header -gen caml_io -params unnamed -gen caml - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c test_cached_prepared_stmts.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o test_cached_prepared_stmts.exe output.ml print_impl.ml test_cached_prepared_stmts.ml
  $ ./test_cached_prepared_stmts.exe 
  
  === TEST: Sequential Filling (TINY, size=1) ===
  [MOCK] PREPARE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  After query 1: 1 prepared, Cache: 1/1 items, 1 ops since start
  Filled tiny cache: 1/1
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  Reused cached: 1 prepared (no new preparations)
  Sequential filling test passed for TINY!
  
  === TEST: Sequential Filling (SMALL, size=2) ===
  [MOCK] PREPARE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=2 
    After query 1: 1 prepared, Cache: 1/2 items, 1 ops since start
  [MOCK] PREPARE[2]: SELECT 2
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
    After query 2: 2 prepared, Cache: 2/2 items, 2 ops since start
  Filled small cache: 2/2
  Sequential filling test passed for SMALL!
  
  === TEST: Overflow Eviction (SMALL, size=2, overflow=3) ===
  [MOCK] PREPARE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=5 
    Fill 1/2: 1 prepared, Cache: 1/2 items, 1 ops since start
  [MOCK] PREPARE[2]: SELECT 2
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=4 
    Fill 2/2: 2 prepared, Cache: 2/2 items, 2 ops since start
  [MOCK] PREPARE[3]: SELECT 3
  [MOCK] CLOSE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=3 
    Overflow 1: 3 prepared, Cache: 2/2 items, 3 ops since start
  [MOCK] PREPARE[4]: SELECT 4
  [MOCK] CLOSE[2]: SELECT 2
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=2 
    Overflow 2: 4 prepared, Cache: 2/2 items, 4 ops since start
  [MOCK] PREPARE[5]: SELECT 5
  [MOCK] CLOSE[3]: SELECT 3
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
    Overflow 3: 5 prepared, Cache: 2/2 items, 5 ops since start
  After overflow: 5 prepared (expected 5)
  [MOCK] PREPARE[6]: SELECT 1
  [MOCK] CLOSE[4]: SELECT 4
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=3 
  [MOCK] PREPARE[7]: SELECT 2
  [MOCK] CLOSE[5]: SELECT 5
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=2 
  [MOCK] PREPARE[8]: SELECT 3
  [MOCK] CLOSE[6]: SELECT 1
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  Evicted queries needed re-preparation: 3/3
  Overflow eviction test passed for SMALL!
  
  === TEST: LRU Pattern (MEDIUM, size=5) ===
  [MOCK] PREPARE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=15 
  [MOCK] PREPARE[2]: SELECT 2
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=14 
  [MOCK] PREPARE[3]: SELECT 3
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=13 
  [MOCK] PREPARE[4]: SELECT 4
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=12 
  [MOCK] PREPARE[5]: SELECT 5
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=11 
  Filled cache: 5 prepared
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=10 
  Used first query (made it recent)
  [MOCK] PREPARE[6]: SELECT 6
  [MOCK] CLOSE[2]: SELECT 2
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=9 
  Added new query: 6 prepared, Cache: 5/5 items, 7 ops since start
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=8 
  First query cached: YES
  [MOCK] PREPARE[7]: SELECT 2
  [MOCK] CLOSE[3]: SELECT 3
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=7 
  Second query evicted: YES
  LRU pattern test passed for MEDIUM!
  
  === TEST: Cycling Pattern (LARGE, size=10, cycles=20) ===
  [MOCK] PREPARE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=200 
  [MOCK] PREPARE[2]: SELECT 2
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=199 
  [MOCK] PREPARE[3]: SELECT 3
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=198 
  [MOCK] PREPARE[4]: SELECT 4
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=197 
  [MOCK] PREPARE[5]: SELECT 5
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=196 
  [MOCK] PREPARE[6]: SELECT 6
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=195 
  [MOCK] PREPARE[7]: SELECT 7
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=194 
  [MOCK] PREPARE[8]: SELECT 8
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=193 
  [MOCK] PREPARE[9]: SELECT 9
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=192 
  [MOCK] PREPARE[10]: SELECT 10
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=191 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=190 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=189 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=188 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=187 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=186 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=185 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=184 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=183 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=182 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=181 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=180 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=179 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=178 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=177 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=176 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=175 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=174 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=173 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=172 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=171 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=170 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=169 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=168 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=167 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=166 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=165 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=164 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=163 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=162 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=161 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=160 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=159 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=158 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=157 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=156 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=155 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=154 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=153 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=152 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=151 
      After cycle 5: 10 prepared, Cache: 10/10 items, 50 ops since start
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=150 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=149 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=148 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=147 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=146 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=145 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=144 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=143 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=142 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=141 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=140 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=139 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=138 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=137 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=136 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=135 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=134 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=133 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=132 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=131 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=130 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=129 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=128 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=127 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=126 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=125 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=124 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=123 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=122 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=121 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=120 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=119 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=118 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=117 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=116 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=115 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=114 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=113 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=112 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=111 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=110 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=109 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=108 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=107 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=106 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=105 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=104 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=103 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=102 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=101 
      After cycle 10: 10 prepared, Cache: 10/10 items, 100 ops since start
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=100 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=99 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=98 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=97 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=96 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=95 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=94 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=93 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=92 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=91 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=90 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=89 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=88 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=87 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=86 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=85 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=84 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=83 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=82 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=81 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=80 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=79 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=78 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=77 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=76 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=75 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=74 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=73 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=72 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=71 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=70 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=69 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=68 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=67 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=66 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=65 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=64 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=63 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=62 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=61 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=60 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=59 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=58 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=57 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=56 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=55 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=54 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=53 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=52 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=51 
      After cycle 15: 10 prepared, Cache: 10/10 items, 150 ops since start
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=50 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=49 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=48 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=47 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=46 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=45 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=44 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=43 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=42 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=41 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=40 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=39 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=38 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=37 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=36 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=35 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=34 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=33 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=32 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=31 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=30 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=29 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=28 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=27 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=26 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=25 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=24 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=23 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=22 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=21 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=20 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=19 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=18 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=17 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=16 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=15 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=14 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=13 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=12 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=11 
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=10 
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=9 
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=8 
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=7 
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=6 
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=5 
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=4 
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=3 
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=2 
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
      After cycle 20: 10 prepared, Cache: 10/10 items, 200 ops since start
  Total prepared after 20 cycles: 10 (expected 10 for first cycle only)
  Cycling pattern test passed for LARGE!
  
  === TEST: TTL non-zero (size=2, ttl=3s) ===
  [MOCK] PREPARE[1]: SELECT 42
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  
  --- MOCK STATS ---
  Prepared: 1, Closed: 0, Open: 1
  Operations:
    1. PREPARE[1]: SELECT 42
    2. SELECT_WITH_STMT[1]
  ---
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  
  --- MOCK STATS ---
  Prepared: 1, Closed: 0, Open: 1
  Operations:
    1. PREPARE[1]: SELECT 42
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
  ---
  TTL>0 immediate reuse did not close — OK
  
  === TEST: TTL delayed expiry (size=2, ttl=0.5s) ===
  [MOCK] PREPARE[1]: SELECT 7
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  
  --- MOCK STATS ---
  Prepared: 1, Closed: 0, Open: 1
  Operations:
    1. PREPARE[1]: SELECT 7
    2. SELECT_WITH_STMT[1]
  ---
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  [MOCK] CLOSE[1]: SELECT 7
  
  --- MOCK STATS ---
  Prepared: 1, Closed: 1, Open: 0
  Operations:
    1. PREPARE[1]: SELECT 7
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
    4. CLOSE[1]: SELECT 7
  ---
  TTL=0.5s reuse after >0.5s closed cached — OK
  [MOCK] PREPARE[2]: SELECT 7
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  
  --- MOCK STATS ---
  Prepared: 2, Closed: 1, Open: 1
  Operations:
    1. PREPARE[1]: SELECT 7
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
    4. CLOSE[1]: SELECT 7
    5. PREPARE[2]: SELECT 7
    6. SELECT_WITH_STMT[2]
  ---
  TTL=0.5s re-prepared after expiry — OK
  
  === TEST: TTL hit/miss behavior (size=2, ttl=0.5s) ===
  [MOCK] PREPARE[1]: SELECT 10
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=8 
  
  --- MOCK STATS ---
  Prepared: 1, Closed: 0, Open: 1
  Operations:
    1. PREPARE[1]: SELECT 10
    2. SELECT_WITH_STMT[1]
  ---
  MISS (prepare) SELECT 10 — OK
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=7 
  
  --- MOCK STATS ---
  Prepared: 1, Closed: 0, Open: 1
  Operations:
    1. PREPARE[1]: SELECT 10
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
  ---
  HIT within TTL SELECT 10 — OK
  [MOCK] PREPARE[2]: SELECT 11
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=6 
  
  --- MOCK STATS ---
  Prepared: 2, Closed: 0, Open: 2
  Operations:
    1. PREPARE[1]: SELECT 10
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
    4. PREPARE[2]: SELECT 11
    5. SELECT_WITH_STMT[2]
  ---
  MISS (prepare) SELECT 11 — OK
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=5 
  [MOCK] CLOSE[1]: SELECT 10
  
  --- MOCK STATS ---
  Prepared: 2, Closed: 1, Open: 1
  Operations:
    1. PREPARE[1]: SELECT 10
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
    4. PREPARE[2]: SELECT 11
    5. SELECT_WITH_STMT[2]
    6. SELECT_WITH_STMT[1]
    7. CLOSE[1]: SELECT 10
  ---
  EXPIRED-USE (closed, no prepare) SELECT 10 — OK
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=4 
  [MOCK] CLOSE[2]: SELECT 11
  
  --- MOCK STATS ---
  Prepared: 2, Closed: 2, Open: 0
  Operations:
    1. PREPARE[1]: SELECT 10
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
    4. PREPARE[2]: SELECT 11
    5. SELECT_WITH_STMT[2]
    6. SELECT_WITH_STMT[1]
    7. CLOSE[1]: SELECT 10
    8. SELECT_WITH_STMT[2]
    9. CLOSE[2]: SELECT 11
  ---
  EXPIRED-USE (closed, no prepare) SELECT 11 — OK
  [MOCK] PREPARE[3]: SELECT 10
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=3 
  
  --- MOCK STATS ---
  Prepared: 3, Closed: 2, Open: 1
  Operations:
    1. PREPARE[1]: SELECT 10
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
    4. PREPARE[2]: SELECT 11
    5. SELECT_WITH_STMT[2]
    6. SELECT_WITH_STMT[1]
    7. CLOSE[1]: SELECT 10
    8. SELECT_WITH_STMT[2]
    9. CLOSE[2]: SELECT 11
    10. PREPARE[3]: SELECT 10
    11. SELECT_WITH_STMT[3]
  ---
  MISS after expiry (re-prepare) SELECT 10 — OK
  [MOCK] PREPARE[4]: SELECT 11
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=2 
  
  --- MOCK STATS ---
  Prepared: 4, Closed: 2, Open: 2
  Operations:
    1. PREPARE[1]: SELECT 10
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
    4. PREPARE[2]: SELECT 11
    5. SELECT_WITH_STMT[2]
    6. SELECT_WITH_STMT[1]
    7. CLOSE[1]: SELECT 10
    8. SELECT_WITH_STMT[2]
    9. CLOSE[2]: SELECT 11
    10. PREPARE[3]: SELECT 10
    11. SELECT_WITH_STMT[3]
    12. PREPARE[4]: SELECT 11
    13. SELECT_WITH_STMT[4]
  ---
  MISS after expiry (re-prepare) SELECT 11 — OK
  
  === TEST: TTL with LRU mass expiry keeps only youngest (size=10, ttl=1s) ===
  [MOCK] PREPARE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=10 
  [MOCK] PREPARE[2]: SELECT 2
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=9 
  [MOCK] PREPARE[3]: SELECT 3
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=8 
  [MOCK] PREPARE[4]: SELECT 4
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=7 
  [MOCK] PREPARE[5]: SELECT 5
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=6 
  [MOCK] PREPARE[6]: SELECT 6
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=5 
  [MOCK] PREPARE[7]: SELECT 7
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=4 
  [MOCK] PREPARE[8]: SELECT 8
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=3 
  [MOCK] PREPARE[9]: SELECT 9
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=2 
  [MOCK] PREPARE[10]: SELECT 10
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  Filled LRU: Cache: 10/10 items, 10 ops since start
  [MOCK] PREPARE[11]: SELECT 11
  [MOCK] CLOSE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[11]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  Added 11th (should evict one old): Cache: 10/10 items, 11 ops since start
  
  --- MOCK STATS ---
  Prepared: 11, Closed: 1, Open: 10
  Operations:
    1. PREPARE[1]: SELECT 1
    2. SELECT_WITH_STMT[1]
    3. PREPARE[2]: SELECT 2
    4. SELECT_WITH_STMT[2]
    5. PREPARE[3]: SELECT 3
    6. SELECT_WITH_STMT[3]
    7. PREPARE[4]: SELECT 4
    8. SELECT_WITH_STMT[4]
    9. PREPARE[5]: SELECT 5
    10. SELECT_WITH_STMT[5]
    11. PREPARE[6]: SELECT 6
    12. SELECT_WITH_STMT[6]
    13. PREPARE[7]: SELECT 7
    14. SELECT_WITH_STMT[7]
    15. PREPARE[8]: SELECT 8
    16. SELECT_WITH_STMT[8]
    17. PREPARE[9]: SELECT 9
    18. SELECT_WITH_STMT[9]
    19. PREPARE[10]: SELECT 10
    20. SELECT_WITH_STMT[10]
    21. PREPARE[11]: SELECT 11
    22. CLOSE[1]: SELECT 1
    23. SELECT_WITH_STMT[11]
  ---
  [MOCK] SELECT_WITH_STMT[10]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  [MOCK] CLOSE[10]: SELECT 10
  Hit SELECT 10: YES
  [MOCK] SELECT_WITH_STMT[11]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  Hit SELECT 11: YES
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=8 
  [MOCK] CLOSE[2]: SELECT 2
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=7 
  [MOCK] CLOSE[3]: SELECT 3
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=6 
  [MOCK] CLOSE[4]: SELECT 4
  [MOCK] SELECT_WITH_STMT[5]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=5 
  [MOCK] CLOSE[5]: SELECT 5
  [MOCK] SELECT_WITH_STMT[6]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=4 
  [MOCK] CLOSE[6]: SELECT 6
  [MOCK] SELECT_WITH_STMT[7]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=3 
  [MOCK] CLOSE[7]: SELECT 7
  [MOCK] SELECT_WITH_STMT[8]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=2 
  [MOCK] CLOSE[8]: SELECT 8
  [MOCK] SELECT_WITH_STMT[9]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  [MOCK] CLOSE[9]: SELECT 9
  [MOCK] PREPARE[12]: SELECT 10
  [MOCK] SELECT_WITH_STMT[12]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  After expiration accesses: Cache: 2/10 items, 22 ops since start
  
  --- MOCK STATS ---
  Prepared: 12, Closed: 10, Open: 2
  Operations:
    1. PREPARE[1]: SELECT 1
    2. SELECT_WITH_STMT[1]
    3. PREPARE[2]: SELECT 2
    4. SELECT_WITH_STMT[2]
    5. PREPARE[3]: SELECT 3
    6. SELECT_WITH_STMT[3]
    7. PREPARE[4]: SELECT 4
    8. SELECT_WITH_STMT[4]
    9. PREPARE[5]: SELECT 5
    10. SELECT_WITH_STMT[5]
    11. PREPARE[6]: SELECT 6
    12. SELECT_WITH_STMT[6]
    13. PREPARE[7]: SELECT 7
    14. SELECT_WITH_STMT[7]
    15. PREPARE[8]: SELECT 8
    16. SELECT_WITH_STMT[8]
    17. PREPARE[9]: SELECT 9
    18. SELECT_WITH_STMT[9]
    19. PREPARE[10]: SELECT 10
    20. SELECT_WITH_STMT[10]
    21. PREPARE[11]: SELECT 11
    22. CLOSE[1]: SELECT 1
    23. SELECT_WITH_STMT[11]
    24. SELECT_WITH_STMT[10]
    25. CLOSE[10]: SELECT 10
    26. SELECT_WITH_STMT[11]
    27. SELECT_WITH_STMT[2]
    28. CLOSE[2]: SELECT 2
    29. SELECT_WITH_STMT[3]
    30. CLOSE[3]: SELECT 3
    31. SELECT_WITH_STMT[4]
    32. CLOSE[4]: SELECT 4
    33. SELECT_WITH_STMT[5]
    34. CLOSE[5]: SELECT 5
    35. SELECT_WITH_STMT[6]
    36. CLOSE[6]: SELECT 6
    37. SELECT_WITH_STMT[7]
    38. CLOSE[7]: SELECT 7
    39. SELECT_WITH_STMT[8]
    40. CLOSE[8]: SELECT 8
    41. SELECT_WITH_STMT[9]
    42. CLOSE[9]: SELECT 9
    43. PREPARE[12]: SELECT 10
    44. SELECT_WITH_STMT[12]
  ---
  TTL+LRU mass expiry left 2 open (SELECT 10, SELECT 11) — OK
  
  === TEST: TTL immediate expiry (size=2, ttl=0) ===
  [MOCK] PREPARE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  
  --- MOCK STATS ---
  Prepared: 1, Closed: 0, Open: 1
  Operations:
    1. PREPARE[1]: SELECT 1
    2. SELECT_WITH_STMT[1]
  ---
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  [MOCK] CLOSE[1]: SELECT 1
  
  --- MOCK STATS ---
  Prepared: 1, Closed: 1, Open: 0
  Operations:
    1. PREPARE[1]: SELECT 1
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
    4. CLOSE[1]: SELECT 1
  ---
  [MOCK] PREPARE[2]: SELECT 1
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  
  --- MOCK STATS ---
  Prepared: 2, Closed: 1, Open: 1
  Operations:
    1. PREPARE[1]: SELECT 1
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
    4. CLOSE[1]: SELECT 1
    5. PREPARE[2]: SELECT 1
    6. SELECT_WITH_STMT[2]
  ---
  TTL immediate expiry test passed!
  
  === TEST: Eviction does not use async (sync close fix) ===
  [MOCK] PREPARE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  [MOCK] PREPARE[2]: SELECT 2
  [MOCK] CLOSE[1]: SELECT 1
  [MOCK] SELECT_WITH_STMT[2]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 
  Async calls before eviction: 0, after: 0
  Eviction does NOT use async - OK (sync close)
  Sync close test passed!
  Error handling test completed!
  Final mock statistics:
  
  --- MOCK STATS ---
  Prepared: 10, Closed: 0, Open: 10
  Operations:
    1. PREPARE[1]: SELECT 1
    2. SELECT_WITH_STMT[1]
    3. PREPARE[2]: SELECT 2
    4. SELECT_WITH_STMT[2]
    5. PREPARE[3]: SELECT 3
    6. SELECT_WITH_STMT[3]
    7. PREPARE[4]: SELECT 4
    8. SELECT_WITH_STMT[4]
    9. PREPARE[5]: SELECT 5
    10. SELECT_WITH_STMT[5]
    11. PREPARE[6]: SELECT 6
    12. SELECT_WITH_STMT[6]
    13. PREPARE[7]: SELECT 7
    14. SELECT_WITH_STMT[7]
    15. PREPARE[8]: SELECT 8
    16. SELECT_WITH_STMT[8]
    17. PREPARE[9]: SELECT 9
    18. SELECT_WITH_STMT[9]
    19. PREPARE[10]: SELECT 10
    20. SELECT_WITH_STMT[10]
    21. SELECT_WITH_STMT[1]
    22. SELECT_WITH_STMT[2]
    23. SELECT_WITH_STMT[3]
    24. SELECT_WITH_STMT[4]
    25. SELECT_WITH_STMT[5]
    26. SELECT_WITH_STMT[6]
    27. SELECT_WITH_STMT[7]
    28. SELECT_WITH_STMT[8]
    29. SELECT_WITH_STMT[9]
    30. SELECT_WITH_STMT[10]
    31. SELECT_WITH_STMT[1]
    32. SELECT_WITH_STMT[2]
    33. SELECT_WITH_STMT[3]
    34. SELECT_WITH_STMT[4]
    35. SELECT_WITH_STMT[5]
    36. SELECT_WITH_STMT[6]
    37. SELECT_WITH_STMT[7]
    38. SELECT_WITH_STMT[8]
    39. SELECT_WITH_STMT[9]
    40. SELECT_WITH_STMT[10]
    41. SELECT_WITH_STMT[1]
    42. SELECT_WITH_STMT[2]
    43. SELECT_WITH_STMT[3]
    44. SELECT_WITH_STMT[4]
    45. SELECT_WITH_STMT[5]
    46. SELECT_WITH_STMT[6]
    47. SELECT_WITH_STMT[7]
    48. SELECT_WITH_STMT[8]
    49. SELECT_WITH_STMT[9]
    50. SELECT_WITH_STMT[10]
    51. SELECT_WITH_STMT[1]
    52. SELECT_WITH_STMT[2]
    53. SELECT_WITH_STMT[3]
    54. SELECT_WITH_STMT[4]
    55. SELECT_WITH_STMT[5]
    56. SELECT_WITH_STMT[6]
    57. SELECT_WITH_STMT[7]
    58. SELECT_WITH_STMT[8]
    59. SELECT_WITH_STMT[9]
    60. SELECT_WITH_STMT[10]
    61. SELECT_WITH_STMT[1]
    62. SELECT_WITH_STMT[2]
    63. SELECT_WITH_STMT[3]
    64. SELECT_WITH_STMT[4]
    65. SELECT_WITH_STMT[5]
    66. SELECT_WITH_STMT[6]
    67. SELECT_WITH_STMT[7]
    68. SELECT_WITH_STMT[8]
    69. SELECT_WITH_STMT[9]
    70. SELECT_WITH_STMT[10]
    71. SELECT_WITH_STMT[1]
    72. SELECT_WITH_STMT[2]
    73. SELECT_WITH_STMT[3]
    74. SELECT_WITH_STMT[4]
    75. SELECT_WITH_STMT[5]
    76. SELECT_WITH_STMT[6]
    77. SELECT_WITH_STMT[7]
    78. SELECT_WITH_STMT[8]
    79. SELECT_WITH_STMT[9]
    80. SELECT_WITH_STMT[10]
    81. SELECT_WITH_STMT[1]
    82. SELECT_WITH_STMT[2]
    83. SELECT_WITH_STMT[3]
    84. SELECT_WITH_STMT[4]
    85. SELECT_WITH_STMT[5]
    86. SELECT_WITH_STMT[6]
    87. SELECT_WITH_STMT[7]
    88. SELECT_WITH_STMT[8]
    89. SELECT_WITH_STMT[9]
    90. SELECT_WITH_STMT[10]
    91. SELECT_WITH_STMT[1]
    92. SELECT_WITH_STMT[2]
    93. SELECT_WITH_STMT[3]
    94. SELECT_WITH_STMT[4]
    95. SELECT_WITH_STMT[5]
    96. SELECT_WITH_STMT[6]
    97. SELECT_WITH_STMT[7]
    98. SELECT_WITH_STMT[8]
    99. SELECT_WITH_STMT[9]
    100. SELECT_WITH_STMT[10]
    101. SELECT_WITH_STMT[1]
    102. SELECT_WITH_STMT[2]
    103. SELECT_WITH_STMT[3]
    104. SELECT_WITH_STMT[4]
    105. SELECT_WITH_STMT[5]
    106. SELECT_WITH_STMT[6]
    107. SELECT_WITH_STMT[7]
    108. SELECT_WITH_STMT[8]
    109. SELECT_WITH_STMT[9]
    110. SELECT_WITH_STMT[10]
    111. SELECT_WITH_STMT[1]
    112. SELECT_WITH_STMT[2]
    113. SELECT_WITH_STMT[3]
    114. SELECT_WITH_STMT[4]
    115. SELECT_WITH_STMT[5]
    116. SELECT_WITH_STMT[6]
    117. SELECT_WITH_STMT[7]
    118. SELECT_WITH_STMT[8]
    119. SELECT_WITH_STMT[9]
    120. SELECT_WITH_STMT[10]
    121. SELECT_WITH_STMT[1]
    122. SELECT_WITH_STMT[2]
    123. SELECT_WITH_STMT[3]
    124. SELECT_WITH_STMT[4]
    125. SELECT_WITH_STMT[5]
    126. SELECT_WITH_STMT[6]
    127. SELECT_WITH_STMT[7]
    128. SELECT_WITH_STMT[8]
    129. SELECT_WITH_STMT[9]
    130. SELECT_WITH_STMT[10]
    131. SELECT_WITH_STMT[1]
    132. SELECT_WITH_STMT[2]
    133. SELECT_WITH_STMT[3]
    134. SELECT_WITH_STMT[4]
    135. SELECT_WITH_STMT[5]
    136. SELECT_WITH_STMT[6]
    137. SELECT_WITH_STMT[7]
    138. SELECT_WITH_STMT[8]
    139. SELECT_WITH_STMT[9]
    140. SELECT_WITH_STMT[10]
    141. SELECT_WITH_STMT[1]
    142. SELECT_WITH_STMT[2]
    143. SELECT_WITH_STMT[3]
    144. SELECT_WITH_STMT[4]
    145. SELECT_WITH_STMT[5]
    146. SELECT_WITH_STMT[6]
    147. SELECT_WITH_STMT[7]
    148. SELECT_WITH_STMT[8]
    149. SELECT_WITH_STMT[9]
    150. SELECT_WITH_STMT[10]
    151. SELECT_WITH_STMT[1]
    152. SELECT_WITH_STMT[2]
    153. SELECT_WITH_STMT[3]
    154. SELECT_WITH_STMT[4]
    155. SELECT_WITH_STMT[5]
    156. SELECT_WITH_STMT[6]
    157. SELECT_WITH_STMT[7]
    158. SELECT_WITH_STMT[8]
    159. SELECT_WITH_STMT[9]
    160. SELECT_WITH_STMT[10]
    161. SELECT_WITH_STMT[1]
    162. SELECT_WITH_STMT[2]
    163. SELECT_WITH_STMT[3]
    164. SELECT_WITH_STMT[4]
    165. SELECT_WITH_STMT[5]
    166. SELECT_WITH_STMT[6]
    167. SELECT_WITH_STMT[7]
    168. SELECT_WITH_STMT[8]
    169. SELECT_WITH_STMT[9]
    170. SELECT_WITH_STMT[10]
    171. SELECT_WITH_STMT[1]
    172. SELECT_WITH_STMT[2]
    173. SELECT_WITH_STMT[3]
    174. SELECT_WITH_STMT[4]
    175. SELECT_WITH_STMT[5]
    176. SELECT_WITH_STMT[6]
    177. SELECT_WITH_STMT[7]
    178. SELECT_WITH_STMT[8]
    179. SELECT_WITH_STMT[9]
    180. SELECT_WITH_STMT[10]
    181. SELECT_WITH_STMT[1]
    182. SELECT_WITH_STMT[2]
    183. SELECT_WITH_STMT[3]
    184. SELECT_WITH_STMT[4]
    185. SELECT_WITH_STMT[5]
    186. SELECT_WITH_STMT[6]
    187. SELECT_WITH_STMT[7]
    188. SELECT_WITH_STMT[8]
    189. SELECT_WITH_STMT[9]
    190. SELECT_WITH_STMT[10]
    191. SELECT_WITH_STMT[1]
    192. SELECT_WITH_STMT[2]
    193. SELECT_WITH_STMT[3]
    194. SELECT_WITH_STMT[4]
    195. SELECT_WITH_STMT[5]
    196. SELECT_WITH_STMT[6]
    197. SELECT_WITH_STMT[7]
    198. SELECT_WITH_STMT[8]
    199. SELECT_WITH_STMT[9]
    200. SELECT_WITH_STMT[10]
    201. SELECT_WITH_STMT[1]
    202. SELECT_WITH_STMT[2]
    203. SELECT_WITH_STMT[3]
    204. SELECT_WITH_STMT[4]
    205. SELECT_WITH_STMT[5]
    206. SELECT_WITH_STMT[6]
    207. SELECT_WITH_STMT[7]
    208. SELECT_WITH_STMT[8]
    209. SELECT_WITH_STMT[9]
    210. SELECT_WITH_STMT[10]
  ---
