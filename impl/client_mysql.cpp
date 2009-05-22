/* client2.c */

#include <stdio.h>
#include <mysql/mysql.h>

#define def_host_name  NULL /* host to connect to (default = localhost) */
#define def_user_name  "root" /* user name (default = your login name) */
#define def_password   NULL /* password (default = none) */
#define def_db_name    "test" /* database to use (default = none) */

MYSQL  *conn;        /* pointer to connection handler */

int
main (int argc, char *argv[])
{
  conn = mysql_init (NULL);
  if (conn == NULL)
  {
    fprintf (stderr, "mysql_init() failed (probably out of memory)\n");
    return 1;
  }
  if (mysql_real_connect (
        conn,          /* pointer to connection handler */
        def_host_name, /* host to connect to */
        def_user_name, /* user name */
        def_password,  /* password */
        def_db_name,   /* database to use */
        0,             /* port (use default) */
        NULL,          /* socket (use default) */
        0)             /* flags (none) */
      == NULL)
  {
    fprintf (stderr, "mysql_real_connect() failed:\nError %u (%s)\n",
              mysql_errno (conn), mysql_error (conn));
    return 1;
  }
    MYSQL_RES *res_set;

  if (mysql_query (conn, "SELECT * FROM test") != 0)
  {
    fprintf(stderr,"mysql_query() failed");
  }
  else
  {
    res_set = mysql_store_result (conn);  /* generate result set */
    if (res_set == NULL)
    {
        fprintf(stderr,"mysql_store_result() failed");
    }
    else
    {
      /* process result set, then deallocate it */
      MYSQL_ROW row = NULL;

      while ((row = mysql_fetch_row (res_set)) != NULL)
      {
        printf(".");
        /* do something with row contents */
      }

      mysql_free_result (res_set);
    }
  }

  mysql_close (conn);
  return 0;
}
