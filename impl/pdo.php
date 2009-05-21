<?php
$dsn = 'sqlite::memory:';

try {
    $db = new PDO($dsn);
}
catch (PDOException $e)
{
    die('Connection failed: ' . $e->getMessage());
}

$db->exec("CREATE TABLE test (x INT,z TEXT)");
$db->exec("INSERT INTO test VALUES (1,'one')");
$db->exec("INSERT INTO test VALUES (2,'two')");
$db->exec("INSERT INTO test VALUES (3,'three')");

/* Execute a prepared statement by passing an array of values */
$sth = $db->prepare('SELECT * FROM test WHERE x < :num', array(PDO::ATTR_CURSOR => PDO::CURSOR_FWDONLY));
$sth->execute(array(':num' => 100));
foreach ($sth->fetchAll() as $row)
{
  print $row['x'] . "\t" . $row['z'] . "\n";
}
print "\n";
$sth->execute(array('num' => 3));
foreach ($sth->fetchAll() as $row)
{
  print $row['x'] . "\t" . $row['z'] . "\n";
}
?>
