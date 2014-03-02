PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE binary (
id INTEGER PRIMARY KEY AUTOINCREMENT,
shortName TEXT,
type1 TEXT,
type2 TEXT,
description TEXT);
INSERT INTO "binary" VALUES(1,'pred1',NULL,NULL,NULL);
INSERT INTO "binary" VALUES(2,'pred2',NULL,NULL,NULL);
CREATE TABLE binaryTrue (
id INTEGER PRIMARY KEY AUTOINCREMENT,
binaryId INTEGER,
value1 TEXT,
value2 TEXT );
INSERT INTO "binaryTrue" VALUES(1,1,'-1','1');
INSERT INTO "binaryTrue" VALUES(2,1,'1','2');
INSERT INTO "binaryTrue" VALUES(3,1,'1','3');
INSERT INTO "binaryTrue" VALUES(4,1,'-1','4');
INSERT INTO "binaryTrue" VALUES(5,1,'-1','5');
INSERT INTO "binaryTrue" VALUES(6,1,'4','6');
INSERT INTO "binaryTrue" VALUES(7,1,'-1','7');
INSERT INTO "binaryTrue" VALUES(8,1,'4','8');
INSERT INTO "binaryTrue" VALUES(9,1,'6','9');
INSERT INTO "binaryTrue" VALUES(10,1,'1','10');
INSERT INTO "binaryTrue" VALUES(11,1,'3','11');
INSERT INTO "binaryTrue" VALUES(12,2,'-1','1');
INSERT INTO "binaryTrue" VALUES(13,2,'1','10');
INSERT INTO "binaryTrue" VALUES(14,1,'10','8');
CREATE TABLE pages (
id INTEGER PRIMARY KEY AUTOINCREMENT,
title text,
text text );
INSERT INTO "pages" VALUES(1,'Node1','<p>Test text for node1.
<p>Test text test text test text.');
INSERT INTO "pages" VALUES(2,'Node2','Node2 test text.

<i>Test</i>');
INSERT INTO "pages" VALUES(3,'Node3','Please write page text
<p>

<i>asdfasdf</i>

<ul>
<li>test</li>
<li>test2</li>
</ul>asdfasdf
');
INSERT INTO "pages" VALUES(4,'Node4','Please write page text');
INSERT INTO "pages" VALUES(5,'Page5','Please write page text');
INSERT INTO "pages" VALUES(6,'Node6','Please write page text');
INSERT INTO "pages" VALUES(7,'Page7','Please write page text');
INSERT INTO "pages" VALUES(8,'Node8','Please write page text');
INSERT INTO "pages" VALUES(9,'Page9','Please write page text


<p>
asdfsaf
<p>
asdfasfd');
INSERT INTO "pages" VALUES(10,'Node10','Please write page text<br>

Test test for node2');
INSERT INTO "pages" VALUES(11,'Node11','Please write page text');
DELETE FROM sqlite_sequence;
INSERT INTO "sqlite_sequence" VALUES('pages',11);
INSERT INTO "sqlite_sequence" VALUES('binary',2);
INSERT INTO "sqlite_sequence" VALUES('binaryTrue',14);
COMMIT;
