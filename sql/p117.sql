PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE binary (
id INTEGER PRIMARY KEY AUTOINCREMENT,
shortName TEXT,
type1 TEXT,
type2 TEXT,
description TEXT);
INSERT INTO "binary" VALUES(1,'About',NULL,NULL,NULL);
INSERT INTO "binary" VALUES(2,'Get started',NULL,NULL,NULL);
INSERT INTO "binary" VALUES(3,'Development',NULL,NULL,NULL);
CREATE TABLE binaryTrue (
id INTEGER PRIMARY KEY AUTOINCREMENT,
binaryId INTEGER,
value1 TEXT,
value2 TEXT,
pos INTEGER DEFAULT -1);
INSERT INTO "binaryTrue" VALUES(1,1,'-1','1', -1);
INSERT INTO "binaryTrue" VALUES(2,1,'-1','2', -1);
INSERT INTO "binaryTrue" VALUES(10,1,'-1','10', -1);
INSERT INTO "binaryTrue" VALUES(14,1,'10','8', -1);
INSERT INTO "binaryTrue" VALUES(15,1,'10','12', -1);
INSERT INTO "binaryTrue" VALUES(16,1,'10','13', -1);
INSERT INTO "binaryTrue" VALUES(17,1,'12','14', -1);
INSERT INTO "binaryTrue" VALUES(18,1,'10','14', -1);
INSERT INTO "binaryTrue" VALUES(19,2,'-1','2', -1);
INSERT INTO "binaryTrue" VALUES(20,2,'-1','4', -1);
INSERT INTO "binaryTrue" VALUES(21,3,'-1','4', -1);
INSERT INTO "binaryTrue" VALUES(22,3,'-1','15', -1);
INSERT INTO "binaryTrue" VALUES(23,3,'-1','16', -1);
INSERT INTO "binaryTrue" VALUES(24,1,'10','17', -1);
CREATE TABLE pages (
id INTEGER PRIMARY KEY AUTOINCREMENT,
title text,
text text );
INSERT INTO "pages" VALUES(1,'What is P117','<p>P117 is a notes management application. It stores a set of "pages" and allows to edit them. Pages are linked together explicitly and link stucture is displayed in separate tree view.
<p>In some way it is like wiki software, because pages can be edited in place. Unlike conventional wiki systems, P117 provides different approach to maintaining logical structure of pages.
<p>Another related thing is mindmaps. Like mindmaps, P117 is designed to maintain logical links between pages in convenient way.');
INSERT INTO "pages" VALUES(2,'How pages are stored','<p>Basically, pages are stored in database as an unordered set.

<p>There are <b>structure predicates</b> which makes a structure of that pages.

<p>They are like predicates in Prolog language. There''re some ''facts'' in database which connect pages together like facts in Prolog which connects Prolog terms:
<pre>
About(-1, ''What is P117'').
About(-1, ''Similar things'').
About(''Similar things'', ''Wiki software'').
About(''Wiki software'', ''Confluence'').
About(''Similar things'', ''Confluence).
...
</pre>

<p>There could be many predicates that form different structures for the same set of pages. All predicates are listed in the combobox which is above the tree widget.

<p>A single page can appear at different places like "Confluence" page, which is attached to both "Wiki software" and "Similar things".

<p>This current page is attached to both "About" and "Get started" structures. It often makes sense to link a page to many places because it is related logically.');
INSERT INTO "pages" VALUES(3,'Technologies','');
INSERT INTO "pages" VALUES(4,'How to build and run P117','<ol>
<li>Install <a href="http://www.haskell.org/platform/">Haskell Platform</a><li>Install sqlite binary and dynamic libraries. For Fedora:
<pre>
yum install sqlite-devel sqlite
</pre>
<li>Clone source repository
<pre>
> git clone https://github.com/etarasov/p117.git
> cd p117
</pre>
<li>Install project dependencies and build the application in sandbox:
<pre>
cabal sandbox init
cabal install --only-dependencies
cabal build
</pre>
<li>Initialize database:
<pre>
> cd sql
> sqlite3 test.db < p117.sql
</pre>
<li> Run the project from sandbox:
<pre
> ./dist/build/p117/p117
</pre>
<li> Connect to 9000 port using browser:
<pre>
> firefox localhost:9000
</pre>
</ol>');
INSERT INTO "pages" VALUES(5,'Page5','Please write page text');
INSERT INTO "pages" VALUES(6,'Node6','Please write page text');
INSERT INTO "pages" VALUES(7,'Page7','Please write page text');
INSERT INTO "pages" VALUES(8,'Mindmaps','<p>Mindmaps allows to express sophisticated logical structures.
However, it''s usually hard to attach big text to mindmap nodes.
<p>Number and connectivity of logical links are restricted by graphical representation of the graph.');
INSERT INTO "pages" VALUES(9,'Page9','Please write page text


<p>
asdfsaf
<p>
asdfasfd');
INSERT INTO "pages" VALUES(10,'Similar things','');
INSERT INTO "pages" VALUES(11,'Dynatree','<a href="https://code.google.com/p/dynatree/">https://code.google.com/p/dynatree/</a>');
INSERT INTO "pages" VALUES(12,'Wiki software','<p>In wiki software, logical structure of information is usually implemented using ''wiki links''. 

<p>This approach makes it hard to keep in mind designed structure of documentation. Refactoring of wiki structure is often a tricky task, because the whole picture is behind the scene.

<p>However, there are plugins which provide a tree widget with a hierarchical structure of pages.');
INSERT INTO "pages" VALUES(13,'Knowledge bases','<p>Knowledge bases deal with structured information.

<p>They are designed for automated reasoning and information extraction. That imposes fixed structure on content data and logical links.

<p>P117 doesn''t imply any automated processing of stored data. Its model is just convenient way to store human readable data and logical links.');
INSERT INTO "pages" VALUES(14,'Confluence','<p><a href="https://www.atlassian.com/software/confluence/">Atlassian Confluence</a> is a great wiki software.

<p>Unlike other wikis, it has explicit hierarchical structure of pages and convenient tree view, which makes easier to maintain documentation.

<p>However, the software is quite expensive for more than 10 users by license and maintenance cost. It aims to enterprise environments and has a lot of auxiliary features. It requires very fast hardware and a lot of RAM to operate.');
INSERT INTO "pages" VALUES(15,'How to update p117.sql','<pre>
$ sqlite3 test.db .dump > tmp.sql
$ mv tmp.sql p117.sql
</pre>');
INSERT INTO "pages" VALUES(16,'Url parameters','<ul>
<li>
<b>Path1</b> - path in the first tree (left)
</li>
<li>
<b>PredicateType1</b> - what predicate is selected for the first tree. <br>Possible values: "custom", "allpages".
</li>
<li>
<b>CustomPredicate1</b> - what custom predicate is selected for the first tree. <br>Only if PredicateType1 == "custom"
</li>
<li>
<b>Path2</b> - path in the second tree (right)
</li>
<li>
<b>PredicateType2</b> - what predicate is selected for the second tree.
</li>
<li>
<b>CustomPredicate2</b> - what custom predicate is selected for the first tree. <br>Only if PredicateType2 == "custom"
</li>
</ul>
');
INSERT INTO "pages" VALUES(17,'Outliners','<p>Outliner is a tool for organizing a hierarchy of items:<br>
<a href="https://en.wikipedia.org/wiki/Outliner">https://en.wikipedia.org/wiki/Outliner</a>
<p>List of some outliners are here:<br>
<a href="http://www.marktaw.com/reviews/Outliners.html">http://www.marktaw.com/reviews/Outliners.html</a>
<p>P117 is like outliner in that is has hierarchy of items and provide tree user interface control to manage them. However, hierarchy in P117 is arbitrary graph(with some technical restrictions in early versions), not just tree. On top of that, the same set of items can be used in many different graphs.');
DELETE FROM sqlite_sequence;
INSERT INTO "sqlite_sequence" VALUES('pages',17);
INSERT INTO "sqlite_sequence" VALUES('binary',2);
INSERT INTO "sqlite_sequence" VALUES('binaryTrue',24);
COMMIT;
