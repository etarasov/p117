CREATE TABLE binary (
id INTEGER PRIMARY KEY AUTOINCREMENT,
shortName TEXT,
type1 TEXT,
type2 TEXT,
description TEXT);
CREATE TABLE binaryTrue (
id INTEGER PRIMARY KEY AUTOINCREMENT,
binaryId INTEGER,
value1 TEXT,
value2 TEXT );
CREATE TABLE pages (
id INTEGER PRIMARY KEY AUTOINCREMENT,
shortName text,
title text,
text text );
