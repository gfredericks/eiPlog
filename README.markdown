# Platform

Erlang and Webmachine
# (RESTful) Interface

All GET methods return JSON strings.
The two PUT requests used for creating new applications and events
should have a content-length of 0 and a content-type of
"text/plain" or "text/html"
I chose POST instead of PUT as the logging method because I don't
think the individual log messages are considered resources, but
rather a group of log messages under a common event should be
considered one resource. So logging is in effect an update to a
resource.
## Logging

**POST /logs/:appname/:eventname**
Content should be a JSON string like:

> {context: "some-context", details: "blah blah blah"}

Both of the following routes can have query string parameters of
"begin" and "end" formatted as "YYYYMMDDHHMMSS" to specify a
timeframe. Either, neither, or both may be present. Additionally, a
query-string value for "key" must be present to decrypt the logs.

**GET /logs/:appname/:eventname**

Possible query-string values:

-   "before": maximum "time" value, formatted "YYYYMMDDHHMMSS"
-   "after": minimum "time" value, formatted "YYYYMMDDHHMMSS"
-   "context": a specified value for the context - context will
    only be given in the return objects if it is **not**&nbsp;specified
    in the query string
-   "key": **required**&nbsp;- the key for decrypting the logs

> [{time: "some-time", details: details-string}, ...]
>
> or
>
> [{context: "some-context", time: "some-time", details: details-string}, ...]
>
> (see below for the difference)

## **Managing Applications**

**GET /applications**
 
> ["appname1", "appname2", ...]

**PUT /applications/:appname**

**DELETE /applications/:appname**

Internally marks application and its events as deleted and moves
all its logs to the archive table. 

This **cannot** be undone through
the API (it **can** be undone through direct SQL calls to the database,
but that should not be done lightly, as it may create a naming conflict
if a new application with the same name has been created).

## Managing Event Types

**GET /events/:appname**

> ["evname1", "evname2", ...]

**PUT /events/:appname/:eventname**

**DELETE /events/:appname/:eventname**

Internally marks event as deleted and moves all its logs to the
archive table

This **cannot** be undone through
the API (it **can** be undone through direct SQL calls to the database,
but that should not be done lightly, as it may create a naming conflict
if a new event with the same name and application has been created).

# Database Implementation

In the Indexes sections, [:col1, :col2, ...] specifies one index on
several columns
## MySQL tables:

### applications

+ **id** integer, key
+ **name** string
+ **created\_at** datetime
+ **deleted\_at** datetime (null allowed)

#### Indexes

[:deleted\_at, :name]
### events

+ **id** integer, key
+ **name** string
+ **application\_id** integer
+ **created\_at** datetime
+ **deleted\_at** datetime (null allowed)

#### Indexes

[:deleted\_at, :application\_id, :name]
### logs

+ **id** integer, key
+ **event\_id** integer
+ **time** datetime
+ **context** string
+ **details** text

#### Indexes

[:event\_id, :context, :time]
[:event\_id, :time]

### deleted\_logs

Identical to the \`logs\` table
# Security

The \`details\` column of the logs/deleted\_logs table will be
encrypted using the mysql AES\_ENCRYPT() function. The encryption
password will be passed as an argument to the application on
startup, and will be required by any application sending a GET
logging request.&nbsp;
# Usage

Applications and events should probably be added manually when a
program is configured to access the server. I can't think of any
reason why a program should be programmatically adding or removing
applications and events from the server. Deleted apps and events
are never lost, but they are no longer accessible through the API.
The apps and event records in the apps/events tables are given a
non-null "deleted\_at" value (which effectively makes them
invisible through the API), and all of the related entries in the
logs table are moved to deleted\_logs (this is only done for
efficiency - theoretically they could be left in the logs table
with no malfunction).
