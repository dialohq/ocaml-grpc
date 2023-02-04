# Contributing notes

## Server

### Gluten's runloop

This part is tricky.
`Gluten` (`eio`) runs the main reader/writer runloop.
When a handler is invoked, it happens as a result of one of the
read operatiosn. Effectively it means that if a handler blocks,
the whole thread is blocked and no further reads are scheduled.

H2 contains the code for scheduling but the actual read/write
operations are performed by gluten.
 
