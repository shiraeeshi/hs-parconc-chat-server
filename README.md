# parconc-chat-server

# An example chat server from the book "Parallel and Concurrent Programming in Haskell"


You can find it in [Chapter 12](https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch12.html#sec_chat).

Added admin operations available to the "admin" user:
 - `/pause`
 - `/resume`
 - `/pauseKicks`
 - `/resumeKicks`
 
 How to make two users kick each other:
  - connect as "admin"
  - connect as "alice"
  - connect as "bob"
  - as "admin", send a `/pauseKicks` command to the server
  - as "alice", send `/kick bob`
  - as "bob", send `/kick alice`
  - as "admin", send `/resumeKicks`
  
  Another scenario showing that the outcome depends on the order of admin's actions:
  - connect as "admin"
  - connect as "alice"
  - connect as "bob"
  - as "admin", send `/pause` and `/pauseKicks` commands to the server in any order
  - as "alice", send `/kick bob`
  - as "bob", send `/kick alice`
  - as "admin", if you send `/resumeKicks` first, and then `/resume`, only one of two users will be kicked (but if you send `/resume` first, and `/resumeKicks` second, both will be kicked)
