Goofing around with pathfinding
====

I just wanted to try out having a grid of Erlang processes linked to each other, and then passing a target coordinate through the grid.

I hand-rolled the loops because I couldn't be bothered to stand up proper OTP behaviours.

e.g. Find a path from 1,1 to 1,3
```
┌───────────────┐
│1,3   2,3   3,3│
├──────────┐    │
│1,2   2,2 │ 3,2│
│    ──────┘    │
│1,1   2,1   3,1│
└───────────────┘
```

How it works
----
1. Setup a "grid" of prcesses that see each other (by Pid or by registered name).
2. Each process has a receive loop and a state
3. Starting grid receives a `start` signal
4. The `start` signal is propogated through neighbours
5. Target grid cell declares when it's been found

### TODO
- Use "sonar": only go in the direction that points to the goal until a dead end, then "echo" back to the first process with "live" neighbours and try the next route.
- Add this to the websocket animation project.
- track the successful path
