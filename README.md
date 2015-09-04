Kontiki
=======
An implementation of the Raft consensus protocol. See the
[original paper](https://ramcloud.stanford.edu/wiki/download/attachments/11370504/raft.pdf)
for details about the protocol.

[![Build Status](https://travis-ci.org/NicolasT/kontiki.png?branch=master)](https://travis-ci.org/NicolasT/kontiki)

Hacking
-------
This project is developed using [stack](https://github.com/commercialhaskell/stack), based on an LTS snapshot of [Stackage](http://www.stackage.org). See the `stack.yaml` file for more details.

Plain `cabal` can be used as well.

### Building
Simply run `stack build --pedantic`.

### Tests
Run `stack build --test` to run the test-suite.

### Haddock
Run `stack build --haddock` to build the Haddock-rendered API documentation.

### Available demos

The __bin__ directory contains at least two demos.

#### udp.hs

This is a demo of an in-memory __kontiki__ cluster running with the use
of [conduit](http://hackage.haskell.org/package/conduit). You can run up to 3 nodes
and you will be able to see the logs output by them.

Simply run the following in separate terminals
``` bash
$ stack exec kontiki-udp <nodeid>
```

where *nodeid = ["node0", "node1", "node2"]*. You should be able to follow 
the logs output by each instance.

Example output:
``` bash
Reset election timeout: 15324284
Awaiting event
Got event: EMessage "node0" (MRequestVote (RequestVote {rvTerm = Term 1, rvCandidateId = "node0", rvLastLogIndex = Index {unIndex = 0}, rvLastLogTerm = Term 0}))
Input state: WrapState (Follower (FollowerState {_fCurrentTerm = Term 0, _fVotedFor = Nothing}))
Output state: WrapState (Follower (FollowerState {_fCurrentTerm = Term 1, _fVotedFor = Just "node0"}))
Log: Received RequestVote for newer term, bumping
Log: Granting vote
CSend: "node0" -> MRequestVoteResponse (RequestVoteResponse {rvrTerm = Term 1, rvrVoteGranted = True})
Reset election timeout: 14611709
Log: []
Awaiting event
Got event: EMessage "node0" (MAppendEntries (AppendEntries {aeTerm = Term 1, aeLeaderId = "node0", aePrevLogIndex = Index {unIndex = 0}, aePrevLogTerm = Term 0, aeEntries = [], aeCommitIndex = Index {unIndex = 0}}))
Input state: WrapState (Follower (FollowerState {_fCurrentTerm = Term 1, _fVotedFor = Just "node0"}))
Output state: WrapState (Follower (FollowerState {_fCurrentTerm = Term 1, _fVotedFor = Just "node0"}))
Reset election timeout: 18456774
CSend: "node0" -> MAppendEntriesResponse (AppendEntriesResponse {aerTerm = Term 1, aerSuccess = True, aerLastIndex = Index {unIndex = 0}})
Log: []
Awaiting event
Got event: EMessage "node0" (MAppendEntries (AppendEntries {aeTerm = Term 1, aeLeaderId = "node0", aePrevLogIndex = Index {unIndex = 0}, aePrevLogTerm = Term 0, aeEntries = [Entry {eIndex = Index {unIndex = 1}, eTerm = Term 1, eValue = ("node0",0)},Entry {eIndex = Index {unIndex = 2}, eTerm = Term 1, eValue = ("node0",1)}], aeCommitIndex = Index {unIndex = 0}}))
Input state: WrapState (Follower (FollowerState {_fCurrentTerm = Term 1, _fVotedFor = Just "node0"}))
Output state: WrapState (Follower (FollowerState {_fCurrentTerm = Term 1, _fVotedFor = Just "node0"}))
Reset election timeout: 14154414
Truncate: Index {unIndex = 0}
Log entries: [Entry {eIndex = Index {unIndex = 1}, eTerm = Term 1, eValue = ("node0",0)},Entry {eIndex = Index {unIndex = 2}, eTerm = Term 1, eValue = ("node0",1)}]
CSend: "node0" -> MAppendEntriesResponse (AppendEntriesResponse {aerTerm = Term 1, aerSuccess = True, aerLastIndex = Index {unIndex = 2}})
Log: [(1,("node0",0)),(2,("node0",1))]
Awaiting event
Got event: EMessage "node0" (MAppendEntries (AppendEntries {aeTerm = Term 1, aeLeaderId = "node0", aePrevLogIndex = Index {unIndex = 2}, aePrevLogTerm = Term 1, aeEntries = [Entry {eIndex = Index {unIndex = 3}, eTerm = Term 1, eValue = ("node0",2)},Entry {eIndex = Index {unIndex = 4}, eTerm = Term 1, eValue = ("node0",3)}], aeCommitIndex = Index {unIndex = 2}}))
Input state: WrapState (Follower (FollowerState {_fCurrentTerm = Term 1, _fVotedFor = Just "node0"}))
Output state: WrapState (Follower (FollowerState {_fCurrentTerm = Term 1, _fVotedFor = Just "node0"}))
Reset election timeout: 17577659
Truncate: Index {unIndex = 2}
Log entries: [Entry {eIndex = Index {unIndex = 3}, eTerm = Term 1, eValue = ("node0",2)},Entry {eIndex = Index {unIndex = 4}, eTerm = Term 1, eValue = ("node0",3)}]
CSend: "node0" -> MAppendEntriesResponse (AppendEntriesResponse {aerTerm = Term 1, aerSuccess = True, aerLastIndex = Index {unIndex = 4}})
Log: [(1,("node0",0)),(2,("node0",1)),(3,("node0",2)),(4,("node0",3))]
Awaiting event
[snip]
```

Contributing
------------

Have a look at the issues and try to implement the proposed features.
