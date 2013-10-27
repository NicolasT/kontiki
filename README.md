kontiki
=======

An implementation of the Raft consensus protocol. Please check 
[original paper](https://ramcloud.stanford.edu/wiki/download/attachments/11370504/raft.pdf)
for details of the protocol

[![Build Status](https://travis-ci.org/NicolasT/kontiki.png?branch=master)](https://travis-ci.org/NicolasT/kontiki)

Hacking
-------

This project is built using [cabal](http://www.haskell.org/cabal/). It is recommended
that you use *cabal >= 1.18* as it introduces an excellent sandboxing capability
which deprecates [cabal-dev](http://hackage.haskell.org/package/cabal-dev).

### Building

If you use the *cabal sandboxes* first run:
``` bash
$ cabal sandbox init
```

This will initialize an empty sandbox under *.cabal-sandbox*. From this point forward,
all dependencies will be installed locally into the sandbox and will not affect your
globally installed packages.

First install all the required dependencies:
``` bash
$ cabal install --only-dependencies --enable-tests --enable-benchmarks --enable-library-coverage
```

Then you'll need to run the __configure__ command:
``` bash
$ cabal configure --enable-tests --enable-benchmarks --enable-library-coverage
```

After that you should be able to build and run the tests:

``` bash
$ cabal build && cabal test
``` 

### Code coverage

Running __cabal test__ with __--enable-library-coverage__ should write
the test coverage report under __dist/hpc__.

### Haddock

Simply run:
``` bash
$ cabal haddock
```

This should generate the documentation under __dist/doc__.

### Available demos

The __bin__ directory contains at least two demos.

#### udp.hs

This is a demo of an in-memory __kontiki__ cluster running with the use
of [conduit](http://hackage.haskell.org/package/conduit). You can run up to 3 nodes
and you will be able to see the logs output by them.

Simply run the following in separate terminals (once you have built the udp demo executable):
``` bash
$ kontiki-udp <nodeid>
```

Where *nodeid = ["node0", "node1", "node2", "node3"]*. You should be able to follow 
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
