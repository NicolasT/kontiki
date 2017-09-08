module Kontiki.Raft.Internal.Leader (
      onRequestVoteRequest
    , onRequestVoteResponse
    , onAppendEntriesRequest
    , onAppendEntriesResponse
    , onElectionTimeout
    , onHeartbeatTimeout
    ) where

import GHC.Stack (HasCallStack)

onRequestVoteRequest :: HasCallStack
                     => a
                     -> b
onRequestVoteRequest _ = error "Not implemented"

onRequestVoteResponse :: HasCallStack
                      => a
                      -> b
onRequestVoteResponse _ = error "Not implemented"

onAppendEntriesRequest :: HasCallStack
                       => a
                       -> b
onAppendEntriesRequest _ = error "Not implemented"

onAppendEntriesResponse :: HasCallStack
                        => a
                        -> b
onAppendEntriesResponse _ = error "Not implemented"

onElectionTimeout :: HasCallStack => a
onElectionTimeout = error "Not implemented"

onHeartbeatTimeout :: HasCallStack => a
onHeartbeatTimeout = error "Not implemented"
