{-# LANGUAGE OverloadedStrings #-}


module JQuery.Deferred where
----
---- Deferred Object
----

---- Assume JQuery > 1.8

import Fay.Text (Text, fromString, pack, unpack)
import FFI

data Deferred t = Deferred t
data DeferredState = Pending | Resolved | Rejected

deferred :: Defined (Deferred t -> Fay ()) -> Fay (Deferred t)
deferred (Defined beforeStart) = (ffi "jQuery.Deferred(%1)" :: (Deferred t -> Fay ()) -> Fay (Deferred t)) beforeStart
deferred Undefined = ffi "jQuery.Deferred()" :: Fay (Deferred t)

always :: Fay ()
       -> Deferred t
       -> Fay (Deferred t)
always = ffi "%2.always(%1)"

done :: (t -> Fay ())
     -> Deferred t
     -> Fay (Deferred t)
done = ffi "%2.done(%1)"

fail_ :: Fay ()
     -> Deferred t
     -> Fay (Deferred t)
fail_ = ffi "%2.fail(%1)"

--notify :: t -> Deferred t -> Fay ()
--notify = ffi "%2.notify(%1)"

-- TODO - store *this* into Deferred
-- notifyWith :: c -> t -> Deferred t -> Fay ()
-- notifyWith = ffi "%3.notify(%1,%2)"

reject :: t -> Deferred t -> Fay (Deferred t)
reject = ffi "%2.reject(%1)"

-- TODO - store *this* into Deferred
rejectWith :: c -> t -> Deferred t -> Fay ()
rejectWith = ffi "%3.rejectWith(%1,%2)"

resolve :: t -> Deferred t -> Fay ()
resolve = ffi "%2.resolve(%1)"

-- TODO - store *this* into Deferred
--resolveWith :: c -> t -> Deferred t -> Fay ()
--resolveWith = ffi "%3.resolveWith(%1,%2)"

-- TODO - Promise not yet implemented. The hidden methods on Promise will fail at runtime ATM
-- TODO - Promise with target untested
promise_ :: Defined target -> Deferred t -> Fay (Deferred t)
promise_ (Defined targ) def = (ffi "%2.promise(%1)" :: target -> Deferred t -> Fay (Deferred t)) targ def
promise_ Undefined def = (ffi "%1.promise()" :: Deferred t -> Fay (Deferred t)) def

--
-- Tested up to here
--

deferredStateToText :: DeferredState -> Text
deferredStateToText ds = case ds of
                           Pending -> pack "pending" :: Text
                           Rejected -> pack "rejected" :: Text
                           Resolved -> pack "resolved" :: Text

-- TODO - the case statement here with the strings as Text does not work, it just chooses the first value
--        I haven't been able to replicate the problem outside of this file though, so I've left it here
state :: Deferred t -> Fay DeferredState
state def = do
  stateStr <- (ffi "%1.state()" :: Deferred t -> Fay Text) def
--  (ffi "window.foo = %1" :: Text -> Fay ()) stateStr
--  (ffi "window.bar = %1" :: Text -> Fay ()) resolvedStr
  return $ case unpack stateStr of
    "pending" -> Pending
    "resolved" -> Resolved
    "rejected" -> Rejected

--  case stateStr of
--    rejectedStr -> (ffi "console.log(\"rejected\")" :: Fay ())
--    resolvedStr -> (ffi "console.log(\"resolved\")" :: Fay ())
--    pendingStr -> (ffi "console.log(\"pending\")" :: Fay ())
--  return $ case stateStr of
--    pendingStr -> Pending
--    resolvedStr -> Resolved
--    rejectedStr -> Rejected
--  where pendingStr = pack "pending" :: Text
--        resolvedStr = pack "resolved" :: Text
--        rejectedStr = pack "rejected" :: Text

then_ :: Defined (t -> Fay s)
      -> Defined (Fay ())
      -> Defined (Fay ())
      -> Deferred t
      -> Fay (Deferred s)
then_ = ffi "%4.then(%1,%2,%3)"

-- Need heterogeneous array to implement this properly
when_ :: [Deferred t] -> Fay (Deferred [t])
when_ = ffi "jQuery.when.apply(jQuery,%1).then(function() { return Array.prototype.slice.call(arguments).sort() })"

