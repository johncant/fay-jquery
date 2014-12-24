{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

-- Compile with: fay --package fay-text,fay-jquery test.hs --pretty

module Test (main) where

import Fay.Text (Text, fromString)
import JQuery
import JQuery.Deferred
import Prelude hiding (div)
import qualified Fay.Text as T
import FFI

myMapM            :: (a -> Fay b) -> [a] -> Fay [b]
myMapM f as       =  mySequence (map f as)

mySequence    :: [Fay a] -> Fay [a]
mySequence ms = let k m m' = do { x <- m; xs <- m'; return (x:xs) } in
                foldr k (return []) ms

tableData :: [[Text]]
tableData = [ ["S", "Soprano"]
            , ["A", "Alto"]
            , ["T", "Tenor"]
            , ["B", "Bass"]
            ]

buildCell :: Text -> Fay JQuery
buildCell cellData = do
    cell <- select "<td/>"
    setText cellData cell

buildRow :: [Text] -> Fay JQuery
buildRow rowData = do
    row <- select "<tr/>"
    myMapM (buildCell >=> appendToJQuery row) rowData
    return row

buildTable :: [[Text]] -> Fay JQuery
buildTable rowsData = do
    table <- select "<table/>"
    myMapM (buildRow >=> appendToJQuery table) rowsData
    return table

main :: Fay ()
main = ready $ do
    div <- select "#replacementDiv"
    table <- buildTable tableData
    replaceWithJQuery table div
    addZebraStriping table

    testAnimations
    testAjax
    testDeferred

    return ()

testAnimations :: Fay ()
testAnimations = do
  animationsTest <- select "#animationsTest"
  container <- select "<div/>" >>= appendTo animationsTest
  thing <- select "<div>Hello</div>" >>= appendTo container
  select "<input type='button' value='Hide slow'>" >>= click (const $ hide Slow thing >> return ()) >>= appendTo container
  select "<input type='button' value='Show instantly'>" >>= click (const $ jshow Instantly thing >> return ()) >>= appendTo container
  select "<input type='button' value='Toggle 100'>" >>= click (const $ toggle (Speed 100) thing >> return ()) >>= appendTo container
  select "<input type='button' value='Chained'>" >>= click (const $ runAnimation $ chainedAnimation thing) >>= appendTo container
  select ("<div>" `T.append` "Thunk" `T.append` "</div>") >>= appendTo animationsTest
  return ()

    where
      chainedAnimation el = chainAnims [speed Fast (anim Toggle el), speed Slow (anim Toggle el), anim FadeOut el, anim FadeIn el]

data AjaxTest = AjaxTest Double Double

testAjax :: Fay ()
testAjax = do
  ajax "http://www.example.com" putStrLn (\_ _ _ -> return ())
  ajaxPost "http://www.example.com" (AjaxTest 1 2) putStrLn (\_ _ _ -> return ())
  ajaxPostParam "http://www.example.com" "foo" (AjaxTest 1 2) putStrLn (\_ _ _ -> return ())

addZebraStriping :: JQuery -> Fay JQuery
addZebraStriping table = do
    evenRows <- findSelector "tr:even" table
    addClass "even" evenRows
    oddRows <- findSelector "tr:odd" table
    addClass "odd" oddRows

-- Set up minimal test DSL and results box
data TestResultBox = TestResultBox { selector :: JQuery }

passTest :: TestResultBox -> Fay ()
passTest r = do
  setHtml "<span class=success>Pass</span>" $ selector r
  return ()

failTest :: TestResultBox -> Fay ()
failTest r = do
  setHtml "<span class=failure>Fail</span>" $ selector r
  return ()

newResultBox :: Text -> Fay TestResultBox
newResultBox name = do
  table <- select "#deferredTest"
  row <- select ("<tr><td>" `T.append` name `T.append` "</td></tr>") >>= appendTo table
  box <- select "<td><span class=pending>Pending</span></td>" >>= appendTo row
  return $ TestResultBox box

-- Not for use in actual lib...
isDeferred :: Deferred a -> Fay Bool
isDeferred = ffi "!!(%1.then && %1.always && %1.done && %1.fail)"

-- Deferred object tests

testDeferredConstructor :: Fay ()
testDeferredConstructor = do
  out <- newResultBox "Deferred constructor with no args"
  def <- deferred Undefined :: Fay (Deferred Text)
  resultIsDeferred <- isDeferred def
  case resultIsDeferred of
    True -> passTest out
    _ -> failTest out
  return ()

testDeferredConstructorBefore :: Fay ()
testDeferredConstructorBefore = do
  out1 <- newResultBox "Deferred constructor calls func arg with Deferred"
  out2 <- newResultBox "Deferred constructor with func arg returns Deferred"
  def <- ((deferred $ Defined (\d -> do
    argumentIsDeferred <- isDeferred d
    case argumentIsDeferred of
      True -> passTest out1
      _ -> failTest out1)) :: Fay (Deferred Text))
  resultIsDeferred <- isDeferred def
  case resultIsDeferred of
    True -> passTest out2
    _ -> failTest out2
  return ()

testDeferredAlways :: Fay ()
testDeferredAlways = do
  called <- newResultBox "Deferred always callback triggered"
  correctRV <- newResultBox "Deferred always returns Deferred"
  def <- (deferred Undefined) :: Fay (Deferred Text)
  def1 <- always (passTest called) def
  def1IsDeferred <- isDeferred def1
  case def1IsDeferred of
    True -> passTest correctRV
    _ -> failTest correctRV
  resolve "foo" def1
  return ()

testDeferredDone :: Fay ()
testDeferredDone = do
  called <- newResultBox "Deferred done callback triggered"
  correctArg <- newResultBox "Deferred done passed correct value"
  correctRV <- newResultBox "Deferred done returns Deferred"
  def <- deferred Undefined :: Fay (Deferred Text)
  rv <- done (\v -> do
    passTest called
    case v of
      "Wee timorous beastie!" -> passTest correctArg
      _ -> failTest correctArg) def
  rvIsDeferred <- isDeferred rv
  case rvIsDeferred of
    True -> passTest correctRV
    _ -> failTest correctRV
  resolve "Wee timorous beastie!" def
  return ()

testDeferredFail :: Fay ()
testDeferredFail = do
  called <- newResultBox "Deferred fail callback triggered"
  correctRV <- newResultBox "Deferred fail returns Deferred"
  def <- (deferred Undefined) :: Fay (Deferred Text)
  def1 <- fail_ (passTest called) def
  def1IsDeferred <- isDeferred def1
  case def1IsDeferred of
    True -> passTest correctRV
    _ -> failTest correctRV
  reject "foo" def1
  return ()

testDeferredPromise :: Fay ()
testDeferredPromise = do
  promiseReturned <- newResultBox "Deferred promise returns something Deferred"
  def <- (deferred Undefined) :: Fay (Deferred Text)
  prom <- promise_ Undefined def :: Fay (Deferred Text)
  promIsDeferred <- isDeferred prom
  case promIsDeferred of
    True -> passTest promiseReturned
    _ -> failTest promiseReturned

testDeferredState :: Fay ()
testDeferredState = do
  statePendingAssertion <- newResultBox "Deferred state returns Pending constructor"
  stateRejectedAssertion <- newResultBox "Deferred state returns Rejected constructor"
  stateResolvedAssertion <- newResultBox "Deferred state returns Resolved constructor"
  defSuccessExample <- (deferred Undefined) :: Fay (Deferred Text)
  defFailureExample <- (deferred Undefined) :: Fay (Deferred Text)
  pendingState <- state defSuccessExample
  resolve "foo" defSuccessExample
  reject "foo" defFailureExample

  successState <- state defSuccessExample
  failureState <- state defFailureExample

  case pendingState of
    Pending -> passTest statePendingAssertion
    _ -> failTest statePendingAssertion
  case failureState of
    Rejected -> passTest stateRejectedAssertion
    _ -> failTest stateRejectedAssertion
  case successState of
    Resolved -> passTest stateResolvedAssertion
    _ -> failTest stateResolvedAssertion

testDeferredThen :: Fay ()
testDeferredThen = do
  doneCalled <- newResultBox "Deferred then calls doneFilter"
  failCalled <- newResultBox "Deferred then calls failFilter"
  progressCalled <- newResultBox "Deferred then progressFilter"
  doneArgCorrect <- newResultBox "Deferred then passes correct argument to doneFilter"
  doneRVCorrect <- newResultBox "Deferred then returns correct value"

  defSuccessExample <- (deferred Undefined) :: Fay (Deferred Text)
  defFailureExample <- (deferred Undefined) :: Fay (Deferred Text)

  then_ Undefined (Defined $ passTest failCalled) Undefined defFailureExample

  filtered <- then_ (Defined $ \v -> do
    passTest doneCalled

    case v of
      "Great chieftain o the pudding race" -> passTest doneArgCorrect
      _ -> failTest doneArgCorrect

    return "Painch, tripe, or thairm" :: Fay Text
    ) Undefined Undefined defSuccessExample

  done (\v -> do
    case v of
      "Painch, tripe, or thairm" -> passTest doneRVCorrect
      _ -> failTest doneRVCorrect
    ) filtered

  resolve "Great chieftain o the pudding race" defSuccessExample
  reject "bar" defFailureExample

  return ()

testDeferredWhen :: Fay ()
testDeferredWhen = do
  dependentResolved <- newResultBox "Deferred when resolves dependent Deferred"
  dependentArgCorrect <- newResultBox "Deferred when returned Deferred receives correct argument"

  d1 <- (deferred Undefined) :: Fay (Deferred Text)
  d2 <- (deferred Undefined) :: Fay (Deferred Text)

  dependent <- when_ [d1, d2] >>= done (\v -> do
    passTest dependentResolved
    if all (\(vi, ti) -> vi == ti) (zip v ["Burns night", "Hogmanay"]) then
      passTest dependentArgCorrect
    else
      -- TODO - Bug somewhere. Please see console output
      putStrLn (show v)
      >>
      putStrLn (show ["Burns night", "Hogmanay"])
      >>
      failTest dependentArgCorrect
    return ()
    )

  resolve "Burns night" d1
  resolve "Hogmanay" d2


testDeferred :: Fay ()
testDeferred = do
  testDeferredConstructorBefore
  testDeferredConstructor
  testDeferredAlways
  testDeferredDone
  testDeferredFail
  testDeferredPromise
  testDeferredState
  testDeferredThen
  testDeferredWhen


  s1 <- (ffi "\"resolved\"" :: Fay Text)
  s2 <- (ffi "\"pending\"" :: Fay Text)
  (ffi "console.log(%1)" :: Bool -> Fay ()) $ (s1 == s2)

  case s1 of
    s1 -> (ffi "console.log(\"OK\")" :: Fay ())
    s2 -> (ffi "console.log(\"Weird\")" :: Fay ())




--  testDeferredAlways

