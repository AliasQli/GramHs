{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Gui.Update where

import Control.Monad
import Control.Monad.ST
import Data.Bifunctor (Bifunctor (second))
import Data.Functor
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GI.Gtk.Declarative.App.Simple
import Gui.CustomInput
import Model.Contact hiding (group)
import Model.Message
import Model.Parse
import Model.Req
import Net.Http
import Type

{- |
  A \"Promise" data type. Inspired by js.
  I was once tempted to use the built-in Haskell lazy-evaluation,
  however I would be unable to retry then.
-}
data Promise a
  = -- | 'Pending' state.
    -- Indicating the action is being performed (or has not started).
    -- May change to 'Failed' or @'Success' a@.
    Pending
  | -- | 'Failed' state. Indicating the action has failed.
    -- Note the error is not contained here.
    Failed
  | -- | 'Success' state. Indicating the action has succeeded.
    Success a
  deriving (Show, Eq)

-- | The current 'Contact'.
data CurrentContact
  = -- | No 'Contact'. Never should it be used apart from in the initial state.
    Nihil
  | -- | Current 'Contact' is a 'Friend'.
    CurrentFriend Friend
  | -- | Current 'Contact' is a 'Group'.
    CurrentGroup Group
  deriving (Show, Eq)

-- | Get the display text of the current 'Contact'.
getCurrentDisplay :: State -> CurrentContact -> Text
getCurrentDisplay State{..} = \case
  Nihil -> ""
  CurrentFriend friend -> getDisplay friend
  CurrentGroup group -> getDisplay group

-- | The related information of a friend :: 'Friend'.
newtype FriendRecord = FriendRecord
  { -- | A vector of 'MessageObject'.
    -- Stores all the received messages with the friend.
    friendMessages :: Vector MessageObject
  }
  deriving (Show, Eq)

-- | The related information of a group :: 'Group'.
data GroupRecord = GroupRecord
  { -- | The 'Promise' of the 'MemberList' related to the 'Group'.
    memberList :: Promise MemberList
  , -- | A vector of 'MessageObject'.
    -- Stores all the received messages in the group.
    groupMessages :: Vector MessageObject
  }
  deriving (Show, Eq)

-- | The state of the GUI.
data State = State
  { -- | The vector of @('Friend', 'FriendRecord')@ tuple.
    -- Here 'Friend' acts as the index.
    -- Don't index the vector directly as it may constantly change.
    friends :: Vector (Friend, FriendRecord)
  , -- | The vector of @('Group', 'GroupRecord')@ tuple.
    -- Here 'Group' acts as the index.
    -- Don't index the vector directly as it may constantly change.
    groups :: Vector (Group, GroupRecord)
  , -- | The current 'Contact'.
    currentContact :: CurrentContact
  , -- | User input search text, splitted by space.
    searchTexts :: [Text]
  , -- | The properties of the custom input box, used to inform the custom widget on its change.
    inputBoxProperties :: InputBoxProperties
  }
  deriving (Show, Eq)

-- | Get the initial state of the GUI.
getInitState :: Http State
getInitState = do
  verify
  friendList <- getFriendList
  groupList <- getGroupList
  return $
    State
      ((,FriendRecord []) <$> friendList)
      ((,GroupRecord Pending []) <$> groupList)
      Nihil
      []
      (InputBoxProperties 0 Sended)

-- | Events widgets may emit.
data Event
  = -- | No event. Since a signal handler must return a value,
    -- it may return NoEvent when nothing should be returned.
    NoEvent
  | -- | The window has been closed.
    Closed
  | -- | The search text has changed.
    --
    -- [@[Text\]@] User input search text, splitted by space.
    SearchChange [Text]
  | -- | The current contact has been changed.
    --
    --   [@CurrentContact@] The new contact.
    ContactChange CurrentContact
  | -- | The requested memberList has arrived.
    --
    -- __This event should only be emitted by 'update'' itself.__
    --
    -- [@Group@] Group the memberList relates to.
    -- [@Maybe MemberList@] @Just memberList@ if succeeded, @Nothing@ if failed.
    MemberListArrive Group (Maybe MemberList)
  | -- | A member in the memberList has been doubleclicked.
    --
    -- [@Int@] The member's qq.
    MemberClicked Int
  | -- | A message in the message list has been doubleclicked.
    --
    -- [@Int@] The messageId.
    MessageClicked Int
  | -- | The used sends some text to the current contact.
    --
    -- [@Maybe Int@] The messageId to quote.
    -- [@Text@] The text to send.
    Send (Maybe Int) Text
  | -- | The sending action has completed.
    --
    -- __This event should only be emitted by 'update'' itself.__
    --
    -- [@CurrentContact@] The contact message is sent to. Never should 'Nihil' be used here.
    -- [@MessageChain@] The message sent. Equals to 'mzero' if failed to send.
    SendComplete CurrentContact MessageChain
  | -- | A message from a friend has been received.
    --
    -- Note that a message successfully sent by the user is also
    -- treated as a message received, and displayed in the same way.
    --
    -- [@Friend@] The message source.
    -- [@MessageChain@] The messageChain.
    ReceiveFriendMessage Friend MessageObject
  | -- | A message from a group has been received.
    --
    -- Note that a message successfully sent by the user is also
    -- treated as a message received, and displayed in the same way.
    --
    -- [@Group@] The message source.
    -- [@MessageChain@] The messageChain.
    ReceiveGroupMessage Group MessageObject

-- | No event to emit.
noEvent :: IO (Maybe Event)
noEvent = return Nothing

-- | Wrap an event to emit in 'Just'.
event :: a -> IO (Maybe a)
event = return . Just

{- |
  The core function 'update'' . It transitions the current state
  to a new state upon an event, and optionally emits a new event.
-}
update' :: State -> Event -> Transition State Event
update' state NoEvent = Transition state noEvent
update' _state Closed = Exit
update' state (SearchChange texts) =
  Transition
    state{searchTexts = texts}
    noEvent
update' state@State{..} (ContactChange contact) =
  if
      | CurrentGroup group <- contact
        , GroupRecord{..} <- groups ?! group
        , memberList `elem` ([Pending, Failed] :: [Promise MemberList]) ->
        Transition
          newstate
            { groups =
                groups `modifyAt` group $ \groupRecord -> groupRecord{memberList = Pending}
            }
          $ runHttp (getMemberList $ getId group)
            >>= \case
              Left err -> putTextLn err $> Nothing
              Right memberList -> return $ Just memberList
            >>= event . MemberListArrive group
      | otherwise ->
        Transition newstate noEvent
 where
  newstate = state{currentContact = contact}
update' state@State{..} (MemberListArrive group maybeList) =
  Transition
    state
      { groups =
          groups `modifyAt` group $
            \groupRecord -> groupRecord{memberList = maybe Failed Success maybeList}
      }
    noEvent
update' state@State{..} (MemberClicked target) =
  Transition
    state
      { inputBoxProperties =
          inputBoxProperties
            { version = succ version
            , command = AtMember target
            }
      }
    noEvent
 where
  InputBoxProperties{..} = inputBoxProperties
update' state@State{..} (MessageClicked messageId) =
  Transition
    state
      { inputBoxProperties =
          inputBoxProperties
            { version = succ version
            , command = QuoteMessage messageId
            }
      }
    noEvent
 where
  InputBoxProperties{..} = inputBoxProperties
update' state@State{..} (Send quote t) =
  Transition
    state
    $ event . SendComplete currentContact
      =<< ( \case
              Just (method, target) ->
                runHttp (method $ SendMessage target quote messageChain)
                  >>= \case
                    Left err -> putTextLn err $> []
                    Right messageId ->
                      return $
                        Source messageId 0
                          `V.cons` case quote of
                            Just messageId -> Quote messageId 0 0 0 [] `V.cons` messageChain
                            Nothing -> messageChain -- Bug: Mirai can't reply to a message which quotes another message
              Nothing -> return []
          )
        if
            | CurrentFriend friend <- currentContact -> Just (sendFriendMessage, getId friend)
            | CurrentGroup group <- currentContact -> Just (sendGroupMessage, getId group)
            | otherwise -> Nothing
 where
  messageChain = parseMessage t
update' state@State{..} (SendComplete contact messageChain) =
  if
      | CurrentFriend friend <- contact ->
        Transition
          newstate
          $ eventMaker (ReceiveFriendMessage friend) FriendMessage
      | CurrentGroup group <- contact ->
        Transition
          newstate
          $ eventMaker (ReceiveGroupMessage group) GroupMessage
      | otherwise ->
        Transition newstate noEvent
 where
  InputBoxProperties{..} = inputBoxProperties
  eventMaker wrapper objectWrapper =
    if null messageChain then noEvent else event $ wrapper (objectWrapper messageChain Nothing)
  newstate =
    state
      { inputBoxProperties =
          inputBoxProperties
            { version = succ version
            , command = if null messageChain then Sended else SendedAndClear
            }
      }
update' state@State{..} (ReceiveFriendMessage friend messageObject) =
  Transition
    state
      { friends =
          putModifiedForward
            friends
            friend
            (FriendRecord [])
            (\record@FriendRecord{..} -> record{friendMessages = friendMessages `V.snoc` messageObject})
      }
    noEvent
update' state@State{..} (ReceiveGroupMessage group messageObject) =
  Transition
    state
      { groups =
          putModifiedForward
            groups
            group
            (GroupRecord Pending [])
            (\record@GroupRecord{..} -> record{groupMessages = groupMessages `V.snoc` messageObject})
      }
    noEvent

{- |
  @modifyAt vector a@ finds the @(a', b')@ in the vector where @a' == a@
  and modifies it to @(a', f b')@. __An error is raised when not found.__

  It is suggested to use its infix version:

  > vector `modifyAt` a $ f
-}
modifyAt :: Eq a => Vector (a, b) -> a -> (b -> b) -> Vector (a, b)
modifyAt vector a f = runST $ do
  let Just ix = vector ?? a
  mvector <- ix `seq` V.unsafeThaw vector
  VM.modify mvector (second f) ix
  V.unsafeFreeze mvector

{- |
  @putModifiedForward vector a b f@ moves the @(a', b')@ to the head of @vector@
  where @a' == a@ and modifies @b'@ to @f b'@.
  If such tuple is not found, @(a, b)@ is added to the head of @vector@.
-}
putModifiedForward :: Eq a => Vector (a, b) -> a -> b -> (b -> b) -> Vector (a, b)
putModifiedForward vector a b f =
  case vector ?? a of
    Just ix ->
      runST $ do
        mvector <- V.unsafeThaw vector
        VM.modify mvector (second f) ix
        forM_ ([ix, ix -1 .. 1] :: [Int]) $ \i -> VM.swap mvector i (i -1)
        V.unsafeFreeze mvector
    Nothing -> (a, b) `V.cons` vector

{- |
  @vector ?? a@ returns @Just n@ where @vector ! n == a@.
  Returns @Nothing@ when not found.
  Returns the max index when multiple is found.
-}
(??) :: Eq a => Vector (a, b) -> a -> Maybe Int
vector ?? a = f (length vector - 1) a vector
 where
  f n a vector
    | n < 0 = Nothing
    | (a', _b') <- vector V.! n
      , a == a' =
      Just n
    | otherwise = f (n -1) a vector

{- |
  @vector ?! a@ finds the @(a', b')@ in the vector where @a' == a@ and returns @b'@.
  __Returns @undefined@ when not found.__
  Returns the last one if multiple is found.
-}
(?!) :: Eq a => Vector (a, b) -> a -> b
vector ?! a = f (length vector - 1) a vector
 where
  f n a vector
    | n < 0 = undefined
    | (a', b') <- vector V.! n
      , a == a' =
      b'
    | otherwise = f (n -1) a vector
