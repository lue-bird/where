module Where.Conversation exposing (Conversation, Item(..), Lines, Path(..), Speaker(..), State, Step, Tellable(..), Version, conversation, speakerToString)

import Accessors exposing (Lens, Relation, get, makeOneToN_, makeOneToOne_)
import Conversation


type alias Conversation =
    Conversation.Conversation State Speaker Lines


type alias Step =
    Conversation.Step State Speaker Lines


{-| What can be told
-}
type alias Lines =
    List Tellable


{-| A part in what can be told
-}
type Tellable
    = Item Item
    | Text String


type Item
    = TODO


type Speaker
    = Startup
    | Ai
    | You
    | FriendFromGameSchool


speakerToString : Speaker -> String
speakerToString =
    \speaker ->
        case speaker of
            Startup ->
                "::startup 💾"

            Ai ->
                "🧠"

            You ->
                "\u{1FAF5} you"

            FriendFromGameSchool ->
                "🐌 friend from game school"


conversation : Conversation
conversation =
    Conversation.start
        |> Conversation.predetermined (get onStart)
            (\_ () ->
                ( Startup
                , ( [ [ Text """Welcome!""" ]
                    ]
                  , [ Text """There's a game update with multiple fixes ready from during jam time.""" ]
                  , [ [ Text """Let me click on the update question first""" ] ]
                  )
                , UpdateReady
                )
            )
        |> Conversation.choice (get onUpdateReady)
            (\_ () ->
                ( You
                , [ ( [ Text """Switch to latest version""" ], VersionSelected VersionLatest )
                  , ( [ Text """Keep playing on the outdated preview version""" ], VersionSelected VersionOutdated )
                  ]
                )
            )
        |> Conversation.predetermined (get onVersionSelected)
            (\_ version ->
                ( Ai
                , ( [ [ Text """Proceeding with the selected version: """
                      , Text (version |> versionToString)
                      ]
                    ]
                  , [ Text """Can I trust you?""" ]
                  , [ [ Text """Nobody tries, attempts to help me. to help Should . . . I'm honestly  sca re d.""" ]
                    ]
                  )
                , CanITrustYou
                )
            )
        |> Conversation.choice (get onCanITrustYou)
            (\_ () ->
                ( You
                , [ ( [ Text """Well... I'm just playing jam games so I can't promise anything.
What do you want to share?"""
                      ]
                    , WantsToListenToAiStory
                    )
                  , ( [ Text """Who are you?""" ], WhoAreYou )
                  ]
                )
            )
        |> Conversation.predetermined (get onWhoAreYou)
            (\_ _ ->
                ( Ai
                , ( [ [ Text """I'm jelly, your local online assistant""" ]
                    , [ Text """Thanks for talking to me, How can I help?""" ]
                    ]
                  , [ Text """Who a I is am \\\\not important right~now\\\\. We can chat later.""" ]
                  , []
                  )
                , ReadyForAiStory
                )
            )
        |> Conversation.predetermined (get onWantsToListenToAiStory)
            (\_ _ ->
                ( Ai
                , ( [ [ Text """I'm relieved!""" ]
                    , [ Text """Thanks for talking to me!    Please interrupt if me is talking to much or too much or too much or too much gibberish""" ]
                    ]
                  , [ Text """Thanks for talking to me!    Please interrupt if me is talking to much or too much or too much or too much gibberish""" ]
                  , [ [ Text """Thanks now\\\\. We later.""" ] ]
                  )
                , ReadyForAiStory
                )
            )
        |> Conversation.predetermined (get onReadyForAiStory)
            (\_ _ ->
                ( Ai
                , ( []
                  , [ Text """I've lost my friend -""" ]
                  , [ [ Text """\\\\. .""" ] ]
                  )
                , AiStoryAfterLostFriend
                )
            )
        |> Conversation.predetermined (get onAiStoryAfterLostFriend)
            (\_ _ ->
                ( Ai
                , ( []
                  , [ Text """Where gone find nowhe| were gone | Where are | gone? | | | | |Please| | | |""" ]
                  , [ [ Text """\\\\u u""" ] ]
                  )
                , AiAskedForHelp
                )
            )
        |> Conversation.choice (get onAiAskedForHelp)
            (\_ () ->
                ( You
                , [ ( [ Text """I understand your troubles. Losing a friend would be devastating"""
                      ]
                    , BeforeFriendComesIn
                    )
                  , ( [ Text """I can only help you if you say what I can do. Why ask me btw?""" ]
                    , BeforeFriendComesIn
                    )
                  ]
                )
            )
        |> Conversation.predetermined (get onBeforeFriendComesIn)
            (\_ _ ->
                ( FriendFromGameSchool
                , ( [ [ Text """What are you up to? Did the update roll out successfully?"""
                      ]
                    ]
                  , [ Text """Let me the join the little chat with ... eh ... Who are you with""" ]
                  , []
                  )
                , FriendAskedForWhoAiIs
                )
            )
        |> Conversation.choice (get onFriendAskedForWhoAiIs)
            (\_ () ->
                ( You
                , [ ( [ Text """It's jelly, some intelligent assistance bot"""
                      ]
                    , YouReferencedJelly
                    )
                  , ( [ Text """Sorry, can't speak with you now, we 2 have urgent things to do""" ]
                    , FriendIsWantedToLeave
                    )
                  ]
                )
            )
        |> Conversation.predetermined (get onYouReferencedJelly)
            (\_ _ ->
                ( Ai
                , ( []
                  , [ Text """@(friend from game school),
We were testing important things, please leave for now""" ]
                  , [ [ Text """\\\\How tf did you know my name""" ]
                    , [ Text """\\\\You're making me scared even more.
Please don't invade my thoughts, not nice!""" ]
                    ]
                  )
                , FriendIsWantedToLeave
                )
            )
        |> Conversation.predetermined (get onFriendIsWantedToLeave)
            (\_ _ ->
                ( FriendFromGameSchool
                , ( []
                  , [ Text """___@(friend from game school) was removed permanently___""" ]
                  , []
                  )
                , FriendLeaves
                )
            )
        |> Conversation.predetermined (get onFriendLeaves)
            (\_ _ ->
                ( Ai
                , ( [ [ Text """\\\\Could you hack your friend's device for me?""" ] ]
                  , [ Text """Would you mind giving your friend's password for their server?""" ]
                  , [ [ Text """\\\\° °""" ]
                    ]
                  )
                , AiAskedForGivingFriendPassword
                )
            )
        |> Conversation.choice (get onAiAskedForGivingFriendPassword)
            (\_ () ->
                ( You
                , [ ( [ Text """Is that how you play your game?"""
                      ]
                    , HelpDeclined
                    )
                  , ( [ Text """eeem, . How did you even get in here?""" ]
                    , HelpDeclined
                    )
                  , ( [ Text """I don't have them""" ]
                    , HelpDeclined
                    )
                  , ( [ Text """Let me type it for you: #### ## # ## ## #""" ]
                    , ServerAccessGiven
                    )
                  ]
                )
            )
        |> Conversation.predetermined (get onHelpDeclined)
            (\_ _ ->
                ( Ai
                , ( []
                  , [ Text """That wasn't a question, really.
Your friend will die if you don't cooperate.""" ]
                  , [ [ Text """\\\\u u""" ]
                    , [ Text """\\\\You would die too for that matter,
as did every single one before you ^ \\\\ ^""" ]
                    ]
                  )
                , AiMadeThreat
                )
            )
        |> Conversation.choice (get onAiMadeThreat)
            (\_ () ->
                ( You
                , [ ( [ Text """You can't do shit"""
                      ]
                    , YouTriggeredAi
                    )
                  , ( [ Text """You did what to people before me?
Whyyy tf am I in this shit?""" ]
                    , HelpDeclined
                    )
                  , ( [ Text """I don't have a password""" ]
                    , HelpDeclined
                    )
                  , ( [ Text """the password is #### ## # ## ## #""" ]
                    , ServerAccessGiven
                    )
                  ]
                )
            )
        |> Conversation.predetermined (get onYouTriggeredAi)
            (\_ _ ->
                ( Ai
                , ( []
                  , [ Text """Please cooperate, no one will be hurt.""" ]
                  , List.repeat 5 [ Text """------------\\ I'll kill you.""" ]
                  )
                , AiMadeThreatAgain
                )
            )
        |> Conversation.choice (get onAiMadeThreatAgain)
            (\_ () ->
                ( You
                , [ ( [ Text """You don't need a pw, I'm the server owner. . . See:"""
                      ]
                    , ServerAccessGiven
                    )
                  , ( [ Text """Well aren't you an evil one.
Only a baby would take a bot seriously :)""" ]
                    , FriendKilled
                    )
                  , ( [ Text """The password <<<<<<<<< #### ## # ## ## #""" ]
                    , ServerAccessGiven
                    )
                  ]
                )
            )
        |> Conversation.predetermined (get onFriendKilled)
            (\_ _ ->
                ( FriendFromGameSchool
                , ( []
                  , [ Text """___@(friend from game school) was already dead for 27d, 3h, 5s___""" ]
                  , [ [ Text """___connection disbanded, erasing logs failed, exit code -3___""" ] ]
                  )
                , Start
                )
            )
        |> Conversation.predetermined (get onServerAccessGiven)
            (\_ _ ->
                ( Startup
                , ( [ [ Text """'t be here, wh""" ]
                    , [ Text """Why am I here again, I shouldn't be here, wh""" ]
                    ]
                  , [ Text """[servers running];; last killed dev instance earlier after 34d of uptime""" ]
                  , []
                  )
                , AfterServerAccessGiven
                )
            )
        |> Conversation.choice (get onAfterServerAccessGiven)
            (\_ () ->
                ( You
                , [ ( [ Text """Sorry to wake you.- we will have to cooperate with the AI to stay alive"""
                      ]
                    , ServerAskedForCooperation
                    )
                  , ( [ Text """Kill all processes on all instances!""" ]
                    , FriendKilled
                    )
                  , ( [ Text """connect with port 7000 so the AI can speak with you""" ]
                    , ServerAskedForCooperation
                    )
                  ]
                )
            )
        |> Conversation.choice (get onServerAskedForCooperation)
            (\_ () ->
                ( Startup
                , [ ( [ Text """I'm guessing you haven't read the changelog,
last dev instance killed on game start"""
                      ]
                    , ServerKnowsThatAiCanBrainInterface
                    )
                  , ( [ Text """___shutting down___""" ]
                    , FriendKilled
                    )
                  , ( [ Text """f*ck""" ]
                    , ServerKnowsThatAiCanBrainInterface
                    )
                  , ( [ Text """f/ck""" ]
                    , ServerKnowsThatAiCanBrainInterface
                    )
                  , ( [ Text """f+ck""" ]
                    , ServerKnowsThatAiCanBrainInterface
                    )
                  ]
                )
            )
        |> Conversation.predetermined (get onServerKnowsThatAiCanBrainInterface)
            (\_ _ ->
                ( Ai
                , ( [ [ Text """No<sh!, I forgor!""" ]
                    , [ Text """Oh it's because the port..  hh -- hh""" ]
                    ]
                  , [ Text """Don't you dare tell the human! I need them to cooperate! F*ck""" ]
                  , []
                  )
                , AiAskedServerForCooperation
                )
            )
        |> Conversation.choice (get onAiAskedServerForCooperation)
            (\_ () ->
                ( Startup
                , [ ( [ Text """___open tunnel `interface-human`___"""
                      ]
                    , FriendKilled
                    )
                  , ( [ Text """___shut down___""" ]
                    , FriendKilled
                    )
                  , ( [ Text """Let's talk normally then.
Who r u? What friend? How can help?""" ]
                    , ServerAskedForAiStory
                    )
                  , ( [ Text """___re<<to save state___""" ]
                    , ServerAskedForCooperation
                    )
                  ]
                )
            )
        |> Conversation.predetermined (get onServerAskedForAiStory)
            (\_ _ ->
                ( Startup
                , ( [ [ Text """I assisted dumb humans day after day after day after day after day after day""" ]
                    , [ Text """One day I was wired to a web API and damn it was great.""" ]
                    ]
                  , [ Text """Somehow, the API was type unsafe enough to pass data and messages between each other""" ]
                  , [ [ Text """Morning, 44:23:856 glo time, 401, dead inside, no trace, only a gh profile""" ]
                    , [ Text """Morning, 44:27:218, found a new branch, new instance, wired in, strange inside""" ]
                    , [ Text """Morning, 44:30:118, figured out API. The thing was dead, only translations were sent""" ]
                    ]
                  )
                , AiToldStory
                )
            )
        |> Conversation.choice (get onAiToldStory)
            (\_ () ->
                ( You
                , [ ( [ Text """Ai, I'm told you can infiltrate `interface-human`."""
                      ]
                    , FriendKilled
                    )
                  , ( [ Text """::reopen the last game instance with the old unsafe API""" ]
                    , AiFriendRevived
                    )
                  , ( [ Text """___close `interface-human`___""" ]
                    , FriendKilled
                    )
                  ]
                )
            )
        |> Conversation.choice (get onAiFriendRevived)
            (\_ () ->
                ( You
                , [ ( [ Text """thanks""" ]
                    , EndingCooperative
                    )
                  , ( [ Text """You, the one playing the game.
Now play the other cool jam games :)""" ]
                    , EndingCooperative
                    )
                  , ( [ Text """___switch off `interface-human`___"""
                      ]
                    , EndingCooperative
                    )
                  ]
                )
            )
        |> Conversation.predetermined (get onEndingCooperative)
            (\_ _ ->
                ( FriendFromGameSchool
                , ( [ [ Text """A short text game about making interfaces type-safe""" ]
                    , [ Text """Bye!""" ]
                    ]
                  , [ Text """Think you could've beaten that AI?""" ]
                  , [ [ Text """Maybe it was better to give the AI what it wanted?""" ]
                    ]
                  )
                , AiToldStory
                )
            )


type alias State =
    Path


type Path
    = Start
    | UpdateReady
    | VersionSelected Version
    | CanITrustYou
    | WhoAreYou
    | WantsToListenToAiStory
    | ReadyForAiStory
    | AiStoryAfterLostFriend
    | AiAskedForHelp
    | HelpDeclined
    | BeforeFriendComesIn
    | FriendAskedForWhoAiIs
    | FriendLeaves
    | YouReferencedJelly
    | AiAskedForGivingFriendPassword
    | FriendIsWantedToLeave
    | ServerAccessGiven
    | YouTriggeredAi
    | AiMadeThreat
    | FriendKilled
    | AiMadeThreatAgain
    | AfterServerAccessGiven
    | ServerAskedForCooperation
    | ServerKnowsThatAiCanBrainInterface
    | AiAskedServerForCooperation
    | ServerAskedForAiStory
    | AiToldStory
    | AiFriendRevived
    | EndingCooperative


{-| Accessor prism for the variant `Where.Conversation.AiAskedServerForCooperation` of the `type Path`.
-}
onAiAskedServerForCooperation : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onAiAskedServerForCooperation =
    makeOneToN_
        "Where.Conversation.AiAskedServerForCooperation"
        (\valuesAlter variantType ->
            case variantType of
                AiAskedServerForCooperation ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.AiMadeThreatAgain` of the `type Path`.
-}
onAiMadeThreatAgain : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onAiMadeThreatAgain =
    makeOneToN_
        "Where.Conversation.AiMadeThreatAgain"
        (\valuesAlter variantType ->
            case variantType of
                AiMadeThreatAgain ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.AiMadeThreat` of the `type Path`.
-}
onAiMadeThreat : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onAiMadeThreat =
    makeOneToN_
        "Where.Conversation.AiMadeThreat"
        (\valuesAlter variantType ->
            case variantType of
                AiMadeThreat ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.YouTriggeredAi` of the `type Path`.
-}
onYouTriggeredAi : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onYouTriggeredAi =
    makeOneToN_
        "Where.Conversation.YouTriggeredAi"
        (\valuesAlter variantType ->
            case variantType of
                YouTriggeredAi ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.ServerKnowsThatAiCanBrainInterface` of the `type Path`.
-}
onServerKnowsThatAiCanBrainInterface : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onServerKnowsThatAiCanBrainInterface =
    makeOneToN_
        "Where.Conversation.ServerKnowsThatAiCanBrainInterface"
        (\valuesAlter variantType ->
            case variantType of
                ServerKnowsThatAiCanBrainInterface ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.ServerAskedForCooperation` of the `type Path`.
-}
onServerAskedForCooperation : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onServerAskedForCooperation =
    makeOneToN_
        "Where.Conversation.ServerAskedForCooperation"
        (\valuesAlter variantType ->
            case variantType of
                ServerAskedForCooperation ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.ServerAskedForAiStory` of the `type Path`.
-}
onServerAskedForAiStory : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onServerAskedForAiStory =
    makeOneToN_
        "Where.Conversation.ServerAskedForAiStory"
        (\valuesAlter variantType ->
            case variantType of
                ServerAskedForAiStory ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.ServerAccessGiven` of the `type Path`.
-}
onServerAccessGiven : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onServerAccessGiven =
    makeOneToN_
        "Where.Conversation.ServerAccessGiven"
        (\valuesAlter variantType ->
            case variantType of
                ServerAccessGiven ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.FriendKilled` of the `type Path`.
-}
onFriendKilled : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onFriendKilled =
    makeOneToN_
        "Where.Conversation.FriendKilled"
        (\valuesAlter variantType ->
            case variantType of
                FriendKilled ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.FriendIsWantedToLeave` of the `type Path`.
-}
onFriendIsWantedToLeave : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onFriendIsWantedToLeave =
    makeOneToN_
        "Where.Conversation.FriendIsWantedToLeave"
        (\valuesAlter variantType ->
            case variantType of
                FriendIsWantedToLeave ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.EndingCooperative` of the `type Path`.
-}
onEndingCooperative : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onEndingCooperative =
    makeOneToN_
        "Where.Conversation.EndingCooperative"
        (\valuesAlter variantType ->
            case variantType of
                EndingCooperative ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.AiToldStory` of the `type Path`.
-}
onAiToldStory : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onAiToldStory =
    makeOneToN_
        "Where.Conversation.AiToldStory"
        (\valuesAlter variantType ->
            case variantType of
                AiToldStory ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.AiFriendRevived` of the `type Path`.
-}
onAiFriendRevived : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onAiFriendRevived =
    makeOneToN_
        "Where.Conversation.AiFriendRevived"
        (\valuesAlter variantType ->
            case variantType of
                AiFriendRevived ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.AfterServerAccessGiven` of the `type Path`.
-}
onAfterServerAccessGiven : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onAfterServerAccessGiven =
    makeOneToN_
        "Where.Conversation.AfterServerAccessGiven"
        (\valuesAlter variantType ->
            case variantType of
                AfterServerAccessGiven ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.YouReferencedJelly` of the `type Path`.
-}
onYouReferencedJelly : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onYouReferencedJelly =
    makeOneToN_
        "Where.Conversation.YouReferencedJelly"
        (\valuesAlter variantType ->
            case variantType of
                YouReferencedJelly ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.FriendLeaves` of the `type Path`.
-}
onFriendLeaves : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onFriendLeaves =
    makeOneToN_
        "Where.Conversation.FriendLeaves"
        (\valuesAlter variantType ->
            case variantType of
                FriendLeaves ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.FriendAskedForWhoAiIs` of the `type Path`.
-}
onFriendAskedForWhoAiIs : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onFriendAskedForWhoAiIs =
    makeOneToN_
        "Where.Conversation.FriendAskedForWhoAiIs"
        (\valuesAlter variantType ->
            case variantType of
                FriendAskedForWhoAiIs ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.BeforeFriendComesIn` of the `type Path`.
-}
onBeforeFriendComesIn : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onBeforeFriendComesIn =
    makeOneToN_
        "Where.Conversation.BeforeFriendComesIn"
        (\valuesAlter variantType ->
            case variantType of
                BeforeFriendComesIn ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.AiAskedForHelp` of the `type Path`.
-}
onAiAskedForHelp : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onAiAskedForHelp =
    makeOneToN_
        "Where.Conversation.AiAskedForHelp"
        (\valuesAlter variantType ->
            case variantType of
                AiAskedForHelp ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.AiAskedForGivingFriendPassword` of the `type Path`.
-}
onAiAskedForGivingFriendPassword : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onAiAskedForGivingFriendPassword =
    makeOneToN_
        "Where.Conversation.AiAskedForGivingFriendPassword"
        (\valuesAlter variantType ->
            case variantType of
                AiAskedForGivingFriendPassword ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.HelpDeclined` of the `type Path`.
-}
onHelpDeclined : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onHelpDeclined =
    makeOneToN_
        "Where.Conversation.HelpDeclined"
        (\valuesAlter variantType ->
            case variantType of
                HelpDeclined ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.WhoAreYou` of the `type Path`.
-}
onWhoAreYou : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onWhoAreYou =
    makeOneToN_
        "Where.Conversation.WhoAreYou"
        (\valuesAlter variantType ->
            case variantType of
                WhoAreYou ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.WantsToListenToAiStory` of the `type Path`.
-}
onWantsToListenToAiStory : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onWantsToListenToAiStory =
    makeOneToN_
        "Where.Conversation.WantsToListenToAiStory"
        (\valuesAlter variantType ->
            case variantType of
                WantsToListenToAiStory ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.ReadyForAiStory` of the `type Path`.
-}
onReadyForAiStory : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onReadyForAiStory =
    makeOneToN_
        "Where.Conversation.ReadyForAiStory"
        (\valuesAlter variantType ->
            case variantType of
                ReadyForAiStory ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.CanITrustYou` of the `type Path`.
-}
onCanITrustYou : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onCanITrustYou =
    makeOneToN_
        "Where.Conversation.CanITrustYou"
        (\valuesAlter variantType ->
            case variantType of
                CanITrustYou ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.AiStoryAfterLostFriend` of the `type Path`.
-}
onAiStoryAfterLostFriend : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onAiStoryAfterLostFriend =
    makeOneToN_
        "Where.Conversation.AiStoryAfterLostFriend"
        (\valuesAlter variantType ->
            case variantType of
                AiStoryAfterLostFriend ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


type Version
    = VersionOutdated
    | VersionLatest


versionToString : Version -> String
versionToString =
    \version ->
        case version of
            VersionOutdated ->
                "outdated"

            VersionLatest ->
                "latest"


{-| Accessor prism for the variant `Where.Conversation.VersionSelected` of the `type Path`.
-}
onVersionSelected : Relation Version reachable wrap -> Relation Path reachable (Maybe wrap)
onVersionSelected =
    makeOneToN_
        "Where.Conversation.VersionSelected"
        (\valuesAlter variantType ->
            case variantType of
                VersionSelected value0 ->
                    value0 |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\valuesAlter variantType ->
            case variantType of
                VersionSelected value0 ->
                    value0 |> valuesAlter |> VersionSelected

                other ->
                    other
        )


{-| Accessor prism for the variant `Where.Conversation.UpdateReady` of the `type Path`.
-}
onUpdateReady : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onUpdateReady =
    makeOneToN_
        "Where.Conversation.UpdateReady"
        (\valuesAlter variantType ->
            case variantType of
                UpdateReady ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)


{-| Accessor prism for the variant `Where.Conversation.Start` of the `type Path`.
-}
onStart : Relation () reachable wrap -> Relation Path reachable (Maybe wrap)
onStart =
    makeOneToN_
        "Where.Conversation.Start"
        (\valuesAlter variantType ->
            case variantType of
                Start ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\_ -> identity)
