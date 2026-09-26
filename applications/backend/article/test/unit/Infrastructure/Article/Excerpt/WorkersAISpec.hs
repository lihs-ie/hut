module Infrastructure.Article.Excerpt.WorkersAISpec (run) where

import Cloudflare.Workers.Binding.WorkersAI (WorkersAIError (WorkersAIException))
import Cloudflare.Workers.Binding.WorkersAI.Gemma (
    GemmaChatChoice (..),
    GemmaInput (..),
    GemmaMessage (..),
    GemmaOutput (..),
    GemmaResponseMessage (..),
    GemmaTextChoice (..),
 )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Domain.Article.Draft (proofread, proofreadedContent)
import Infrastructure.Article.Excerpt.WorkersAI (excerptFromOutput, generateExcerptWith, generationInput)
import Shared.Domain.Excerpt (excerptText)
import TestSupport (check, confirmed, right, start, timestamp)

run :: IO ()
run = do
    draft <- right start
    available <- right confirmed
    article <- right (proofread (timestamp 1) available draft)
    case generationInput (proofreadedContent article) of
        GemmaMessages (GemmaSystem system :| [GemmaUser user]) -> do
            check "system treats article as data" ("source text" `Text.isInfixOf` system)
            check "input includes title" ("Haskell" `Text.isInfixOf` user)
            check "input includes body" ("Body with managed-image" `Text.isInfixOf` user)
        _ -> fail "expected system and user messages"

    let chatOutput =
            GemmaChatOutput
                "result"
                (GemmaChatChoice 0 (GemmaResponseMessage (Just "  Short excerpt  ")) :| [])
    chat <- right (excerptFromOutput chatOutput)
    check "chat text is trimmed" (excerptText chat == "Short excerpt")
    plain <- right (excerptFromOutput (GemmaTextOutput "result" (GemmaTextChoice 0 "Plain excerpt" :| [])))
    check "text output is accepted" (excerptText plain == "Plain excerpt")
    check "blank text is rejected" (isLeft (excerptFromOutput (textOutput 0 "  ")))
    check
        "overlong text is rejected"
        (isLeft (excerptFromOutput (textOutput 0 (Text.replicate 201 "x"))))
    check "missing first choice is rejected" (isLeft (excerptFromOutput (textOutput 1 "Other")))
    let orderedText = GemmaTextOutput "result"
            (GemmaTextChoice 1 "Wrong" :| [GemmaTextChoice 0 "Selected"])
    selectedText <- right (excerptFromOutput orderedText)
    check "text choice zero is selected" (excerptText selectedText == "Selected")
    let missingText = GemmaChatOutput "result" (GemmaChatChoice 0 (GemmaResponseMessage Nothing) :| [])
    check "missing chat text is rejected" (isLeft (excerptFromOutput missingText))
    let missingChatChoice = GemmaChatOutput "result"
            (GemmaChatChoice 1 (GemmaResponseMessage (Just "Other")) :| [])
        orderedChat = GemmaChatOutput "result"
            ( GemmaChatChoice 1 (GemmaResponseMessage (Just "Wrong"))
                :| [GemmaChatChoice 0 (GemmaResponseMessage (Just "Selected"))]
            )
    check "missing chat choice zero is rejected" (isLeft (excerptFromOutput missingChatChoice))
    selectedChat <- right (excerptFromOutput orderedChat)
    check "chat choice zero is selected" (excerptText selectedChat == "Selected")
    generated <- generateExcerptWith
        (\_ -> pure (Right chatOutput)) (proofreadedContent article)
    check "generation adapter returns validated excerpt"
        (fmap excerptText generated == Right "Short excerpt")
    failed <- generateExcerptWith
        (\_ -> pure (Left (WorkersAIException "offline" Nothing)))
        (proofreadedContent article)
    check "provider failure becomes a retryable domain error" (isLeft failed)

textOutput :: Integer -> Text.Text -> GemmaOutput
textOutput index value = GemmaTextOutput "result" (GemmaTextChoice index value :| [])

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
