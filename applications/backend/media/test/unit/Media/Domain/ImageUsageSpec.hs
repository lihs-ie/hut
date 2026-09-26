module Media.Domain.ImageUsageSpec (run) where

import Data.Either (isLeft)
import Media.Domain.ImageUsage qualified as Usage (
    SourceKind (ArticleSource),
    foldImageUsageProjection,
    imageReferenceText,
    newImageReference,
    newImageUsageProjection,
    newSourceIdentifier,
    newSourcePosition,
    sourceIdentifierText,
    sourcePositionText,
 )
import Media.TestSupport (
    assertEqual,
    baseTime,
    expectRight,
    (<&&>),
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (

 )
import Media.UseCase.RetryImageInspection qualified as RetryInspection (

 )

run :: IO Bool
run =
    and
        <$> sequence
            [ testImageUsageProjection
            ]

testImageUsageProjection :: IO Bool
testImageUsageProjection = do
    source <- expectRight "source fixture" (Usage.newSourceIdentifier "article-1")
    position <- expectRight "position fixture" (Usage.newSourcePosition "body")
    first <- expectRight "first image reference" (Usage.newImageReference "image-1")
    second <- expectRight "second image reference" (Usage.newImageReference "image-2")
    let projection =
            Usage.newImageUsageProjection
                Usage.ArticleSource
                source
                position
                [first, second, first]
                baseTime
        values =
            Usage.foldImageUsageProjection
                ( \kind actualSource actualPosition references referencedAt ->
                    ( kind
                    , Usage.sourceIdentifierText actualSource
                    , Usage.sourcePositionText actualPosition
                    , fmap Usage.imageReferenceText references
                    , referencedAt
                    )
                )
                projection
    assertEqual
        "usage projection deduplicates references without losing order"
        (Usage.ArticleSource, "article-1", "body", ["image-2", "image-1"], baseTime)
        values
        <&&> assertEqual
            "blank usage values are rejected"
            True
            ( and
                [ isLeft (Usage.newSourceIdentifier " ")
                , isLeft (Usage.newSourcePosition " ")
                , isLeft (Usage.newImageReference " ")
                ]
            )
