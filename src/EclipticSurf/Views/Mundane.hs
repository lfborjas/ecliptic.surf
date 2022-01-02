module EclipticSurf.Views.Mundane where

import Lucid
import Control.Monad (when, forM_, unless)
import Data.Maybe (isJust, fromJust)
import Almanac.Extras
import SwissEphemeris (Planet)
import Data.Text (Text)
import Data.Time
import Almanac
import Data.List (intersperse)
import Data.Time.Format.ISO8601 (iso8601Show)

page :: [(Transit Planet, [UTCTime])]-> Html () -> Html ()
page transits chart = do
  main_ $ do
    p_ [class_  "mt-2"] $ do
      "Mundane transits"
    chart
    ul_ $ do
      forM_ transits $ \(Transit{transiting, transited, aspect}, exacts) -> do
        unless (null exacts) $ do
          li_  $ do
            toHtml . mconcat . intersperse " " $ [
                show transiting,
                show aspect,
                show transited,
                "exact at:",
                mconcat . intersperse "," $ map iso8601Show exacts
              ]


form :: Maybe Text -> Html ()
form err = do
  h2_ "Explore mundane transits"
  p_ "Dates are UTC. The Moon is not included due to its high speed."
  form_ [action_ "/mundane", method_ "get"] $ do
    when (isJust err) $ do
      div_ [class_ "bg-error p-2"] $ do
         toHtml . fromJust $ err

    div_ [class_ "form-group"] $ do
      label_ [for_ "start"] "Start"
      input_
        [ class_  "form-input"
        , type_ "datetime-local"
        , id_ "start"
        , name_ "start"
        , required_ ""
        ]

    div_ [class_ "form-group"] $ do
      label_ [for_ "end"] "End"
      input_
        [ class_  "form-input"
        , type_ "datetime-local"
        , id_ "end"
        , name_ "end"
        , required_ ""
        ]

    div_ [class_ "form-group"] $ do
      label_ [for_ "transiting"] "Transiting"
      select_ [id_ "transiting", name_ "transiting", class_ "form-select", multiple_ "", required_ ""] $ do
        planetOptions

    div_ [class_ "form-group"] $ do
      label_ [for_ "transited"] "Transited"
      select_ [id_ "transited", name_ "transited", class_ "form-select", multiple_ "", required_ ""] $ do
        planetOptions

    div_ [class_ "form-group"] $ do
      label_ [for_ "aspects"] "Aspects"
      select_ [id_ "aspects", name_ "aspects", class_ "form-select", multiple_ "", required_ ""] $ do
        forM_ majorAspects $ \Aspect{aspectName} -> do
          option_ . toHtml $ show aspectName

    div_ [class_ "form-group text-center"] $ do
      button_ [class_ "btn btn-primary btn-round btn-lg"] "Submit"

planetOptions :: Html ()
planetOptions =
  forM_ (tail defaultPlanets) $ \p ->
    option_ . toHtml $ show p
