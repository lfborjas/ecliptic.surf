module EclipticSurf.Views.SurfCharts where

import Lucid
import Control.Monad (when, forM_, unless)
import Data.Maybe (isJust, fromJust)
import Almanac.Extras
import SwissEphemeris (Planet)
import Data.Text (Text, toTitle)
import Data.Time
import Almanac
import Data.List (intersperse)
import Data.Time.Format.ISO8601 (iso8601Show)

mundanePage 
  :: UTCTime 
  -> UTCTime
  -> [Planet]
  -> [Planet]
  -> [AspectName]
  -> [(Transit Planet, [UTCTime])]-> Html () -> Html ()
mundanePage start end _transitingC _transitedC _chosenAspects transits chart = do
  main_ $ do
    p_ [class_  "mt-2"] $ do
      "Mundane transits between "
      toHtml . iso8601Show $ start
      " and "
      toHtml . iso8601Show $ end
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


mundaneForm :: Maybe Text -> Html ()
mundaneForm err = do
  h2_ "Explore mundane transits"
  p_ "Dates are UTC. The Moon is not included due to its high speed."
  form_ [action_ "/mundane", method_ "get"] $ do
    errorDisplay err
    startEnd
    multiSelects
    submit


natalForm :: Maybe Text -> Html ()
natalForm err = do
  h2_ "Explore natal transits"
  p_ "Start and end dates are UTC. The Moon is not included due to its high speed."
  form_ [action_ "/natal", method_ "get"] $ do
    errorDisplay err
    startEnd
    dateInput (Just "Date of Birth") "dob"
    div_ [class_ "form-group"] $ do
      label_ [for_ "offset"] "Timezone"
      input_
        [ class_ "form-input"
        , type_ "text"
        , id_ "offset"
        , name_ "offset"
        , required_ ""
        , value_ "+00:00"
        , placeholder_ "+00:00"
        , pattern_ "^[+-][0-9]{2}:[0-9]{2}$"
        ]
    multiSelects
    submit


errorDisplay :: Maybe Text -> Html ()
errorDisplay err = do
  when (isJust err) $ do
      div_ [class_ "bg-error p-2"] $ do
         toHtml . fromJust $ err

startEnd :: Html ()
startEnd = do
  dateInput' "start"
  dateInput' "end"

dateInput :: Maybe Text -> Text -> Html ()
dateInput label inputName = do
  div_ [class_ "form-group"] $ do
    label_ [for_ inputName] $ 
      maybe (toHtml . toTitle $ inputName) toHtml label
    input_
      [ class_  "form-input"
      , type_ "datetime-local"
      , id_ inputName
      , name_ inputName
      , required_ ""
      ]

dateInput' :: Text -> Html ()
dateInput' = dateInput Nothing

multiSelects :: Html ()
multiSelects = do
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

submit :: Html ()
submit = do
  div_ [class_ "form-group text-center"] $ do
    button_ [class_ "btn btn-primary btn-round btn-lg"] "Submit"

planetOptions :: Html ()
planetOptions =
  forM_ (tail defaultPlanets) $ \p ->
    option_ . toHtml $ show p
