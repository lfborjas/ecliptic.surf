module EclipticSurf.Views.Helpers where
    
import Data.Time
import Data.Text (Text)
import Lucid
import CMark (commonmarkToHtml)

dateTimeShow :: UTCTime -> String
dateTimeShow = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %p (%Z)"

markdownToHtml :: Text -> Html ()
markdownToHtml = toHtmlRaw . commonmarkToHtml []
