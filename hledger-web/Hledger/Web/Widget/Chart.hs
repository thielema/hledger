-- | The report pages' charts: hledger's SVG charts, as the commands'
-- --chart draws them (see "Hledger.Cli.Chart"), drawn by the server and
-- styled by /chart.css (ChartCssR), since the Content-Security-Policy
-- allows no inline styles.

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Web.Widget.Chart (
  chartWidth,
  reportChart,
  chartLegend,
) where

import Data.Text qualified as T
import Safe (readMay)
import Text.Blaze ((!), (!?))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

import Hledger.Data.Types (Amount(..))
import Hledger.Utils.I18n (Translations, tr)
import Hledger.Write.Chart (Chart(..), Panel(..), setChartLayout)
import Hledger.Write.Chart.Svg (SvgOptions(..), chartPanelsSvgMarkupWith, chartSvgMarkupWith, defaultSvgOptions)
import Hledger.Web.Import

-- | How wide to draw a chart: as wide as the page's main column, which
-- the browser measures and keeps in the chartwidth cookie (hledger.js),
-- from 320 to 2400 pixels; or by default, 880.
chartWidth :: Handler Double
chartWidth = maybe (svgWidth defaultSvgOptions) (fromIntegral . max 320 . min (2400 :: Int)) . (>>= readMay . T.unpack)
  <$> uiCookie "chartwidth"

-- | A report's chart, to go between its heading and its table: as wide
-- as the page's main column, in the layout its report prefers, untitled,
-- since the heading names it, and named for screen readers by the given
-- label. Nothing when the chart has nothing to show. With several
-- commodities, a legend chooses the one charted, as on the register
-- (see 'chartLegend'). The c key hides it, and the d key the table
-- (hledger.js, hledger.css).
reportChart :: Translations -> Text -> Chart -> Widget
reportChart trs label chart = unless (null panels) $ do
  w <- liftHandler chartWidth
  addStylesheet ChartCssR
  let opts = defaultSvgOptions{svgStyles = False, svgWidth = w, svgPlotHeight = 200}
  toWidget $ case panels of
    [_] -> H.div ! A.class_ "report-chart" $
             chartSvgMarkupWith opts chart' ! H.customAttribute "aria-label" (H.toValue label)
    _   -> H.div ! A.class_ "report-chart with-legend" $
             chartLegend "report-chart" legendlabel
               [ (name, svg ! H.customAttribute "aria-label" (H.toValue $ label <> ": " <> name))
               | (p, svg) <- zip panels $ chartPanelsSvgMarkupWith opts chart'
               , let c = acommodity $ pnUnit p
               , let name = if T.null c then nosymbol else c
               ]
  where
    chart' = setChartLayout Nothing chart{chTitle = ""}
    panels = chPanels chart'
    -- TRANSLATORS: in a report page's chart, before the commodities that can be charted.
    legendlabel = tr trs "Commodity"
    -- TRANSLATORS: a chart legend's entry for amounts written without a commodity symbol.
    nosymbol = tr trs "no symbol"

-- | A legend choosing which of several charts is shown, with no javascript:
-- its name, then for each chart, a radio button (named and numbered with
-- the given prefix, which should be unique on the page), its label, and the
-- chart, which /chart.css shows while its button is checked. The first is
-- checked at first. It goes in an element of class with-legend.
chartLegend :: Text -> Text -> [(Text, Html)] -> Html
chartLegend prefix legendlabel charts = do
  H.span ! A.class_ "chart-legend-label" $ H.toHtml legendlabel <> ":"
  forM_ (zip [0 :: Int ..] charts) $ \(k, (name, chart)) -> do
    let i = H.toValue $ prefix <> "-" <> T.pack (show k)
    H.input ! A.type_ "radio" ! A.name (H.toValue prefix) ! A.id i !? (k == 0, A.checked "")
    H.label ! A.for i $ H.toHtml name
    H.div ! A.class_ "chart-panel" $ chart
