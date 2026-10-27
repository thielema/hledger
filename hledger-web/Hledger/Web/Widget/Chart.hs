-- | The report pages' charts: hledger's SVG charts, as the commands'
-- --chart draws them (see "Hledger.Cli.Chart"), drawn by the server and
-- styled by /chart.css (ChartCssR), since the Content-Security-Policy
-- allows no inline styles.

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Web.Widget.Chart (
  chartWidth,
  reportChart,
) where

import Data.Text qualified as T
import Safe (readMay)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

import Hledger.Write.Chart (Chart(..), setChartLayout)
import Hledger.Write.Chart.Svg (SvgOptions(..), chartSvgMarkupWith, defaultSvgOptions)
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
-- label. Nothing when the chart has nothing to show. The c key hides it,
-- and the d key the table (hledger.js, hledger.css).
reportChart :: Text -> Chart -> Widget
reportChart label chart = unless (null $ chPanels chart) $ do
  w <- liftHandler chartWidth
  addStylesheet ChartCssR
  toWidget $ H.div ! A.class_ "report-chart" $
    chartSvgMarkupWith defaultSvgOptions{svgStyles = False, svgWidth = w, svgPlotHeight = 200}
      (setChartLayout Nothing chart{chTitle = ""})
      ! H.customAttribute "aria-label" (H.toValue label)
