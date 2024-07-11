{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson
import qualified Data.Colour.SRGB
import qualified DataTypes
import qualified Diagrams.Backend.SVG.CmdLine
import qualified Diagrams.Prelude
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr
import qualified Records
import qualified Renderer
import qualified System.IO

backgroundColour ::
  Diagrams.Prelude.Colour Double
backgroundColour = Data.Colour.SRGB.sRGB (230.0/255.0) (232.0/255.0) (216.0/255.0)

main :: IO ()
main = do
  let titleIcon = Records.Icon { Records.iconKey = 1, Records.iconText = "hello world process", Records.iconType = DataTypes.Title }

  let actionIcon = Records.Icon { Records.iconKey = 2, Records.iconText = "Hello, world!", Records.iconType = DataTypes.Action }

  let endIcon = Records.Icon { Records.iconKey = 3, Records.iconText = "end", Records.iconType = DataTypes.End }

  let serializedIcons = Data.Aeson.encode [ titleIcon, actionIcon, endIcon ]

  print serializedIcons

  let possiblyIcons = Data.Aeson.decode "[{\"iconText\":\"hello world process\",\"iconType\":\"Title\",\"iconKey\":1},{\"iconText\":\"Hello, world!\",\"iconType\":\"Action\",\"iconKey\":2},{\"iconText\":\"end\",\"iconType\":\"End\",\"iconKey\":3}]" :: Maybe [Records.Icon]
  print possiblyIcons

  case possiblyIcons of
    Just icons -> do
      let graph = Renderer.graph' icons
      GHC.Utils.Outputable.printSDocLn GHC.Utils.Outputable.defaultSDocContext GHC.Utils.Ppr.LeftMode System.IO.stdout $ GHC.Utils.Outputable.ppr graph
      Diagrams.Backend.SVG.CmdLine.mainWith $
        Diagrams.Prelude.position (Renderer.visualGraph graph)
        Diagrams.Prelude.#
        Diagrams.Prelude.bg backgroundColour
        Diagrams.Prelude.#
        Diagrams.Prelude.lw Diagrams.Prelude.none
    Nothing -> return ()