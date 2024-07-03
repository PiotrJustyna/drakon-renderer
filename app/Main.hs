{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson
import qualified Icon
import qualified Renderer

import qualified Data.Colour.SRGB
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr
import qualified Diagrams.Backend.SVG.CmdLine
import qualified Diagrams.Prelude
import qualified System.IO

backgroundColour ::
  Diagrams.Prelude.Colour Double
backgroundColour = Data.Colour.SRGB.sRGB (230.0/255.0) (232.0/255.0) (216.0/255.0)

-- main :: IO ()
main = do
  let titleIcon = Icon.Icon { Icon.iconText = "hello world process", Icon.iconType = Icon.Title }

  let actionIcon = Icon.Icon { Icon.iconText = "Hello, world!", Icon.iconType = Icon.Action }

  let endIcon = Icon.Icon { Icon.iconText = "end", Icon.iconType = Icon.End }

  let serializedIcons = Data.Aeson.encode [ titleIcon, actionIcon, endIcon ]

  print serializedIcons

  let possiblyIcons = Data.Aeson.decode "[{\"iconText\":\"hello world process\",\"iconType\":\"Title\"},{\"iconText\":\"Hello, world!\",\"iconType\":\"Action\"},{\"iconText\":\"end\",\"iconType\":\"End\"}]" :: Maybe [Icon.Icon]
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