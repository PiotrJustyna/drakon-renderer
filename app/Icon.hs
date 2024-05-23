module Icon where

import qualified GHC.Utils.Outputable

data IconType = Title | End | Action | Question

instance Show IconType where
    show Title = "Title"
    show End = "End"
    show Action = "Action"
    show Question = "Question"

data Icon = Icon { iconText :: String, iconType :: IconType }

instance GHC.Utils.Outputable.Outputable Icon where
    ppr Icon { iconText = x, iconType = y } = GHC.Utils.Outputable.text $ show y ++ ": " ++ x