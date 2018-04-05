module Data.Template (
    render
    , renderWithContext
    , RenderContext(..)
    , newContext
    , (+=), (?+=)
    , generateExp
    , currentModules
    , renderRuntime
    , renderDynamic
    , renderDynamicWithContext
) where

import Data.Template.Model
import Data.Template.TH
import Data.Template.Dynamic