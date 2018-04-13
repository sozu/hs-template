module Data.Template (
    render
    , renderWithContext
    , RenderContext(..)
    , newContext
    , (+=), (?+=)
    , Repr(..)
    , generateExp
    , currentModules
    , importedModules
    , renderRuntime
    , renderDynamic
    , renderDynamicWithContext
) where

import Data.Template.Model
import Data.Template.TH
import Data.Template.Dynamic