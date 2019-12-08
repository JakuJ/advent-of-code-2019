{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Days (runDays) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

runDay :: Int -> Q Exp
runDay n = [| $(varE (mkName ("day" ++ show n))) |]

runDays :: Int -> Q Exp
runDays 0 = [| return () |]
runDays n = [| $(runDays (n - 1)) >> $(runDay n) |]
