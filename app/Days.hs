{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Days (runDays) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

runDay :: Int -> Q Exp
runDay n = [| putStrLn title >> $(varE (mkName function)) |]
    where
        title = "Day " ++ num ++ ":"
        function = "day" ++ num
        num = show n

runDays :: Int -> Q Exp
runDays 0 = [| return () |]
runDays n = [| $(runDays (n - 1)) >> $(runDay n) |]
