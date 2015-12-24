module Test.Main where

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Trans
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Data.Tuple.Nested
import DOM (DOM())
import Prelude
import Test.Unit

import Network.URI.TLD

main = runTest do
	makeTest "http://localhost:3000/sdfsdf" "" "" "localhost"
	--makeTest "https://localhost:3000/sdfsdf" "" "" "localhost"


	where 
		makeTest input subdomain domain tld  = do
			parsedM <- lift $ lift $ parseTLD input
 			test input $ case parsedM of
 				Just (Tuple (Tuple subdomain' domain') tld') -> do
 					assert "subdomain" $ subdomain' == subdomain
 					assert "domain" $ domain' == domain
 					assert "tld" $ tld' == tld
 				Nothing -> do
 					assert "parseTLD returned Nothing" false
 

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
-- 	tests <- traverse makeTest [
-- 			  tuple4 "http://localhost:3000/sdfsdf" "" "" "localhost"
-- 			, tuple4 "https://localhost:3000/sdfsdf" "" "" "localhost"
-- 			, tuple4 "https://google.com" "" "google" "com"
-- 			, tuple4 "http://www.theguardian.co.uk/world" "www" "theguardian" "co.uk"
-- 			]
-- 	runTest tests
-- 
-- 	where
-- 		makeTest :: forall e . Tuple4 String String String String -> Eff (dom :: DOM | e) Test
-- 		makeTest (Tuple (Tuple (Tuple input subdomain) domain) tld) = do
-- 			tld <- parseTLD input
-- 			return $ test input $ case tld of
-- 				Just (Tuple (Tuple subdomain' domain') tld') -> do
-- 					assert "subdomain" $ subdomain' == subdomain
-- 					assert "domain" $ domain' == domain
-- 					assert "tld" $ tld' == tld
-- 				Nothing -> do
-- 					assert "parseTLD returned Nothing" false
-- 
