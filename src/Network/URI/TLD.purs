module Network.URI.TLD (parseTLD) where

import Control.Monad.Eff (Eff())
import Data.Foldable (foldl)
import qualified Data.Lazy as Lazy
import Data.Maybe
import qualified Data.String as String
import Data.Tuple
import Data.Tuple.Nested
import qualified Data.Set as Set
import DOM (DOM())
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.Document as DOM
import qualified DOM.Node.Element as Elem
import DOM.Node.Types (Element())
import Prelude

import Network.URI.TLD.Internal

-- http://hackage.haskell.org/package/text-1.2.2.0/docs/Data-Text.html#v:break
break :: (Char -> Boolean) -> String -> (Tuple String String)
break f s = 
	-- Can't write this?
	-- let Tuple (Tuple pre post) _ = foldl helper (tuple3 [] [] True) $ String.toCharArray s in
	-- Tuple (String.fromCharArray pre) (String.fromCharArray post)
	case foldl helper (tuple3 [] [] true) $ String.toCharArray s of
		Tuple (Tuple pre post) _ -> 
			Tuple (String.fromCharArray pre) (String.fromCharArray post)

	where
		helper (Tuple (Tuple pre post) false) c = tuple3 pre (post <> [c]) false
		helper (Tuple (Tuple pre post) true) c = 
			if f c then
				tuple3 (pre <> [c]) post true
			else
				tuple3 pre (post <> [c]) false
				

-- | Parse a domain into its subdomain, domain, and top level domain.
-- parseTLD :: forall e . String -> Eff (dom :: DOM | e) (Maybe (Tuple3 String String String))
parseTLD :: forall e . String -> Eff (dom :: DOM | e) (Maybe {subdomain :: String, domain :: String, tld :: String})
parseTLD url = do
	-- let domain = extractDomainName url in
	domain <- parseURI url
	return $ helper "" "" $ String.toLower domain

	where
		tlds = Lazy.force tldSet

		helper _ _ "" = Nothing
		helper subdomain domain tld = 
			if Set.member tld tlds then
				Just $ {subdomain : subdomain, domain : domain, tld : tld}
			else
				let subdomain' = 
					if String.null subdomain then
						domain
					else
						subdomain <> "." <> domain
				in
				case break (== '.') tld of -- Note: change this if syntax is upgraded.
					(Tuple domain' tld') ->
						if String.null domain' then
							Nothing
						else
							case String.uncons tld' of
								Just {head:'.', tail: tld''} -> 
									helper subdomain' domain' tld''
								_ ->
									Nothing

		parseURI url = do
			document <- DOM.htmlDocumentToDocument <$> (DOM.window >>= DOM.document)
			a <- DOM.createElement "a" document
			Elem.setAttribute "href" url a
			hostname a
			-- How do I call hostname?

foreign import hostname :: forall e . Element -> Eff (dom :: DOM | e) String 

-- foreign import extractDomainName :: String -> String
