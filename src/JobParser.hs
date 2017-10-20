{-|
Module      : JobParser

Extract an XML-formatted JobSpec.
-}
module JobParser
    ( JobSpec(JobSpec, jobName, jobDuration)
    , parse
    ) where


import           Preamble

import qualified Text.XML.Light       as XML
import qualified Text.XML.Light.Lexer as Lexer


-- | A simple specification for a "job." This is really standing in for
-- something more interesting.
data JobSpec = JobSpec
    { jobName     :: String -- ^ Description of job (will be in log output)
    , jobDuration :: Int    -- ^ Duration of job in seconds
    } deriving (Show)


-- | We convert from 'XmlSource' which has instances for various
-- text-ual type things: 'String', 'ByteString', 'Text', etc. If parsing
-- succeeds we'll end up with a 'JobSpec'.
parse :: Lexer.XmlSource s => s -> Maybe JobSpec
parse s = XML.parseXMLDoc s >>= JobParser.get


-- | Extract the "jobName" and "seconds" fields out of the XML document.
get :: XML.Element -> Maybe JobSpec
get el = liftA2 JobSpec
    (XML.strContent <$> XML.filterChildName (byName "jobName") el)
    (read . XML.strContent <$> XML.filterChildName (byName "seconds") el)


-- | A helper function that will search by the name of the tag (rather
-- than attributes or other XML-y things).
byName :: String -> XML.QName -> Bool
byName needle haystack = XML.qName haystack == needle
