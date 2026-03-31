{-# LANGUAGE QuasiQuotes #-}

module DB.Opers where

import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)

import Hasql.Session (Session, statement)
import qualified Hasql.TH as TH

{-
**** Eg:
-- in general logic:
  rezB <- use dbPool $ Op.fetchTaxoByLabel (taxoLabel, nodeLabel)
  case rezB of
    Left err -> ... (show err)
    Right taxoOut -> ...

-- id, ownerid
type TaxoOut = (Int32, Int32)
fetchTaxoByLabel :: (Text, Text) -> Session (Maybe TaxoOut)
fetchTaxoByLabel params =
  statement params [TH.maybeStatement|
    select
      a.id::int4, b.id::int4
    from taxonomy a
      join owners b on a.ownerid = b.id
    where a.label = $2::text
          and b.internalname = $1::text
  |]
-}

