{-# LANGUAGE QuasiQuotes #-}
module DB.PublishStmt where

import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


selectPublishSourceStmt :: Statement Int64 (Maybe (UUID, Text))
selectPublishSourceStmt =
  [TH.maybeStatement|
    select
      a.eid::uuid,
      coalesce(
        n.title,
        'Narravid production ' || rj.uid::text
      )::text
    from prod.render_job rj
    join prod.narration n on n.uid = rj.narration_fk
    join asset a on a.uid = rj.final_asset_fk
    where rj.uid = $1::int8
      and rj.status = 'completed'
    limit 1
  |]