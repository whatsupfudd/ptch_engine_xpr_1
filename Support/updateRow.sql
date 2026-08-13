WITH
  updated AS (
  UPDATE prod.render_node n
    SET status = 'done', asset_fk = 458 , asset_eid = '2022ad5b-01ab-4421-82a7-cd064da0e2ff' , lease_owner = NULL, lease_expires_at = NULL, completed_at = now(), error_text = NULL, updated_at = now()
    WHERE n.uid = 2150  AND n.lease_owner = 'owner_1'  AND n.status IN ('leased', 'running') AND n.lease_expires_at IS NOT NULL AND n.lease_expires_at >= now() RETURNING n.render_job_fk, n.derive_key, n.lane, n.exec, n.artifact_kind
  )
  , meta AS (SELECT rj.narration_fk, u.derive_key, u.lane, u.exec, u.artifact_kind FROM updated u JOIN prod.render_job rj ON rj.uid = u.render_job_fk)
  , upsert_artifact AS (
    INSERT INTO prod.render_artifact
      (narration_fk, derive_key, lane, exec, artifact_kind, status, asset_fk, asset_eid, request_eid, notes)
    SELECT
      m.narration_fk, m.derive_key, m.lane, m.exec, m.artifact_kind, 'done', 458 , '2022ad5b-01ab-4421-82a7-cd064da0e2ff' , null , null
    FROM meta m
    ON CONFLICT (narration_fk, md5(derive_key))
    DO UPDATE
      SET
        lane = excluded.lane, exec = excluded.exec, artifact_kind = excluded.artifact_kind, status = 'done', asset_fk = excluded.asset_fk, asset_eid = excluded.asset_eid, request_eid = excluded.request_eid, notes = excluded.notes, updated_at = now()
    RETURNING 1
  )
SELECT EXISTS (SELECT 1 FROM updated);


1: "2150"
2: ,"\"owner_1\""
3: ,"458"
4: ,"2022ad5b-01ab-4421-82a7-cd064da0e2ff"
5: ,"null"
6: ,"null"